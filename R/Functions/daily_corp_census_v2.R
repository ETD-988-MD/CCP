# Daily corporation census -- v2

# Provides a summary of character-level activity for all accounts associated with a specific character

corp_census <- function(date,corpID,t.thres=1,print.report=T,conn){
  require(lubridate)
  require(RODBC)
  require(data.table)
  date = as.Date(date) 
  snapshot = data.table(sqlQuery(conn,
                                 paste0("SELECT 
                                          analysisDate ='",date,"',
                                          T.userID,
                                          E.userName,
                                          T.characterID,
                                          T.corporationID,
                                          T.allianceID,
                                          E.countryName,
                                          E.createDate,
                                          days_since_create =0,
                                          E.dob,
                                          age= 0,
                                          E.gender,
                                          E.languageName,
                                          logCnt = T.logonCount,
                                          charSessions = T.characterSessions,
                                          logMin = T.logonMinutes,
                                          charMin = T.characterMinutes,
                                          propTime_playedChar = 0,
                                          totalwarps = T.travelWarpsTotal,
                                          totalSG = T.travelJumpsStargateTotal,
                                          SGHigh = T.travelJumpsStargateHighSec,
                                          SGLow = T.travelJumpsStargateLowSec,
                                          SGNull = T.travelJumpsStargateNullSec,
                                          T.iskIn,T.iskOut,T.iskSnapshot,T.marketIskIn,T.marketIskOut,
                                          totalOre = T.miningOreTotal,
                                          combat = T.combatDamageToPlayersTotalAmount,
                                          joinFleets = T.socialFleetJoins,
                                          ipCN = E.ipAddressCountryName,
                                          E.userTypeText,
                                          E.timeZoneName,
                                          E.timeZoneID,
                                          E.statusText
                                        FROM ebs_FACTORY.eve.characterHistory T
                                        INNER JOIN ebs_RESEARCH.zuser.usersEx E ON (E.userID = T.userID)
                                        WHERE corporationID = ",
                                        corpID," AND ","historyDate = '",date,"'"),stringsAsFactors =F))
  setkey(snapshot)
  snapshot$dob = as.Date(snapshot$dob)
  snapshot$createDate = as.Date(snapshot$createDate)
  snapshot$age = as.numeric(round((Sys.Date() - snapshot$dob)/365))
  snapshot$days_since_create = as.numeric(round((Sys.Date() - snapshot$createDate)))
  snapshot$gender = ifelse(snapshot$gender==1,"male","female")
  snapshot$propTime_playedChar = round(snapshot$charMin/snapshot$logMin,2)
  
  # Corporate level conditions
  ctax = data.table(sqlQuery(conn, # corp tax rate
                             paste0("SELECT 
                              corporationID,
                              taxRate,
                              dateFrom,
                              dateTo
                              FROM ebs_WAREHOUSE.owner.dimCorporationSCD
                              WHERE corporationID = ",corpID),stringsAsFactors =F))
  if(nrow(ctax)>1){
    ctax$dateFrom = as.Date(ctax$dateFrom)
    loc = abs(ctax$dateFrom - date) == min(abs(ctax$dateFrom - date))
    tax.rate = ctax[loc]$taxRate
  } else{tax.rate =  ctax$taxRate}
  snapshot$taxRate = tax.rate
  
  # Location -- who is playing together in a single day?
  char.list = unique(snapshot$characterID)
  # user.list = paste0("'",paste0(user.list,collapse="','"),"'")
  char.list = paste0(char.list,collapse=",")
  location = data.table(sqlQuery(conn,paste0("SELECT
                            O.eventDate,
                            characterID=O.ownerID,
                            locationID=O.referenceID,
                            ssname = S.solarSystemName,
                            S.security
                            FROM ebs_RESEARCH.zevent.ownerEventsEx O
                            INNER JOIN datawarehouse.dbo.dimSolarSystems S ON (S.solarSystemID = O.referenceID)
                            WHERE eventTypeID = 6 AND eventDate BETWEEN '",date,"' and '",date+1,"' and ownerID IN (",char.list,")"),stringsAsFactors =F))
  
  location$eventDate = ymd_hms(format.Date(location$eventDate,"%Y-%m-%d %H:%M:%S"))
  setkey(location)

  # Identifying individual players in the same space.
  same = location[,unique(characterID),by=locationID]
  same$tt = 1
  same = same[,sum(tt)>=2,by=locationID]
  same = same[V1==T]$locationID
  
  # Generate temporal proximity matrix for those in the "same space".
  master.dyads = c()
  for(j in 1:length(same)){
    sub = location[locationID == same[j]]
    time = ymd_hms(format.Date(sub$eventDate,"%Y-%m-%d %H:%M:%S"))
    tMat=c()
    for(i in 1:length(time)){
      tMat = rbind(tMat,as.numeric(time[i]-time))
    }
    boolTProx = abs((tMat/60)/60)<= t.thres # Arbitrary time threshold in hours
    diag(boolTProx) = F # set Diagonals to false 
    # Generating Dyadic Pairs
    dyads = which(boolTProx==T,arr.ind=TRUE); dyads2 = dyads
    dyads[,1] = sub$characterID[dyads[,1]];dyads[,2] = sub$characterID[dyads[,2]]
    # Record Necessary information....
    dyads = cbind(dyads,sub$locationID[dyads2[,1]])
    dyads = unique(dyads)
    dups = dyads[,1] == dyads[,2]
    dyads = dyads[!dups,]
    # Output
    master.dyads = rbind(master.dyads,dyads)
  }
  colnames(master.dyads) = c("char_A","char_B","locationID")
  master.dyads = data.table(master.dyads);setkey(master.dyads)
  dyad.out = merge(master.dyads,unique(location[,list(locationID,ssname,security)]),by="locationID")
  dyad.out$analysisDate = date
  # Map on character specific information
  for(tt in c("char_A","char_B")){
    colnames(dyad.out)[colnames(dyad.out)==tt] = "characterID"
    temp = snapshot[,list(characterID,countryName,age,gender,days_since_create,logMin,timeZoneID)]
    colnames(temp)[-1] = paste0(tt,"_",colnames(temp)[-1])
    dyad.out = merge(dyad.out,temp,by="characterID")
    colnames(dyad.out)[colnames(dyad.out)=="characterID"] = tt
  }
  dyad.out$corporationID = corpID
  setcolorder(dyad.out,c("analysisDate","corporationID","char_A","char_B","locationID","ssname","security",
                         "char_A_countryName","char_B_countryName","char_A_age","char_B_age",
                         "char_A_gender","char_B_gender",
                         "char_A_days_since_create","char_B_days_since_create",
                         "char_A_logMin","char_B_logMin",
                         "char_A_timeZoneID","char_B_timeZoneID"))

  # Generate Coporation Summary Table (built from the players up)
  warps = sum(snapshot$totalwarps,na.rm=T)
  gates = sum(snapshot$totalSG,na.rm=T)
  ave.age = round(mean(snapshot$age,na.rm=T),1)
  ave.exp = round(mean(snapshot$days_since_create,na.rm=T)/365,2)
  combat = sum(snapshot$combat,na.rm=T)
  iskMade = round(sum(snapshot$iskIn + snapshot$iskOut,na.rm=T)/1000000,2)
  total.mins.played = sum(snapshot$charMin,na.rm=T)
  ave.hours.played = round(mean(snapshot$charMin,na.rm=T)/60,2)
  no.countries = length(unique(snapshot$countryName))
  countries = paste0(unique(snapshot$countryName),collapse=", ") 
  total.characters = length(unique(snapshot$characterID))
  total.users = length(unique(snapshot$userID))
  no.players.interact = length(unique(c(dyad.out$char_A,dyad.out$char_B)))
  no.interactions = nrow(dyad.out[,list(char_A,char_B)])/2
  prop.online.interact = paste0(round(no.players.interact/total.characters,2)*100,"%")
  
  
  # Print Summary Report
  if(print.report==T){
    cat(paste0("\n__________________________________",
               "\nCorporation: ", corpID,"\nActivity Summary: ",format.Date(date,"%b %d %Y"),
               "\n__________________________________",
               "\n\t - ave age of players: ",ave.age,
               "\n\t - ave experience of players (in years): ",ave.exp,
               "\n\t - total # of countries played from: ",no.countries,
               "\n\t - countries played from: ",countries,
               "\n\t - cumulative character minutes played: ",total.mins.played,
               "\n\t - average time played (in hours): ",ave.hours.played,
               "\n\t\t--------------------------",
               "\n\t - total # of characters online: ",total.characters,
               "\n\t - total # of users online: ",total.users,
               "\n\t - total # of players that interacted: ",no.players.interact,
               "\n\t - total # of interactions (players in the same SS w/in ",t.thres," hour(s)): ",no.interactions,
               "\n\t - proporation of characters that interacted: ",prop.online.interact,
               "\n\t\t--------------------------",
               "\n\t - cumulative ISK made by players (in millions): ",iskMade,
               "\n\t - total stargates used: ",gates,
               "\n\t - total warps: ",warps,
               "\n\t - total # of conflicts: ",combat
               ))
  }
  summary.table = data.table(corpID,date,ave.age,ave.exp,no.countries,countries,
                             total.characters,total.users,no.players.interact,no.interactions,
                             prop.online.interact,total.mins.played,ave.hours.played,
                             iskMade,warps,gates,combat)

  
  # List output
  allout = list()
  allout$aggregate_summary = summary.table
  allout$day_snapshot = snapshot
  allout$player_interactions = dyad.out
  class(allout) <- c("list","corp_census")
  return(allout)
}


# Test
# conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;')
# day_data = corp_census("2016-04-07",98188326,t.thres = 1,print.report = T,conn)

