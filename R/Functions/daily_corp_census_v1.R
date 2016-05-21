# Daily corporation census -- v.1
corp_census <- function(date,corpID,conn){
  date = as.Date(date) 
  rel.chars = data.table(sqlQuery(conn,paste0("SELECT * FROM ebs_FACTORY.eve.characterHistory WHERE corporationID = ",
                                              corpID," AND ","historyDate = '",date,"'")))
  setkey(rel.chars)
  rel.chars2 = rel.chars[,list(
    ##### Snapshot of character
    # Player Association
    corporationID,characterID,
    userID,customerID,allianceID,
    # Player activity
    logCnt = logonCount,logMin = logonMinutes,paying = isPaying,
    charMin = characterMinutes,charSessions = characterSessions,
    # Travel
    totalwarps = travelWarpsTotal,totalSG = travelJumpsStargateTotal,SGHigh = travelJumpsStargateHighSec,
    SGLow = travelJumpsStargateLowSec,SGNull = travelJumpsStargateNullSec,
    # Finaces 
    iskIn,iskOut,iskSnapshot,marketIskIn,marketIskOut,
    totalOre = miningOreTotal,
    # Combat
    combat = combatDamageToPlayersTotalAmount,
    joinFleets = socialFleetJoins)]
  
  # Draw Out User Information
  user.list = unique(rel.chars$userID)
  user.list = paste0("'",paste0(user.list,collapse="','"),"'")
  
  rel.users = data.table(sqlQuery(conn,paste0("SELECT *
                                              FROM ebs_RESEARCH.zuser.usersEx
                                              WHERE userID IN (",user.list,")")))
  setkey(rel.users)
  rel.users2 = rel.users[,list(userID,userName,
                               createDate=as.Date(createDate),
                               dob=as.Date(dob),age=0,
                               gender=ifelse(gender==1,"male","female"),
                               countryName,
                               languageName,
                               ipCN = ipAddressCountryName,
                               userTypeText,timeZoneName,
                               statusText)]
  rel.users2$age = as.numeric(round((Sys.Date() - rel.users2$dob)/365))
  rel.users2$createdDays = as.numeric(round((Sys.Date() - rel.users2$createDate)))
  
  # Location information
  
  # Merge
  out1 = merge(rel.users2,rel.chars2,by="userID")
  out1$analysisDate = date
  cols = colnames(out1)
  ord = c("analysisDate","userID","userName","characterID","corporationID","allianceID",
          "countryName","createdDays","age","gender","languageName")
  
  setcolorder(out1,c(ord,cols[!cols %in% ord]))
  
  
  full.out = list()
  full.out$corpSnapshot = out1
  return(out1)
}