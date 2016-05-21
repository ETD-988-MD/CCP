setwd("~/ETD")
rm(list=ls());
gc(reset=TRUE)
library(RODBC);
library(ggplot2);
library(lubridate);
library(plyr);
library(scales);
library(data.table)
library(caTools)
library(dplyr)
library(lfe)
require(igraph)
require(dplyr)



# EXPLORATION --------
# Purpose: Get intuition of DW code; locate corporations

# *****************************************
# Active corporations ------ as of 5.9.2016 --------------
# *****************************************
conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;');
corps <- as.data.frame(sqlQuery(conn, "SELECT * FROM ebs_FACTORY.eve.activeCorporations"))
plot(density(corps$members))
summary(corps$members)
hist(corps$members[corps$members<200],breaks=50)
hist(corps$members[corps$members>200],breaks=50)
# Most corporations seem to never get off the group


M = corps$members
member.cat = ifelse(M <=50,1,
                    ifelse(M>=51 & M <= 100,2,
                           ifelse(M>=101&M<=500,3,4)))
hist(member.cat)
table(member.cat)
#     1     2     3     4     5 
# 82520  7977  1754   759   948 

# Further break down of the <50
member.cat2 = ifelse(M <=5,1,
                     ifelse(M>=6 & M <= 15,2,
                            ifelse(M>=16&M<=30,3,
                                   ifelse(M>=31&M<=50,4,5))))
table(member.cat2[member.cat2!=5])
# 1     2     3     4 
# 82520  7977  1754   759

# Selecting a corporation with more than 100 memebers
C = corps[corps$members>=100,]
set.seed(123);C[sample(nrow(C),size = 1,replace=F)]
# CorpID: 98188326    Members: 110

# Let's isolate the name of this corp.
sqlQuery(conn, "           
         SELECT * FROM ebs_RESEARCH.corporation.corporationsLookup WHERE corporationID = 98188326
         ")
# 98188326 == Space Mechanics
# "YKE4-3 III - Moon 1 - Guardian Angels Testing Facilities"

# *****************************************************
# Inside a Corporation: 98188326 - the "Space Mechanics ---------
# *****************************************************
conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_RESEARCH;Trusted Connection=true;Integrated Security=true;');
roster <- data.table(sqlQuery(conn,"SELECT * FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE corporationID = 98188326"))

# Roles
roles <- sqlQuery(conn,"SELECT * FROM ebs_RESEARCH.dbo.crpRoleHistory WHERE corporationID = 98188326 ORDER BY changeTime ASC")
roles.desc <- sqlQuery(conn,"SELECT * FROM ebs_RESEARCH.corporation.roles")

# To do -- map the role id onto the role positions to see who plays what role and when. 
as.character(roles[,6])%in% as.character(roles.desc[,2])


# Messages -- can we see who is sending messages to who (to track interaction)?
list = unique(roster$characterID)
dtmail <- data.table(
  sqlQuery(conn, paste0(paste(c("SELECT
                                sCID = senderID,
                                rCID = toCharacterIDs
                                FROM ebs_RESEARCH.mail.messages
                                WHERE senderID IN (90034497",list[-1*c(1,length(list))]),collapse=","),",1990366912)"),stringsAsFactors=FALSE))

# Subset by only senders that are in the same corp
dtmail = dtmail[dtmail$rCID %in% list,]
# remove messages where the sender and receiver are the same.
dtmail = dtmail[dtmail$sCID!=dtmail$rCID]

dtmail$sCID = dtmail$sCID %>% trimws(.) %>% as.character(.) %>% paste0("v_",.)
dtmail$rCID = dtmail$rCID %>% trimws(.) %>% as.character(.) %>% paste0("v_",.)
dtmail = as.matrix(dtmail)

all = c(dtmail$sCID,dtmail$rCID)
net = graph.empty(directed = F)
net = add.vertices(graph = net,length(unique(c(dtmail))),names= as.character(unique(c(dtmail))))
# net = add.vertices(graph = net,length(unique(dtmail)),names= unique(dtmail))
net = add.edges(net,edges = t(dtmail)) # issue here ... need to resolve
summary(net)


# TO Do: Make sure the date sent is not prior to the date a char was a member in a corp
roster$startDate = as.Date(roster$startDate)

# *****************************************************
# Corporate Histories/Timelines - 5-10-2016 -------------
# *****************************************************

# Read in the corporate history (first 1000 rows, most recent) of the space mechanics 
sm.hist = data.table(
  sqlQuery(conn, "           
           SELECT TOP 1000* FROM ebs_FACTORY.eve.corporationHistory  WHERE corporationID = 98188326 ORDER BY historyDate Desc
           "))
colnames(sm.hist) # Survey the option
# THis provides a comprehensive daily history of what a corporation does in the game.
as.list(sm.hist[1,])[1:100]
# Records everything from the weapons used, the ore mined, the damage taken on, 
# the security of the space where a death was counted, isk (both start-end of
# recorded interval), travel (docking, jumps, etc)

grep("tax",colnames(sm.hist)) # No tax rate information

# *****************************************************
# ROUND 2: Active Corporations (seperating NPC corps out) - 5-11-2016 ------
# *****************************************************

# NPC Corporations ----------
npc.corps <- data.table(sqlQuery(conn,"SELECT corporationID,
                                        corporationName,description
                                        FROM ebs_RESEARCH.corporation.npcCorporations"))
colnames(npc.corps)
npc.corps$corporationID %in% corps$corporationID %>% sum(.) # Are we picking up NPC Corps? yep

# Took NPC corporations into account when generating the initial membership breakdowns. 
ply.corps = corps[!corps$corporationID %in% npc.corps$corporationID,]
np.corps = corps[corps$corporationID %in% npc.corps$corporationID,]

act.corps = nrow(corps) # updated stats on the total number of active corps (useful for the proportion stats below)

summary(ply.corps$members)
summary(np.corps$members)

M = ply.corps$members
member.cat = ifelse(M <= 5,1,
                    ifelse(M>=6 & M <=50,2,
                           ifelse(M>=51 & M <= 100,3,
                                  ifelse(M>=101&M<=500,4,
                                         ifelse(M>=501&M<=1000,5,
                                                ifelse(M>=1001&M<=5000,6,7))))))
table(member.cat)

M = np.corps$members
npc.member.cat = ifelse(M <= 5,1,
                        ifelse(M>=6 & M <=50,2,
                               ifelse(M>=51 & M <= 100,3,
                                      ifelse(M>=101&M<=500,4,
                                             ifelse(M>=501&M<=1000,5,
                                                    ifelse(M>=1001&M<=5000,6,7))))))
table(npc.member.cat)

# Generate some quick tables
labels = c("<5","6-50","50-100","100-500","500-1000","1000-5000","5000>")
member.cat = as.factor(member.cat); levels(member.cat) = labels
npc.member.cat = as.factor(npc.member.cat); levels(npc.member.cat) = labels[c(1,6,7)]
val = (table(member.cat)/act.corps) %>% round(.,5)*100 # proportion of the total # of corps
val2 = (table(npc.member.cat)/act.corps) %>% round(.,5)*100
spacing = c(.75,2,3.25,4.50,5.75,7,8.25)

pdf(file="plots/corp_membership_breakdown.pdf",height=10,width=10)
par(mfrow=c(2,1))
barplot(table(member.cat),horiz = F,main="Player-Run Corp Membership",col="lightblue")
for(i in 1:length(labels)){text(y=2e4,x=spacing[i],paste0(val[i],"%")) }
barplot(table(npc.member.cat),horiz = F,main="NPC Corp Membership",col="lightgreen",
        xlab="Percentages are proportions of the total number of active corps (93,958) \n as of 5.10.2016")
for(i in 1:length(labels)){text(y=10,x=spacing[i],paste0(val2[i],"%")) }
dev.off()

# *****************************************************
# Mapping Individual Players to the same space -------
# *****************************************************
conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_RESEARCH;Trusted Connection=true;Integrated Security=true;')
# Take the kill log and map it onto the corporate event log?
# are event IDs the same? And can we leverage them to map behavior in the game space?

klog = data.table(sqlQuery(conn,"SELECT * FROM ebs_RESEARCH.dbo.logKillEvents"))
klog # Very large (~ 4 mil) - kill log
clog = data.table(sqlQuery(conn,"SELECT * FROM ebs_RESEARCH.dbo.logCorporationEvents"))
clog # corporate log

setkey(klog,eventID)
setkey(clog,eventID)
tables()

klog[eventID==25333208886]
clog[eventID==25333208886]

same = merge(klog,clog,by="eventID")
same # No matches...so these are unique eventIDs to each table.... shame. 

# So there isn't a DIRECT way to solve this solution. 

# Let's try and use character specific data to map. 
ch.hist = data.table(sqlQuery(conn,"SELECT TOP 10* FROM ebs_FACTORY.eve.characterHistory"))

    # as a useful side note. These detailed histories exist for each level of
    # agregation (user,char, corp, alliance)
    al.hist =  data.table(sqlQuery(conn,
                                   "SELECT TOP 10* FROM ebs_FACTORY.eve.allianceHistory"))
    colnames(al.hist)
    
    user.hist = data.table(sqlQuery(conn,
                                    "SELECT TOP 10* FROM ebs_FACTORY.eve.userHistory WHERE userID = 12748017"))
    setkey(user.hist)
    tables()
    colnames(user.hist)
    user.hist[3,]
    
# *****************************************************
# Unpacking the Tax Rate -------
# *****************************************************
    conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;');
    # First let's get a distribution of the standing tax rate
    taxR = data.table(sqlQuery(conn,"SELECT 
	                             corporationID,
                               memberCount,
                               taxRate,
                               logonCount,
                               logonMinutes
                               FROM  ebs_RESEARCH.dbo.crpCorporations"))
    npc.corps <- data.table(sqlQuery(conn,"SELECT corporationID,
                                        corporationName,description
                                     FROM ebs_RESEARCH.corporation.npcCorporations"))
    active <- data.table(sqlQuery(conn,"SELECT * FROM ebs_FACTORY.eve.activeCorporations"))
    
    # Subset to only include active player corporations
    taxR = taxR[!taxR$corporationID %in% npc.corps$corporationID]
    taxR = taxR[taxR$corporationID %in% active$corporationID]
    
    # Now remove all players who are self-incorporated
    players = data.table(sqlQuery(conn,"SELECT 
                                  characterID,
                                  userID,
                                  corporationID 
                                  FROM ebs_FACTORY.eve.activeCharacters
                                 "))
    
    total.corps = players$corporationID %>% unique(.)
    
    players$single.player=0
    for(i in total.corps){
      uid = unique(players[corporationID==i,userID])
      if(length(uid)==1){
        players[corporationID==i,"single.player"] <- 1
      }
    }
   
    
  xx = players # temp object 
  setkey(xx)
  xx[,startup:=sum(single.player),by=corporationID] 
  xx[startup>=2] %>% View(.)
  # Self-corporated players don't have more than 3 players. That is, players who
  # play in this way are doing so off of one account, OR I am missing multiple
  # accounts.
  
  cleaned_corp_list = xx[!startup>=2]
  # Retained Single player corporations that don't have more than one char in
  # the corp.
  
  #Remove NPC Corps
  npcID = unique(npc.corps$corporationID)
  cleaned_corp_list = cleaned_corp_list[!corporationID%in%npcID]
  # House keeping
  cleaned_corp_list = cleaned_corp_list[,list(characterID,userID,corporationID,single.player)]
  
  # save(cleaned_corp_list,file="R/cleaned_corp_list.Rdata")
  load("R/cleaned_corp_list.Rdata")
  
  # Back to looking at the tax rate...
  # Let's remove startups (i.e. corps with one member)
  clnd = cleaned_corp_list[single.player!=1]
  clnd = clnd$corporationID
  taxR = taxR[corporationID%in%clnd] # clean corps
  # Now only active corps
  ac = active$corporationID
  taxR = taxR[corporationID%in%ac]
  dim(taxR) # Cleaning reduces the total # of corps down considerably.
  
  # Is there a corr between (a) member count, (b) logon time,(c) logon tax and tax rate?
  cor.test(taxR$taxRate,log(taxR$memberCount)) # Weak neg corr ***
  cor.test(taxR$taxRate,log(taxR$logonMinutes+1)) # Weak pos corr ***
  cor.test(taxR$taxRate,log(taxR$logonCount+1)) # Weak pos corr ***
  # So tax rate isn't fully driven by numbers or experience
  summary(lm(taxR$taxRate~log(taxR$memberCount)+log(taxR$logonMinutes+1)+log(taxR$logonCount+1)))
  # Model indicates that the more members you have the lower your tax structure
  # will be (NOTE: this is a very weak corr)
  
 
  # Tax Rate
  ggplot(taxR,aes(x=taxRate))+
    geom_histogram(binwidth=.01,fill="steelblue",col="white") + 
    theme_bw()
  # Most corporations don't tax much. But there is a non-trivial amount of corps
  # that tax a lot.
  
  # Here is membership on tax rate
  ggplot(taxR,aes(y=taxRate,x=log(memberCount))) + geom_point() + theme_bw()
  
  # Bin the tax rates
  tax.bins = table(cut(taxR$taxRate,breaks = seq(0,1,by=.1)))
  tt = data.frame(tax.bins)
  tt$props = round(tax.bins/sum(tax.bins),3)
  tt
  barplot(tt$props,col="steelblue")

############################################################
# ROUND 3: Active Corporations (using cleaned list) ---- 
############################################################  
  ggplot(taxR,aes(x=log(memberCount))) + geom_histogram(binwidth = .1) +theme_bw()
  ggplot(taxR,aes(x=memberCount)) + geom_histogram(binwidth = 10) + theme_bw() + ggtitle("Active Player Corporations \n (No NPC or Self-Incorporated)")
  
  tt2 = data.frame(table(cut(taxR$memberCount,breaks=c(1,2,5,10,25,50,100,500,1000,5000,10000))))
  tt2$prop = round(tt2$Freq/sum(tt2$Freq),3)
  tt2$Var1 = c("1-2",
               "2-5",
               "5-10",
               "10-25",
               "25-50",
               "50-100",
               "100-500",
               "500-1000",
               "1000-5000",
               "5000+")
  tt2

###########################################
# Corporation Life Cycles ----------------
###########################################
  clife = data.table(sqlQuery(conn,"select 
                              corporationID, 
                              startdate = min(historyDate),
                              enddate= max(historyDate) 
                              from ebs_FACTORY.eve.corporationHistory 
                              group by corporationID "))
  clife$startdate = clife$startdate %>% as.Date(.)
  clife$enddate = clife$enddate %>% as.Date(.)
  
  clife$duration = as.numeric(clife$enddate - clife$startdate)
  clife$startdate %>% min(.) # This only offers us from 2015 Onward
  clife$startdate %>% max(.)
  min(clife$startdate)-max(clife$enddate) # corp history only capture 497 days of activity. 
  
  # Only active corps
  clife2 = clife[corporationID %in% active$corporationID]

  ggplot(clife2,aes(x=duration)) + geom_histogram(binwidth=1,fill="blue") + theme_bw()
  
  
  # need a less truncated sample.
  
  # For now, let's look at all corps that exist from 2015 onward
  cl2015 = data.table(sqlQuery(conn,"SELECT corporationID,createDate, 
                                    memberCount, creatorID,stationID,taxRate 
                                    FROM ebs_RESEARCH.dbo.crpCorporations 
                                    WHERE createDate >= '1/1/2015'"))
  
  # Let's assess just those corps that started in Jan 2015
  cl2015 = cl2015[createDate <= as.Date("2015-01-30") ]
  nrow(cl2015)
  # (1) are there corps NOT on the cleaned list (i.e. single-player owned)
    cl2015[!cl2015$corporationID %in% cleaned_corp_list$corporationID] %>% dim(.) # 65871
  # (2) are on the cleaned list
    cl2015[cl2015$corporationID %in% cleaned_corp_list$corporationID] %>% dim(.) # 26945
      # Take only the corps on the cleaned list
    cl2015clnd <- cl2015[cl2015$corporationID %in% cleaned_corp_list$corporationID]
  # (3) are these corporations still alive?
    M1 = merge(cl2015clnd,clife,by="corporationID") # lost 9 corps in merge
    M1$alive = ifelse(M1$enddate==as.Date("2016-05-12"),1,0)
    table(M1$alive);mean(M1$alive) # 43% of the corps that started in 2015 are still in operation.
    
    # Look at the dist of those who died
    ggplot(M1[alive==0],aes(duration))+geom_histogram(binwidth = 1) + theme_bw()
    
    # How many never made it past a day?
    nrow(M1[alive==0 & duration==0]) # 319 corps were made on a whim
    
    # Let's make a breakdown of survival of the first few months
    data.frame(table(cut(M1[alive==0]$duration,breaks=c(0,1,7,31,62,74,186,365,496))))
  
  # (4) membership --- how big are these corps
    ggplot(M1,aes(memberCount,fill=alive)) + geom_histogram(binwidth=100) + theme_bw()
    ggplot(M1[memberCount<500],aes(memberCount,fill=alive)) + geom_histogram(binwidth=1)+ theme_bw()
  
    mem.bd = data.frame(table(cut(M1$memberCount,breaks=c(1,2,5,10,25,50,100,500,1000,5000,10000))))
    mem.bd$prop = round(mem.bd$Freq/sum(mem.bd$Freq),3)
    mem.bd
    
    # Breakdown by which corps are still in operation and which are not
    gridExtra::grid.arrange(ncol=2,nrow=1,
    ggplot(M1[alive==1],aes(memberCount)) + geom_histogram(binwidth=10,fill="blue") + theme_bw(),
    ggplot(M1[alive==0],aes(memberCount)) + geom_histogram(binwidth=1,fill="red") + theme_bw())
    # Corps that die out never get that big... makes sense.
    
    M1[,mean(memberCount),by=alive] # Corps that thrive have on ave 9 members; ones that die 2. 
    
  # (5) Tax Rate
    ggplot(M1,aes(taxRate)) + geom_histogram(binwidth = .01) + theme_bw()
    # Ave TR by survival
    M1[,mean(taxRate),by=alive]
    t.test(M1[alive==0]$taxRate,M1[alive==1]$taxRate) # diff in means not stat sig
    
    cbind(data.frame(table(cut(M1[alive==1]$taxRate,breaks = seq(0,1,by=.1)))),
          data.frame(table(cut(M1[alive==0]$taxRate,breaks = seq(0,1,by=.1)))))
    
  # (6) Monthly Revenue [query not running]
    cr.activity = data.table(sqlQuery(conn,"select 
                                      corporationID, 
                                      aveRevIn = avg(iskIn),
                                      aveRevOut = avg(iskIn),
                                      aveMarketIn = avg(marketISKGained),
                                      aveMarketOut = avg(marketISKSpent),
                                      aveLogonMin = avg(logonMinutes),
                                      aveLogonCnt = avg(logonCount),
                                      aveCombatKills = avg(combatKillsTotal),
                                      aveTravel = avg(travelWarpsTotal)
                                      from ebs_FACTORY.eve.corporationHistory 
                                      group by corporationID "))
    

###########################################
# Corps: Initial States -------------
###########################################
  # Can we piece together the initial conditions within a specific corporation. 
  # What was going on when the corp first started? How joined? What was the tax 
  # rate? How active was it? How many recruitment adds did it post, etc...
    
  # Goal: construct a function that does all of this in one go.
    conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;');
    date = "2013-11-11"
    corpID = 98188326
    
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
   
  
  