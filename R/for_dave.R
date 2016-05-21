# PURPOSE: Data Vizs for Dave

setwd("~/ETD")
require(data.table)

# Distribution of corporation membership ------ 
conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;');
corps <- data.table(sqlQuery(conn, "SELECT * FROM ebs_FACTORY.eve.activeCorporations"))


pdf(file="plots/OVERVIEW_active_corp_members.pdf",height=14,width=8)
par(mfrow=c(4,1))
hist(corps$members,breaks=100,
     main="All Active Corporations",col="lightgrey",border="white",
     xlab="Number of Members",ylab="Number of Corporations")
hist(corps$members[corps$members>=5 & corps$members<=100],breaks=100,
     main="Active Corporations \n with 5-100 members",col="darkgrey",border="white",
     xlab="Number of Members",ylab="Number of Corporations")
hist(corps$members[corps$members>=101 & corps$members<=500],breaks=100,
     main="Active Corporations \n with 100-500 members",col="black",border="white",
     xlab="Number of Members",ylab="Number of Corporations")
hist(corps$members[corps$members>=501],breaks=100,
     main="Active Corporations \n with 500+ members",col="lightblue",border="white",
     xlab="Number of Members\n (Red Lines Delineate Intervals of 500)",ylab="Number of Corporations")
for(i in 1:80){abline(v=500*i,lty=3,col="red")}
dev.off()

# Differentiate between NPC and Player Corps
npc.corps <- sqlQuery(conn,"SELECT * FROM ebs_RESEARCH.corporation.npcCorporations")
ply.corps = corps[!corps$corporationID %in% npc.corps$corporationID,]
np.corps = corps[corps$corporationID %in% npc.corps$corporationID,]

pdf(file="plots/active_Player_corp_members.pdf",height=10,width=8)
par(mfrow=c(3,1))
hist(ply.corps$members,breaks=100,
     main="All Active Player Corporations",col="lightgrey",border="white",
     xlab="Number of Members",ylab="Number of Corporations")
hist(ply.corps$members[ply.corps$members>=5 & ply.corps$members<=100],breaks=100,
     main="Active Player Corporations \n with 5-100 members",col="darkgrey",border="white",
     xlab="Number of Members",ylab="Number of Corporations")
hist(ply.corps$members[ply.corps$members>=101],breaks=100,
     main="Active Player Corporations \n with 100+ members",col="black",border="white",
     xlab="Number of Members \n (Red Lines Delineate Intervals of 500)",ylab="Number of Corporations")
for(i in 1:10){abline(v=500*i,lty=3,col="red")}
dev.off()



