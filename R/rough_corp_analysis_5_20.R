# Most recent compilation
load("~/ETD/R/Data/census/corp_census_SAVE_05_20__19_05.Rdata")

# Have an issue list recording all inactive day (e.g. days when a corporation wasn't doing anything)

# Need to build "coordination" measure: which is a variant on net density measures. 

source("~/ETD/R/Functions/generate_network.R") # quick function for anaylsis

require(igraph)
require(dplyr)
require(ggplot2)
require(data.table)
require(RODBC)
conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;')
c.list = interTable$corporationID %>% unique(.) # All corps


# Calculate from the current sample:
    # (A) average network density
      ave_dens = NULL
      for(c in c.list){
        ave_dens = rbind(ave_dens,ave_net_density(c,F))
      }


    # (B) Average coordination
      ave_coord = NULL
      for(c in c.list){
        ave_coord = rbind(ave_coord,coord(c,F))
      }

      hist(unlist(ave_dens$density),breaks=100,col="grey",border="white")
      hist(unlist(ave_coord$ave.coord.density),breaks=100,col="grey",border="white")

# For Basic correlations: build a dataset
      
      # Ave Tax Rate, Std. Dev, and how often the taxrate is changed
      tax = snapTable[,list(mean(taxRate),sd(taxRate),length(unique(taxRate))-1),by=corporationID]
      colnames(tax)=c("corpID","tax","tax.sd","tax.change")
  
      # Ave Combat
      combat = sumTable[,log(mean(combat,na.rm=T)+1),by=corpID]
      colnames(combat) = c("corpID","combat")
      
      # Ave No. of Countries
      ave.countries = sumTable[,mean(no.countries),by=corpID]
      colnames(ave.countries)   = c("corpID","noco")
      
      # Isk Made
      isk = sumTable[,mean(iskMade),by=corpID]
      colnames(isk) = c("corpID","isk")
      
      # age
      age = sumTable[,mean(ave.age),by=corpID]
      colnames(age) = c("corpID","age")
      
      # experience
      exp = sumTable[,mean(ave.exp),by=corpID]
      colnames(exp) = c("corpID","exp")
      
      # Time zone [Number of unique TZ]
      timezone = snapTable[,length(unique(timeZoneID)),by=corporationID]
      colnames(timezone) = c("corpID","timezone")
      
      # Gender [is there more than one gender in your group?]
      gender = snapTable[,length(unique(gender)),by=corporationID]
      colnames(gender) = c("corpID","gender")
      gender[gender==3]$gender = 2
      
      # Average Time played
      timeplayed = sumTable[,mean(ave.hours.played),by=corpID]
      colnames(timeplayed) = c("corpID","timeplayed")
      
      # Corporation is in an alliance
      alliance = data.table(corpID = snapTable$corporationID,
                            alliance=ifelse(is.na(snapTable$allianceID),0,1))
      alliance = alliance[,max(alliance),by=corpID]
      colnames(alliance) = c("corpID","alliance")
      
      # Ave Number players online
      users = sumTable[,mean(total.users),by=corpID]
      colnames(users) = c("corpID","users")
      
      # Security Zone of the Space you Occupy
      # unique(interTable$security)
      xx = interTable[,list(corporationID,security)]
      xx$null = ifelse(xx$security=="Null sec",1,0)
      xx$low = ifelse(xx$security=="Low sec",1,0)
      xx$high = ifelse(xx$security=="High Sec",1,0)
      security = xx[,list(ifelse(length(unique(security))>=2,1,0),mean(null,na.rm=T),mean(low),mean(high)),by=corporationID]
      colnames(security) =c("corpID","sec.div","null","low","high")
      security$sec_zone =  security[,sum(low+high),by=corpID]$V1
      hist(security$null,breaks=20)
      hist(security$low,breaks=20)
      hist(security$high,breaks=20)
      hist(security$sec_zone,breaks=20)
     

      # Wormholes?
      # wh = read.csv("~/ETD/Wormhole.csv",stringsAsFactors = F)
      # interTable[ssname%in%wh$J.] # Doesn't appear if I've captured Wormhole activity
      
      # bring it together...
      M = merge(ave_dens,ave_coord,by="corpID")
      M = merge(M,tax,by="corpID")
      M = merge(M,combat,by="corpID")
      M = merge(M,ave.countries,by="corpID")
      M = merge(M,isk,by="corpID")
      M = merge(M,age,by="corpID")
      M = merge(M,exp,by="corpID")
      M = merge(M,timezone,by="corpID")
      M = merge(M,gender,by="corpID")
      M = merge(M,timeplayed,by="corpID")
      M = merge(M,alliance,by="corpID")
      M = merge(M,users,by="corpID")
      M = merge(M,security,by="corpID")
      
# Basic Correlations
      
      Qcor("density",.4,M,c("corpID","ave.coord.density"))
      Qcor("ave.coord.density",.4,M,c("corpID","density"))
      Qcor("combat",.4,M,c("corpID"))
      Qcor("tax",.4,M,c("corpID"))
      Qcor("timeplayed",.4,M,c("corpID"))
      
      Qlm(dv="ave.coord.density",M,c("corpID","density"))
      Qlm(dv="density",M,c("corpID","ave.coord.density"))
      
      # Time zone is a great predictor
      M$combat %>% range(.)
      
      summary(lm(combat~density+noco+timezone+taxRate,data=M))
      
      summary(lm(ave.coord.density~null+low+high,data=M))
      summary(lm(ave.coord.density~null+sec_zone,data=M))
      # Can't have all security zones in one...since when in combination they are invariant. (add to 1)
      
      summary(lm(ave.coord.density~null,data=M))
      summary(lm(ave.coord.density~sec_zone,data=M))
      
      summary(lm(ave.coord.density~null+tax+noco+timezone+gender+age+exp+timeplayed+alliance+combat,data=M))
      summary(lm(ave.coord.density~low+tax+noco+timezone+gender+age+exp+timeplayed+alliance+combat,data=M))
      summary(lm(ave.coord.density~high+tax+noco+timezone+gender+age+exp+timeplayed+alliance+combat,data=M))
      
      
      
      summary(lm(density~null+tax+noco+timezone+gender+age+exp+timeplayed+alliance+combat,data=M))
      summary(lm(density~low+tax+noco+timezone+gender+age+exp+timeplayed+alliance+combat,data=M))
      summary(lm(density~high+tax+noco+timezone+gender+age+exp+timeplayed+alliance+combat,data=M))
      
      
      
      summary(lm(tax~ave.coord.density+noco+timezone+gender+age+exp,data=M))
      summary(lm(tax.change~ave.coord.density+noco+timezone+gender+age+exp,data=M))
      summary(lm(tax~density+noco+timezone+gender+age+exp,data=M))
      
      # There is a strong relationship between how coordinated a group is and
      # it's likelihood of being in an alliance
      summary(lm(alliance~taxRate+ave.coord.density+noco+timezone+gender+age+exp+timeplayed,data=M))
      summary(lm(alliance~taxRate+density+noco+timezone+gender+age+exp+timeplayed,data=M))
      
      
      # 
      ggplot(M) + geom_point(aes(ave.coord.density,tax)) + theme_bw()
      ggplot(M) + geom_point(aes(density,tax)) + theme_bw()
      ggplot(M) + geom_point(aes(tax.change,tax)) + theme_bw()
      
      
      plot(M$density,M$combat,pch=16,col="grey")
      plot(M$taxRate,M$ave.coord.density,pch=16,col="grey")
      
      plot(M$noco,M$density,pch=16,col="lightblue")
      plot(M$noco,M$ave.coord.density,pch=16,col="maroon")
      
      
# Mapping networks

    net.out = gen_net("98169165",plot = F)

      
    coord("98169165",T)
      
      