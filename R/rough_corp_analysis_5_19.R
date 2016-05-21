

# Preliminary Analysis on the flawed subset of the available data.
load("~/ETD/R/Data/corp_census_5_19_2pm.Rdata")

source("~/ETD/R/Functions/generate_network.R") # quick function for generating the network.

require(igraph)
require(dplyr)
require(data.table)
require(RODBC)
conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;')

# The interaction table is inflated because the interaction window is too large 
# (6 hours). But maybe I can use this data to build the code that I will run on
# on the main sample.

# DENSITY and TAX RATE --------
    # Generate Network density scores for the networks we have
    c.list = interTable$corporationID %>% unique(.) # All corps
    
    C_dens = NULL
    for(i in 1:length(c.list)){
      dd = data.table(corp = c.list[i],density = graph.density(gen_net(corpID = c.list[i],interTable,snapTable),loop=F))
      C_dens = rbind(C_dens,dd)
    }
    
    # let's see if the (average) tax rate is correlated with network density
    c_tax = NULL 
    for(i in 1:length(c.list)){
      tt = data.table(
        corp = c.list[i],
        taxRate = mean(snapTable[corporationID==c.list[i]]$taxRate)
      )
      c_tax = rbind(c_tax,tt)
    }
    
    M1 = merge(c_tax,C_dens,by="corp")
    
    cor.test(M1$taxRate,M1$density)
    summary(lm(density~taxRate,data=M1))
    plot(y = M1$density,x=M1$taxRate)
    # No Relationship what so ever.
    
    C_dens = NULL
    for(i in 1:length(c.list)){
      # More conservative estimation of density... That is, one tie per period
      n = gen_net2(corpID = c.list[i],interTable,snapTable)
      vc = length(V(n))
      ac = length(E(n))/2 # we are overcounting the actual connections by double
      pc = (vc*(vc-1))/2 
      
      dd = data.table(corp = c.list[i],density = ac/pc)
      C_dens = rbind(C_dens,dd)
    }
    # No matter how you swing it...there is just no relationship
    
    # This might not mean anything. First, the threshold for interactions is quite 
    # large... and probably not reasonable ... that said, shrinking the threshold
    # might not do much.

# DENSITY and COMBAT -----------
    # Are denser networks associated with a higher level of combat?
    hh = sumTable[,mean(combat,na.rm=T),by=corpID]
    colnames(hh) = c("corp","combat")
    M2 = merge(C_dens2,hh,by="corp") # Let's use our more conservative density measure....
    cor.test(M2$density,log(M2$combat+1)) 
    plot(x=M2$density,y=log(M2$combat+1))
    summary(lm(density~log(combat+1),data=M2))
    summary(lm(log(combat+1)~density,data=M2))
    # The denser you're network...the less you engage in combat.
    
    # what's the effect if we think about network density a little differently.
    M2 = merge(C_dens,hh,by="corp") # Here all ties are taken under consideration
    cor.test(log(M2$density+1),log(M2$combat+1)) 
    plot(x=log(M2$density+1),y=log(M2$combat+1))
    # Looks parabolic
    summary(lm(log(combat+1)~log(density+1),data=M2))
    summary(lm(log(combat+1)~log(density+1)+I(log(density+1)^2),data=M2))
    # Yep -- there is a parabolic relationship here...inverse u-shaped
    
    # So depending on how we think about network density, we get a very different picture...I think the 

# AVE NO. OF COUNTRIES and DENSITY --------------- 
  # When you have more players from different countries, are you more likely to
  # play together?
    ave.countries = sumTable[,mean(no.countries),by=corpID]
    colnames(ave.countries)   = c("corp","noco")
    M3 = merge(C_dens2,ave.countries,by="corp")
    cor.test(M3$density,M3$noco)
    plot(y=M3$density,x=M3$noco)
    summary(lm(density~noco,data=M3)) # Weak negative relationship
    # We have an outlier here with ave of 40 countries participating in it.
    M3[noco>40]
    sqlQuery(conn,"SELECT * 
                  FROM ebs_RESEARCH.corporation.corporationsLookup
                  WHERE corporationID = 98169165")
    # Brave Newbies Inc. (Badivefi VI - Moon 2 - Royal Khanid Navy Logistic Support)
    
    # Interesting.... let's drop them
    M3 = M3[noco<40]
    summary(lm(density~noco,data=M3))
    plot(y=M3$density,x=M3$noco)
    
    # There isn't a strong story here... the diverse range of countries that you
    # come from doesn't change
  
    
    
      
# DENSITY and AGE ------ 
    ave.age = sumTable[,mean(ave.age),by=corpID]
    colnames(ave.age)   = c("corp","age")
    M4 = merge(C_dens2,ave.age,by="corp")
    cor.test(M4$density,M4$age)
    plot(y=M4$density,x=M4$age)
    summary(lm(density~age,data=M4))
    # A light but *** rel b/t age and density.
    
    
# DENSITY Measures
    
    # I need a better density measure... need a function that looks at the 
    # relative density by day and then takes the average density...will build a
    # function that does this....
    
    ave_net_density = function(corpID){
      require(data.table)
      sub = interTable[corporationID==corpID]
      subST = snapTable[corporationID==corpID]
      actors = unique(subST$characterID)
      dates = unique(sub$analysisDate)
      out = NULL
      out= data.table(t(sapply(dates,function(d){
        sub2 = sub[analysisDate==d]
        EM = as.matrix(sub2[,list(char_A,char_B)])
        actors = unique(c(EM)) # Only consider players that are online for that day
        EM = unique(EM) # subset pair values -- reduce the number of ties
        # Calculate the network density
        vc = length(actors)
        ac = nrow(EM)/2
        pc = (vc*(vc-1))/2
        density.day = ac/pc
        out = rbind(out,data.table(date=d,density.day))
        out
      })))
      out2 = data.table(corpID,density = mean(unlist(out$density.day)))
      return(out2)
    }
   
    den.list = sapply(c.list,function(c){
      hold = ave_net_density(c)
      hold$density
    })
   C_den3 = data.table(corp= c.list,density=den.list)

    
    
   # Let's use this new measure to relook at some of the relationships above
   M5 = merge(c_tax,C_den3,by="corp") # Tax
   cor.test(M5$taxRate,M5$density)
   summary(lm(density~taxRate,data=M5))
   plot(y = M5$density,x=M5$taxRate)
   
   M5 = merge(C_dens2,hh,by="corp") # Combat
   cor.test(log(M5$combat+1),M5$density)
   summary(lm(log(combat+1)~density,data=M5))
   plot(x = M5$density,y=log(M5$combat+1))
   
   M5 = merge(C_den3,ave.countries,by="corp") # country
   M5 = M5[noco<40] # remove
   cor.test(M5$density,M5$noco)
   plot(y=M5$density,x=M5$noco)
   summary(lm(density~noco,data=M5))
   # Wow! As the group becomes more diverse, they become less dense...
   # This probably has to do with time zone...
   
   M5 = merge(C_den3,ave.age,by="corp")
   cor.test(M5$density,M5$age)
   plot(y=M5$density,x=M5$age)
   summary(lm(density~age,data=M5))
    
    
    
    
    
# Mapping the networks

set.seed(123)
draws = c.list[sample(1:length(c.list),20,replace = F)]
gen_net(corpID = draws[9],plot=T,interTable,snapTable)
gen_net2(corpID = draws[9],plot=T,interTable,snapTable)













