# Group Level Mobilization
setwd("~/ETD/R/")

# Census Function
source("~/ETD/R/Functions/daily_corp_census_v2.R")

require(igraph)
require(RColorBrewer)

# Focusing just on 2016 activity, locate individual players in the same space and time

    conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;')
    day_data = corp_census("2016-04-07",98188326,t.thres = 6,print.report = T,conn)
    
    
    # Visualizing the network for a day
    require(igraph)
    EM = as.matrix(day_data$player_interactions[,list(char_A,char_B)])
    colnames(EM) = NULL
    actors = day_data$day_snapshot$characterID
    net <- graph_from_data_frame(EM,actors,directed=F) 
    
    l = layout.fruchterman.reingold(net)
    walk <- walktrap.community(net)
    
    new.color = data.frame(col2rgb(walk$membership)/255)
    new.color = rgb(new.color)
    
    summary(net)
    plot.igraph(net,layout=l,vertex.color="steelblue",vertex.frame.color="white",
                edge.color="grey",edge.width=1)
    
    
    net.stat = data.table(characterID=actors,degree=degree(net),between=log(betweenness(net)+1),
                          close=log(closeness(net)+1),constraint=log(constraint(net)+1),
                          eigen=log(evcent(net)$vector+1))
    graph.density(net,loops = F)

    
# Piece together a single corporation network over a two month period
    
    date.range = seq(from=as.Date("2016-03-01"),to=as.Date("2016-04-30"),by="day")
    # Loop through the days
    sumTable = NULL;snapTable = NULL;interTable = NULL
    for(j in 1:length(date.range)){
      temp = corp_census(date.range[j],98188326,t.thres = 1,print.report = F,conn)
      sumTable = rbind(sumTable,temp$aggregate_summary)
      snapTable = rbind(snapTable,temp$day_snapshot)
      interTable = rbind(interTable,temp$player_interactions)
    }
   
    EM = as.matrix(interTable[,list(char_A,char_B)])
    colnames(EM) = NULL
    actors = unique(snapTable$characterID)
    net <- graph_from_data_frame(EM,actors,directed=F)
    # Color By Country
    cn=c()
    for(i in 1:length(actors)){cn = c(cn,unique(snapTable[snapTable$characterID==actors[i]]$countryName))}
    V(net)$country = cn
    col.sch = data.table(country=unique(cn),
                         color= brewer.pal(6,"Set2"))
    colors = merge(col.sch,data.table(country=cn),by="country")[,color]
    pdf(file="~/ETD/R/Plots/corp_98188326_network_apr2016.pdf",height=10,width=10)
    plot.igraph(net,vertex.color=colors,vertex.frame.color="white",
                edge.color="grey",edge.width=1,vertex.size=3,vertex.label="")
    dev.off()
    
    
# Piecing together a sample of all active corporation list for one month
    load("~/ETD/R/Data/cleaned_corp_list.Rdata")
    active <- data.table(sqlQuery(conn,"SELECT * FROM ebs_FACTORY.eve.activeCorporations"))
    active = active[active$corporationID %in% cleaned_corp_list$corporationID]
    active = active[members>=6] # Take only corporations with more than 6 members
    active.corps = active$corporationID
    
    # Start with 98248637
    
    date.range = seq(from=as.Date("2016-04-01"),to=as.Date("2016-04-30"),by="day")
    sumTable = NULL;snapTable = NULL;interTable = NULL;issue.list = NULL
    save.intervals = seq(0,length(active.corps),by=250)[-1]
    for(corp in 3258:length(active.corps)){
      if(corp %in% save.intervals){
        # Built in "Save Points" ... every 250 corps that are processed, save
        save(sumTable,interTable,snapTable,issue.list,file=paste0("~/ETD/R/Data/census/corp_census_SAVE_",format.Date(Sys.time(),"%m_%d__%H_%m"),".Rdata"))
      }
      # Re-estabplish the connection (in case of time out)
      conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;')
      print(paste0(active.corps[corp]," - ",round(corp/length(active.corps)*100,2),"% complete"))
      for(j in 1:length(date.range)){
        temp = try(corp_census(date.range[j],corpID = active.corps[corp],t.thres = 1,print.report = F,conn),silent=T)
        if(length(temp)==1){ # Record issue days (where there is no information from the SQL draw)
          issue.list = rbind(issue.list,data.frame(corp=active.corps[corp],date=date.range[j]))
          print(paste0("     ! ",active.corps[corp]," : ",date.range[j]))
          next()
        }
        sumTable = rbind(sumTable,temp$aggregate_summary)
        snapTable = rbind(snapTable,temp$day_snapshot)
        interTable = rbind(interTable,temp$player_interactions)
      }
      close(conn)
      if(corp == 11376){
        # Save when finished
        save(sumTable,interTable,snapTable,issue.list,file=paste0("~/ETD/R/Data/census/corp_census_FINAL_",format.Date(Sys.time(),"%m_%d__%H_%m"),".Rdata"))
      }
    }
    
    
    
    
    
    
    
    