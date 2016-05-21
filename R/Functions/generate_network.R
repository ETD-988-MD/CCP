# GEN. NETWORK [ALL TIES] ----
gen_net = function(corpID,plot=F){
  # Quickly Generates a network object
  require(igraph);require(data.table)
  sub = interTable[corporationID==corpID]
  subST = snapTable[corporationID==corpID]
  EM = as.matrix(sub[,list(char_A,char_B)])
  colnames(EM) = NULL
  actors = unique(subST$characterID)
  net <- graph_from_data_frame(EM,actors,directed=F)
  if(plot==T){
    plot.igraph(net,vertex.color="steelblue",vertex.frame.color="white",
                edge.color="grey",edge.width=1,vertex.size=3,vertex.label="")
  }else{return(net)}
}

# GEN. NETWORK [REDUCED TIES] ----
gen_net2 = function(corpID,interTable,snapTable,plot=F){
  # Quickly Generates a network object -- but subset so that there is only one 
  # tie for the period of interest
  require(igraph);require(data.table)
  sub = interTable[corporationID==corpID]
  subST = snapTable[corporationID==corpID]
  EM = as.matrix(sub[,list(char_A,char_B)])
  EM = unique(EM)
  colnames(EM) = NULL
  actors = unique(subST$characterID)
  net <- graph_from_data_frame(EM,actors,directed=F)
  if(plot==T){
    plot.igraph(net,vertex.color="steelblue",vertex.frame.color="white",
                edge.color="grey",edge.width=1,vertex.size=3,vertex.label="")
  }else{return(net)}
}


# AVE DAILY NET DENSITY ----
# Doesn't take into account location
ave_net_density = function(corpID,daily_calc=T,all.actors=F){
  require(data.table)
  sub = interTable[corporationID==corpID]
  if(all.actors==T){# Consider all actors who logged on within the rel time period
    subST = snapTable[corporationID==corpID]
    actors = unique(subST$characterID)
  }
  dates = unique(sub$analysisDate)
  out = NULL
  out= data.table(t(sapply(dates,function(d){
    sub2 = sub[analysisDate==d]
    EM = as.matrix(sub2[,list(char_A,char_B)])
    if(all.actors==F){ # Only consider players that are online for that day
      actors = unique(c(EM))
    }
    
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
  if(daily_calc==T){
   return(out)
  } else{ return(out2) }
}


# AVE DAILY COORDINATION --------
    # "Coordination" is a variant on network density measures. It takes into
    # account the potential connection by calculating all potential ties between
    # nodes AND node location. Every set of pairs could also exist for each
    # location. The closer to one, the more players who are online coordinate
    # (i.e. are systematically in the same system aroudn the same time.)

coord = function(corpID,daily_calc=T){
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
    loc = nrow(unique(sub2[,list(locationID)]))
    # Calculate the network density by expanding all potential connections to each location
    vc = length(actors)
    ac = nrow(EM)/2 # All actual connections (but adjusted for dyadic reversal)
    pc = ((vc*(vc-1))/2)*loc
    coord.density = ac/pc
    out = rbind(out,data.table(date=d,coord.density))
    out
  })))
  out2 = data.table(corpID,ave.coord.density = mean(unlist(out$coord.density)))
  if(daily_calc==T){
    return(out)
  } else{ return(out2) }
}
# coord(corpID,F)





# Analysis -------------

# Quick correlation
Qcor = function(var,strength=.4,data=M,excl.var){
  data = as.data.frame(data)
  main = data[,var]
  candidates = colnames(data)[!colnames(data)%in%c(excl.var,var)]
  candidates = as.data.frame(data)[candidates]
  out= plyr::ldply(candidates,function(x){
    cc = cor.test(main,x)
    data.table(corr=round(cc$estimate,2),
               ifelse(round(cc$estimate,2)>=strength,
                      "  Strong Pos  ",
                      ifelse(round(cc$estimate,2)<=-strength,
                             "  Strong Neg  ","")),
               p.val=round(cc$p.value,2),
               ifelse(round(cc$p.value,2)<=.05,
                      "***",""))
  })
  colnames(out)[c(1,3,5)] = ""
  cat(paste0("Correlation Summary:  ",var,"\n\n"))
  print(out)
}

# Quick linear models
Qlm = function(dv,data=M,excl.var){
  data = as.data.frame(data)
  main = data[,dv]
  candidates = colnames(data)[!colnames(data)%in%c(excl.var,dv)]
  candidates = as.data.frame(data)[candidates]
  out= plyr::ldply(candidates,function(x){
    cc = summary(lm(main~x))
    round(cc$coefficients,2)[-1,]
  })
  colnames(out)[c(1)] = ""
  cat(paste0("Bivariate Linear Model Summary:\nDV: ",dv,"\n\n"))
  print(out)
}


