# internal functions

.LikeTable = function(lstOuts){
  Name=lstOuts$info$model
  lstOuts=list(lstOuts$data)
  names(lstOuts)=Name
  tab = cbind(lstOuts[[1]]$Like_Comp_names,do.call(cbind,lapply(lstOuts,function(x){round(x[["Like_Comp"]],2)})))
  #if(save==TRUE) write.csv(tab,file=file.path(inputPath,"LikelihoodTable2013.csv"),row.names=F)
  return(tab)
}

.Fut_SSB_SD = function(lstOuts){
  Name=lstOuts$info$model
  lstOuts=list(lstOuts$data)
  names(lstOuts)=Name
  fut       <- do.call(rbind,lapply(lstOuts,function(x){
    do.call(rbind,lapply(x[grep("SSB_fut_",names(x))],
                         function(y){return(y[,1:3])}))}))
  fut       <- as.data.frame(fut,stringsAsFactors=F)
  colnames(fut) <- c("year","SSB","SD")
  fut$modelscenario <- paste(rep(names(lstOuts),each=nrow(lstOuts[[1]]$SSB_fut_1) *
                                   length(grep("SSB_fut_",names(lstOuts[[1]])))),
                             paste("Scen",
                                   rep(1:length(grep("SSB_fut_",names(lstOuts[[1]]))),each=nrow(lstOuts[[1]]$SSB_fut_1)),
                                   sep="_"),
                             sep="_")
  return(fut)
}

.SSB_SD = function(lstOuts){
  Name=lstOuts$info$model
  lstOuts=list(lstOuts$data)
  names(lstOuts)=Name
  SSB_SD=lstOuts[[1]]$SSB
  SSB_SD=SSB_SD[,1:3]
  SSB_SD=as.data.frame(SSB_SD,stringsAsFactors=F)
  colnames(SSB_SD) <- c("year","SSB","SD")
  return(SSB_SD)
}

.Puntual_SSB_SD = function(lstOuts,year){
  Name=lstOuts$info$model
  lstOuts=list(lstOuts$data)
  names(lstOuts)=Name
  if (year>lstOuts[[1]]$SSB[nrow(lstOuts[[1]]$SSB),1])
    stop(cat('Year should be lesser than ',lstOuts[[1]]$SSB[nrow(lstOuts[[1]]$SSB),1]))
  ass       <- do.call(rbind,lapply(lstOuts,function(x){
    x$SSB[which(x$SSB[,1] == year),1:3]}))
  ass       <- as.data.frame(ass,stringsAsFactors=F)
  colnames(ass) <- c("year","SSB","SD")
  ass$modelscenario <- names(lstOuts)
  return(ass)
}