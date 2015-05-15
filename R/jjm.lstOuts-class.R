.combineModels <- function(...){
  
  modelList <- list(...)
  
  for(i in modelList)
    if(class(i) != "jjm.output")
      stop("Objects must be of class 'jjm.output'.")
  
  modelList <- c(...)
  
  # Remove repeated models from modelList 
  modelList2 <- modelList
  for(i in seq_along(modelList[-length(modelList)]))
    for(j in seq(from = i + 1, to = length(modelList)))
      if(identical(modelList[[i]], modelList[[j]]))
        modelList2[[j]] <- NULL
  
  modelList <- modelList2; rm("modelList2")
  
  modelNames <- NULL
    for(j in seq_along(modelList)){
      modelNames <- c(modelNames, modelList[[j]]$info$output$model)
    }
    
  # Models
  models <- list()
  for(i in seq_along(modelList)){
    models[[i]] <- modelList[[i]][c("info", "output", "data")]
  }
  
  names(models) <- modelNames

  output = models 

  class(output) <- c("jjm.output")
  
  return(output)

}

print.jjm.lstOuts <- function(x, ...) {
  cat("\nOutput list from:\n\n")
  print(x$info, ...)
  
  return(invisible())
}

summary.jjm.lstOuts = function(object,...) {
    
  output <- list()
  
  output$info <- object$info
  output$like <- .LikeTable(object)
  output$fut  <- .Fut_SSB_SD(object)
  output$SSB  <- .SSB_SD(object)
  
  class(output) <- "summary.jjm.lstOuts"
  return(output)
}

print.summary.jjm.lstOuts = function(x, ...) {
  
  cat("\nList of models:\n")
  print(x$info, ...)
  
  cat("\nLikelihood Table:\n")
  print(x$like, ...)
  
  cat("\nFuture SSB and SD:\n")
  print(x$fut, ...)
  
  cat("\nSSB and SD:\n\n")
  print(x$SSB, ...)
  
  return(invisible(x))
}

plot.jjm.lstOuts <- function(lstObject, what = "SSB", SD = TRUE, 
                             Sum = NULL, startYear = NULL, legendPos = "topright", 
                             YrInd = FALSE, Apply = "mean", ...) {

  .compareTime(lstObject, Slot="TotBiom", SD=SD, Sum=Sum, startYear=startYear, legendPos=legendPos, ...)
  .compareTime(lstObject, Slot="SSB", SD=SD, Sum=Sum, startYear=startYear, legendPos=legendPos, ...)
  .compareTime(lstObject, Slot="TotBiom_NoFish", SD=SD, Sum=Sum, startYear=startYear, legendPos=legendPos, ...)
  .compareTime(lstObject, Slot="SSB_NoFishR", SD=SD, Sum=Sum, startYear=startYear, legendPos=legendPos, ...)
  .compareTime(lstObject, Slot="R", SD=SD, Sum=Sum, startYear=startYear, legendPos=legendPos, ...)
  .compareTime(lstObject, Slot="SSB_fut_1", SD=SD, Sum=Sum, startYear=startYear, legendPos=legendPos, ...)
  .compareMatrix(lstObject, Slot="TotF", Sum=Sum, YrInd=YrInd, Apply=Apply, startYear=startYear, legendPos=legendPos, ...)
  
  return(invisible())
}


logLik.jjm.lstOuts = function(object, detailed = FALSE, ...) {
  
  if(isTRUE(detailed))
    like <- .detailedIndLikelihood(object) else
      like <- .LikeTable(object)
  
  return(like)
}


kobe.jjm.lstOuts = function(model, add=FALSE, col=NULL, Bref = 1, Fref = 1, Blim = Bref, Flim = Fref,  
                            xlim = NULL, ylim = NULL, ...) {
  
  nModels = length(model)
  
  kob  = list(nModels)
  maxB = numeric(nModels)
  maxF = numeric(nModels)
  n = numeric(nModels)
  for(i in seq(nModels)){
    kob[[i]] = model$data[[i]]$output$output$msy_mt
    kob[[i]] = kob[[i]][,c(1,4,13)]
    maxB[i] = max(kob[[i]][,3]) 
    maxF[i] = max(kob[[i]][,2]) 
    n[i] = nrow(kob[[i]])
  }
  
  maxBmsy = max(maxB)
  maxFmsy = max(maxF)
  
  if(!isTRUE(add)) {
    
    if(is.null(xlim)) xlim= range(pretty(c(0, maxBmsy)))
    if(is.null(ylim)) ylim= range(pretty(c(0, maxFmsy)))
    
    plot.new()
    plot.window(xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
    par(xpd = TRUE)
    
    ylim = par()$usr[3:4]
    zero = ylim[1]
    
    polygon(x=c(0, 0, Bref, Bref),
            y=c(Fref, ylim[2], ylim[2], Fref),
            col=rgb(1, 165/255, 0, alpha = 0.5), border=NA)
    polygon(x=c(0, 0, Bref, Bref),
            y=c(zero, Fref, Fref, zero),
            col=rgb(1, 1, 0, alpha = 0.5), border=NA)
    polygon(x=c(Bref, Bref, xlim[2], xlim[2]),
            y=c(Fref, ylim[2], ylim[2], Fref),
            col=rgb(1, 1, 0, alpha = 0.5), border=NA)
    polygon(x=c(Bref, Bref, xlim[2], xlim[2]),
            y=c(zero, Fref, Fref, zero),
            col = rgb(0, 1, 0, alpha = 0.5), border=NA)
    polygon(x=c(0, 0, Blim, Blim),
            y=c(Flim, ylim[2], ylim[2], Flim),
            col=rgb(1, 0, 0, alpha = 0.5), border=NA)
    
    mtext(toExpress("F/F[msy]"), 2, line=2.5)
    mtext(toExpress("B/B[msy]"), 1, line=2.5)
    axis(1, las=1)
    axis(2, las=2)
    box()
    
  }
  
  for(i in seq(nModels)){
    if(is.null(col)){
    text(kob[[i]][c(1,n[i]),3] + 0.01, kob[[i]][c(1,n[i]),2] + 0.1, labels=range(kob[[i]][,1]), cex=0.6,
        adj=-0.2, col=i)
    lines(kob[[i]][,3], kob[[i]][,2], type="b", pch=19, cex=0.5, col= i)
    points(kob[[i]][c(1,n[i]),3], kob[[i]][c(1,n[i]),2], pch=c(15, 17), col= i, cex=0.8)
    }
    
    else {
      text(kob[[i]][c(1,n[i]),3] + 0.01, kob[[i]][c(1,n[i]),2] + 0.1, labels=range(kob[[i]][,1]), cex=0.6,
         adj=-0.2, col=col[i])
      lines(kob[[i]][,3], kob[[i]][,2], type="b", pch=19, cex=0.5, col= col[i])
      points(kob[[i]][c(1,n[i]),3], kob[[i]][c(1,n[i]),2], pch=c(15, 17), col= col[i], cex=0.8)
    }
    
    }	
  
  return(invisible())
}

