# auxiliary functions

.compareTime <-  function(lstOuts, Slot = "TotBiom", SD = FALSE, Sum = NULL, startYear = NULL, legendPos = "topright",
                          ylim = NULL, grid = TRUE, yFactor = 1, main = NA, ylab = Slot,
                          linesCol = NULL, lwd = 1, lty = 1, ...){
  
  dat <- lapply(lstOuts$data, function(x){return(x[[Slot]])})
  nms <- names(dat)
  
  if(!is.null(Sum)){
    nms <- c(nms, paste(Sum[1], "+", Sum[2], sep = ""))}
  
  nD <- length(dat)
  if(is.null(startYear)){
    xrange <- range(unlist(lapply(dat, function(x){x[,1]})), na.rm = TRUE)
  }else {
    xrange <- c(startYear, range(unlist(lapply(dat, function(x){x[,1]})), na.rm = TRUE)[2])
  }
  
  dat <- lapply(dat, function(x){idx <- which(x[,1] %in% xrange[1]:xrange[2]); return(x[idx,])})
  
  if(is.null(ylim)) 
    ylim <- range(pretty(range(unlist(lapply(dat, function(x){x[,4:5]})), na.rm = TRUE)))
  
  if(is.null(linesCol))
    linesCol <- rainbow(nD) else
      linesCol <- rep(linesCol, length.out = nD)
  
  if(is.na(main))
    mar <- c(2, 4, 0.1, 0.1) else
      mar <- c(2, 4, 2, 0.1)
  
  par(mar = mar, xaxs = "i")
    
  plot(x = dat[[1]][,1], y = dat[[1]][,2]*yFactor, col = linesCol[1], type = "l", main = main,
       ylim = ylim, xlim = xrange, axes = FALSE, lwd = lwd, lty = lty, ylab = ylab, ...)
  
  if(grid) grid()
  axis(1)
  axis(2, las=2)
  box()
  
  for(i in 2:nD)
    lines(x = dat[[1]][,1], y = dat[[i]][,2]*yFactor, col = linesCol[i], lwd = lwd, lty = lty)
  
  if(!is.null(Sum)){
    idx1    <- which(nms == Sum[1])
    idx2    <- which(nms == Sum[2])
    datsum  <- colSums(rbind(dat[[idx1]][,2], dat[[idx2]][,2]))
    
    lines(x = dat[[idx1]][,1], y = datsum*yFactor, col = nD + 1, lwd = lwd, lty = lty)
  }
  
  if(SD){
    for(i in 1:nD){
      polygon(x = c(dat[[i]][,1], rev(dat[[i]][,1])),
              y = c(dat[[i]][,4], rev(dat[[i]][,5]))*yFactor,
              col = adjustcolor(linesCol[i], alpha.f = 0.1), border = 0)
    }
  }
  
  legend(legendPos, legend = nms, col = linesCol, lwd = lwd, lty = lty, box.col = NA)
  box()
}

.compareTimes <- function(lstOuts, Slots, SD = FALSE, Sum = NULL, YrInd = NULL, Apply = "mean", startYear = NULL,
                          legendPos = "topright", xlim = NULL, ylim = NULL){
  lstOuts <- lstOuts$data  
  
  nD          <- length(lstOuts)
  idxD        <- as.list(rep(numeric(1), nD))
  names(idxD) <- names(lstOuts)
  
  for(i in 1:nD)
    idxD[[i]] <- grep(Slots, names(lstOuts[[i]]))
  
  dat         <- as.list(rep(numeric(1), nD))
  names(dat)  <- names(lstOuts)
  
  if(is.null(YrInd)){
    xrange  <- range(unlist(lapply(dat, function(x){x[,1]})), na.rm = TRUE)
    yrange  <- range(pretty(range(dat), na.rm = TRUE))
  } else {
    xrange  <- range(YrInd, na.rm = TRUE)
    yrange  <- range(pretty(range(dat), na.rm = TRUE))
    
    for(i in 1:nD){
      if(length(YrInd) != length(dat[[i]])){
        dat[[i]] <- cbind(rev(rev(YrInd)[-1]), dat[[i]])
      } else {
        dat[[i]] <- cbind(YrInd, dat[[i]])
      }
    }
  }
  
  Apply <- get(Apply)
  
  for(i in 1:nD){
    for(j in idxD[[i]]){
      dat[[i]] <- cbind(dat[[i]], lstOuts[[i]][[j]])
    }
    dat[[i]] <- dat[[i]][,-1]
    
    if(length(idxD[[i]])>1)
      dat[[i]] <- apply(dat[[i]], 1, Apply)
  }
  nms     <- names(dat)
  
  if(!is.null(Sum)){
    nms <- c(nms, paste(Sum[1], "+", Sum[2], sep = ""))}
  
  
  
  if(is.null(startYear)){xrange <- range(unlist(lapply(dat, function(x){x[,1]})), na.rm = TRUE)
  } else {
    xrange <- c(startYear, range(unlist(lapply(dat, function(x){x[,1]})), na.rm = TRUE)[2])}
  
  dat     <- lapply(dat,function(x){idx <- which(x[,1] %in% xrange[1]:xrange[2]); return(x[idx,])})
  
  if(is.null(ylim)==F) yrange <- ylim
  if(is.null(xlim)==F) xrange <- xlim
  
  plot(x=dat[[1]][,1],y=dat[[1]][,2],type="l",lwd=2,xlab="Years",ylab=Slots,
       xlim=xrange,ylim=yrange)
  grid(); box()
  for(i in 1:nD)
    lines(x=dat[[i]][,1],y=dat[[i]][,2],col=i,lwd=2)
  if(!is.null(Sum)){
    idx1  <- which(nms==Sum[1])
    idx2  <- which(nms==Sum[2])
    datsum<- colSums(rbind(dat[[idx1]][,2],dat[[idx2]][,2]))
    lines(x=dat[[idx1]][,1],y=datsum,col=nD+1,lwd=2)
  }
  if(SD){
    for(i in 1:nD){
      iCol  <- col2rgb(i)
      iCol  <- rgb(iCol[1]/255,iCol[2]/255,iCol[3]/255,0.25)
      polygon(x=c(dat[[i]][,1],rev(dat[[i]][,1])),
              y=c(dat[[i]][,4],rev(dat[[i]][,5])),col=iCol,border=0)
    }
  }
  legend(legendPos,legend=c(nms),col=1:length(nms),lwd=2,lty=1,box.lty=0)
  box()
  
}

.compareMatrix <- function(lstOuts, Slot, SD = FALSE, Sum = NULL, YrInd = NULL, Apply = mean, startYear = NULL,
                           legendPos = "topright", xlim = NULL, ylim = NULL){
  dat     <- lapply(lstOuts,function(x){return(x[[Slot]])})
  nms     <- names(dat); if(!is.null(Sum)){nms <- c(nms,paste(Sum[1],"+",Sum[2],sep=""))}
  
  nD      <- length(dat)
  if(is.null(YrInd)){
    xrange  <- range(unlist(lapply(dat,function(x){x[,1]})),na.rm=T)
    yrange  <- range(pretty(range(unlist(lapply(dat,function(x){apply(x[,-1],1,Apply)})),na.rm=T)))
  } else {
    xrange  <- range(YrInd,na.rm=T)
    yrange  <- range(pretty(range(unlist(lapply(dat,function(x){apply(x,1,Apply)})),na.rm=T)))
    for(i in 1:nD){
      if(length(YrInd) != nrow(dat[[i]])){
        dat[[i]] <- cbind(rev(rev(YrInd)[-1]),dat[[i]])
      } else {
        dat[[i]] <- cbind(YrInd,dat[[i]])
      }
    }
  }
  for(i in 1:nD) dat[[i]] <- cbind(dat[[i]][,1],apply(dat[[i]][,-1],1,Apply))
  
  if(is.null(startYear)){xrange <- range(unlist(lapply(dat,function(x){x[,1]})),na.rm=T)
  } else { xrange <- c(startYear,range(unlist(lapply(dat,function(x){x[,1]})),na.rm=T)[2])}
  
  dat     <- lapply(dat,function(x){idx <- which(x[,1] %in% xrange[1]:xrange[2]); return(x[idx,])})
  
  if(is.null(ylim)==F) yrange <- ylim
  if(is.null(xlim)==F) xrange <- xlim
  plot(x=dat[[1]][,1],y=dat[[1]][,2],type="l",lwd=2,xlab="Years",ylab=Slot,
       xlim=xrange,ylim=yrange)
  grid(); box()
  for(i in 1:nD)
    lines(x=dat[[i]][,1],y=dat[[i]][,2],col=i,lwd=2)
  if(!is.null(Sum)){
    idx1  <- which(nms==Sum[1])
    idx2  <- which(nms==Sum[2])
    datsum<- colSums(rbind(dat[[idx1]][,2],dat[[idx2]][,2]))
    lines(x=dat[[idx1]][,1],y=datsum,col=nD+1,lwd=2)
  }
  if(SD){
    for(i in 1:nD){
      iCol  <- col2rgb(i)
      iCol  <- rgb(iCol[1]/255,iCol[2]/255,iCol[3]/255,0.25)
      polygon(x=c(dat[[i]][,1],rev(dat[[i]][,1])),
              y=c(dat[[i]][,4],rev(dat[[i]][,5])),col=iCol,border=0)
    }
  }
  legend(legendPos,legend=c(nms),col=1:length(nms),lwd=2,lty=1,box.lty=0)
  box()
}

.getParameters <- function(patternList, myList) {
  
  list3 <- NULL
  for(i in seq_along(patternList))
    if(names(patternList)[i] %in% names(myList))
      list3[[i]] <- myList[[i]] else
        list3[[i]] <- patternList[[i]]
  
  return(list3)
}

.getResume <- function(typePlot) {
  formulaVector <- NULL
  for(i in names(diagPlots[[typePlot]]))
  {
    if(class(diagPlots[[typePlot]][[i]]) == "list")
    {
      result <- c(name = i, type = "List of plots")
    }else
    {
      result <- c(name = i, type = "Single plot")
    }
    
    formulaVector <- rbind(formulaVector, result)
  }
  
  return(formulaVector)
}

.getPath <- function(path)
{
  firstChar <- substr(path, 1, 1)
  
  if(firstChar == "/" | firstChar == "")
    path <- file.path(getwd())
  
  return(path)
}

.getPath2 <- function(pattern, target)
{
  output <- list.files(recursive = TRUE, pattern = pattern)
  output <- output[grep(x = output, pattern = target)]
  
  return(output)
}