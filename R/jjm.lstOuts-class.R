
.getJjmOutputS <- function(path, listName){
  
  compareList <- sapply(listName, .getPath2, pattern = "_R.rep", path = path)
  
  lstOuts <- list()
  for(i in seq_along(compareList))
    lstOuts[[i]] <- readList(file.path(path, compareList[i]))
  
  # Assign names to each model in lstOuts
  names(lstOuts) <- listName
  info <- list(model = listName)
  output <- list(data = lstOuts, info = info)
  
  class(output) <- c("jjm.lstOuts", class(output))
  return(output)
}

print.jjm.lstOuts <- function(x, ...) {
  cat("\nOutput list from:\n\n")
  print(x$info$model, ...)
  
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
  
  cat("\nList of models:\n\n")
  print(x$info$model, ...)
  
  cat("\nLikelihood Table:\n\n")
  print(x$like, ...)
  
  cat("\nFuture SSB and SD:\n\n")
  print(x$fut, ...)
  
  cat("\nSSB and SD:\n\n")
  print(x$SSB, ...)
  
  return(invisible(x))
}

plot.lstOuts <- function(lstObject, comparisonType = "time", Slot = NULL, SD = FALSE, Sum = NULL, startYear = NULL,
                         legendPos = "topright", YrInd = FALSE, Apply = "mean", ...){
  if(comparisonType == "time")
  {
    if(is.null(Slot))
      Slot <- "TotBiom"
    
   .compareTime(lstObject, Slot = Slot, SD = SD, Sum = Sum, startYear = startYear,
                legendPos = legendPos, ...)
  }else if(comparisonType == "matrix")
  {
    if(is.null(Slot)) 
      Slot <- "TotF"
    
   .compareMatrix(lstObject$data , Slot, Sum, YrInd, Apply, startYear, legendPos, ...)
  }else
    stop("Incorrect value for 'comparisonType'.")  
}