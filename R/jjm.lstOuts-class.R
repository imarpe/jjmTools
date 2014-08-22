
.getJjmOutputS <- function(path, listName){
  
  path <- file.path(path, "admb/arc/")
  compareList <- file.path(path, paste0(listName,"_R.rep"))
  
  lstOuts <- list()
  for(i in seq_along(compareList))
    lstOuts[[i]] <- readList(compareList[i])
  
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
  
  x2        <- x
  class(x2) <- 'jjm.output'
  
  print(x2, ...)
  
  cat("\nLikelihood Table:\n\n")
  print(x$like, ...)
  
  cat("\nFuture SSB and SD:\n\n")
  print(x$fut, ...)
  
  cat("\nSSB and SD:\n\n")
  print(x$SSB, ...)
  
  return(invisible(x))
}

plot.lstOuts <- function(lstObject, comparisonType = "time", comparisonParams, ...)
{
  .compareModels(lstObject = lstObject, comparisonType = comparisonType, comparisonParams = comparisonParams, ...)
  
  return(invisible())
}