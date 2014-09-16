.combineModels <- function(...){
  modelList <- deparse(substitute(list(...)))
  modelList <- substr(modelList, start = 6, stop = nchar(modelList) - 1)
  modelList <- unlist(strsplit(x = modelList, split = ", "))
  
  # Remove repeated models from modelList 
  modIndex <- NULL
  for(i in modelList)
    modIndex <- c(modIndex, get(i)$info$model)
  
  modelNames <- modIndex[!duplicated(modIndex)]
  modelList <- modelList[!duplicated(modIndex)]
  
    
  # Models
  models <- list()
  for(i in modelList)
    models[[i]] <- get(i)[c("output", "data")]
  names(models) <- modelNames
  
  # Combined results
  # Combine plots
  combined <- list()
  
  allOutputs <- list()
  allYPR <- list()
  allData <- list()
  for(i in modelList)
  {
    allOutputs[[i]] <- get(i)$output$output
    allYPR[[i]]     <- get(i)$output$YPR
    allData[[i]]    <- get(i)$data$data  
  }
  names(allOutputs) <- names(allYPR) <- names(allData) <- modelNames
  
  combined <- list(outputs = allOutputs, YPR = allYPR, data = allData)
    
  output <- list(info = modelNames, data = models, combined = combined)
  
  class(output) <- c("jjm.lstOuts", class(output))
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
  
  cat("\nList of models:\n\n")
  print(x$info, ...)
  
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