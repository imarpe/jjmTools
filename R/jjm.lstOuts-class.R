.combineModels <- function(...){
  modelList <- list(...)
  
  # Verify if the models are jjm.outputs objects
  for(i in modelList)
    if(class(i) != "jjm.output")
      stop("Objects must be of class 'jjm.output'.")

  # Remove repeated models from modelList 
  modelList2 <- modelList
  for(i in seq_along(modelList[-length(modelList)]))
    for(j in seq(from = i + 1, to = length(modelList)))
      if(identical(modelList[[i]], modelList[[j]]))
        modelList2[[j]] <- NULL
  
  modelList <- modelList2; rm("modelList2")
  
  modelNames <- NULL
  for(i in modelList)
    modelNames <- c(modelNames, i$info$model)  
    
  # Models
  models <- list()
  for(i in seq_along(modelList))
    models[[i]] <- modelList[[i]][c("output", "data")]
  names(models) <- modelNames
  
  # Combined results
  # Combine plots
  combined <- list()
  
  allOutputs <- list()
  allYPR <- list()
  allData <- list()
  for(i in seq_along(modelList))
  {
    allOutputs[[i]] <- modelList[[i]]$output$output
    allYPR[[i]]     <- modelList[[i]]$output$YPR
    allData[[i]]    <- modelList[[i]]$data$data  
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

plot.jjm.lstOuts <- function(lstObject, comparisonType = "time", Slot = NULL, SD = FALSE, Sum = NULL, startYear = NULL,
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
    
   .compareMatrix(lstObject , Slot = Slot, Sum = Sum, YrInd = YrInd, Apply = Apply, 
                  startYear = startYear, legendPos = legendPos, ...)
  }else
    stop("Incorrect value for 'comparisonType'.")
  
  return(invisible())
}