# To define generic classes and methods for the package
#.readJjmOutput = function(outputPath, modelName, ...)
readJJM <- function(path, modelName, ...) {

  modelName <- tolower(modelName)
  
  if(length(modelName) > 1)
    output <- .getJjmOutputS(path = path, listName = modelName) else
      output <- .getJjmOutput(path = path, model = modelName, ...)
  
  return(output)
}


diagnostics <- function(outputObject, ...) {
  
  output <- .diagnostics(jjm.out = outputObject$output, jjm.in = outputObject$data$data,
                         jjm.ypr = outputObject$output$YPR)
  
  class(output) = c("jjm.diag", class(output))
  return(output)
}


compareModels <- function(lstObject, outputFilename, plotType = "pdf", comparisonType = "time",
                          comparisonParams, ...) {
  
  .compareModels(lstObject = lstObject, outputFilename = outputFilename, plotType = plotType,
                 comparisonType = comparisonType, comparisonParams = comparisonParams, ...)
  
  return(invisible())
}

