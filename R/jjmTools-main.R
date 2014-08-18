# To define generic classes and methods for the package
#.readJjmOutput = function(outputPath, modelName, ...)
readJJM <- function(path, modelName, type="output", ...) {

  if(length(modelName) > 1) 
    type <- "lstOuts"
  
  output <- switch(type,
                   output   = .getJjmOutput(path = path, model = modelName, ...),
                   data     = .getJjmData(path = path, model = modelName, ...),
                   lstOuts  = .getJjmOutputS(path = path, listName = modelName),
                   
                   stop("Invalid type."))
  
  return(output)
}


diagnostics <- function(inputObject, outputObject, what, ...) {
  
  output <- .diagnostics(jjm.out = outputObject$out, jjm.in = inputObject$data,
                         jjm.ypr = outputObject$YPR, what = what)
  
  class(output) = c("jjm.diag", class(output))
  return(output)
}


compareModels <- function(lstObject, outputFilename, plotType = "pdf", comparisonType = "time",
                          comparisonParams, ...) {
  
  .compareModels(lstObject = lstObject, outputFilename = outputFilename, plotType = plotType,
                 comparisonType = comparisonType, comparisonParams = comparisonParams, ...)
  
  return(invisible())
}

