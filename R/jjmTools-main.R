# To define generic classes and methods for the package
#.readJjmOutput = function(outputPath, modelName, ...)
readJJM <- function(path, modelName, ...) {
    
  # Set lower case for model name
  modelName <- tolower(modelName)
  
  # Acording to the length of modelname (number of models), decide the way to process
  if(length(modelName) > 1)
    output <- .getJjmOutputS(path = path, listName = modelName) else
      output <- .getJjmOutput(path = path, model = modelName, ...)
  
  return(output)
}


diagnostics <- function(outputObject, ...) {
  
  # Take an output object and get diagnostic plots extracting outputs, data and YPR
  output <- .diagnostics(jjm.out = outputObject$output, jjm.in = outputObject$data$data,
                         jjm.ypr = outputObject$output$YPR)
  
  # Return a jjm.diag object
  class(output) = c("jjm.diag", class(output))
  return(output)
}


# compareModels <- function(lstObject, outputFilename, plotType = "pdf", comparisonType = "time",
#                           comparisonParams, ...) {
#   
#   .compareModels(lstObject = lstObject, outputFilename = outputFilename, plotType = plotType,
#                  comparisonType = comparisonType, comparisonParams = comparisonParams, ...)
#   
#   return(invisible())
# }

