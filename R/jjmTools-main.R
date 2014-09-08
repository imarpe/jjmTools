# To define generic classes and methods for the package
#.readJjmOutput = function(outputPath, modelName, ...)
readJJM <- function(modelName, path = "", ...) {
  
  path <- .getPath(path)
  
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

runJJM <- function(modelName, path = "", wait = TRUE, ...)
{
  path <- .getPath(path)
  
  # Set working directory in /admb directory to run model
  oldWD <- getwd()
  setwd(path)
  
  # Set lower case for model name and filter repeated names
  modelName <- tolower(modelName)
  modelName <- unique(modelName)
  
  # Run models
  for(i in modelName)
    system(paste("run", i), wait = wait, ...)
  
  # Back to previous working directory
  setwd(oldWD)
  
  return(invisible())
}