#' @title Read a model or list of models
#' @description Function to read models and list if models and generate results
#' @param modelName String with the name of model that will be readed or run.
#' @param path Directory where the 'admb' folder is located.
#' @param ... Extra arguments
#' @examples
#' readJJM(modelName = "mod2.4")
#' readJJM(modelName = paste0("mod2.", 1:4))
readJJM <- function(modelName, path = "", ...) {
  
  path <- .getPath(path)
  
  # Set lower case for model name
  modelName <- tolower(modelName)
  
  # Acording to the length of modelname (number of models), decide the way to process
  if(length(modelName) > 1)
    output <- .getJjmOutputS(path = path, listName = modelName, ...) else
      output <- .getJjmOutput(path = path, model = modelName, ...)
  
  return(output)
}

#' @title Generate Assessment plots from single model
#' @description Function to generate plots from results of readJJM function
#' @param outputObject Object ob class outputs.
#' @param ... Extra arguments
#' @examples
#' model <- readJJM(modelName = "mod2.4")
#' diagnostics(outputObject = model)
diagnostics <- function(outputObject, ...) {
  
  # Take an output object and get diagnostic plots extracting outputs, data and YPR
  output <- .diagnostics(jjm.out = outputObject$output, jjm.in = outputObject$data$data,
                         jjm.ypr = outputObject$output$YPR)
  
  # Return a jjm.diag object
  class(output) = c("jjm.diag", class(output))
  return(output)
}

#' @title Run a model
#' @description Function to run a model (o list of models) of JJM and generate inputs files
#' @param modelName String with the name of model that will be readed or run.
#' @param path Directory where the 'admb' folder is located.
#' @param ... Arguments passed from \code{system} function.
#' @examples
#' model <- runJJM(modelName = "mod2.4")
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