# jjmTools package Rd file ------------------------------------------------
#' Tools to process and get results from Joint Jack Mackerel (JJM) model outputs.
#' 
#' Graphics and diagnostics tools for SPRFMO's Joint Jack Mackerel model.
#' 
#' \tabular{ll}{ Package: \tab jjmTools\cr Type: \tab Package\cr Version: \tab
#' 1.0\cr Date: \tab 2014-08-15\cr License: \tab TBD\cr }
#' 
#' @name jjmTools-package
#' @aliases jjmTools-package jjmTools
#' @docType package
#' @author Ricardo Oliveros-Ramos, Wencheng Lau-Medrano, Giancarlo Moron and
#' Niels Hintzen
#' @seealso Joint Jack Mackerel Repository
#' \code{\link{https://github.com/SPRFMO/jack_mackerel}}
#' @keywords jjmTools
#' 
#' 
NULL

# readJJM function --------------------------------------------------------

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
  
  output <- .getJjmOutput(path = path, model = modelName, ...)      
  
  if(length(modelName) > 1)
    warning("The condition has length > 1 and only the first element will be used")
  
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

#' @title Run a JJM model
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
    system(paste("./run", i), wait = wait, ...)
  
  # Back to previous working directory
  setwd(oldWD)
  
  return(invisible())
}

# .runJJM = function(model, wait, ...) {
#   return(invisible())
# }

#' @title Combine outputs
#' @description This function takes model objects (class \code{outputs}) of JJM and generate an object 
#' with combined models.
#' @param ... One or more output objects, to be combined to list of models.
#' @examples
#' mod1 <- runJJM(modelName = "mod2.1")
#' mod2 <- runJJM(modelName = "mod2.2")
#' mod3 <- runJJM(modelName = "mod2.3")
#' 
#' mod_123 <- combineModels(mod1, mod2, mod3)
combineModels <- function(...)
{
  output <- .combineModels(...)
  
  return(output)
}

#' @title combineStocks
#' @description This function takes model objects (class \code{outputs}) of JJM and generate a model
#' with SD and mean combined.
#' @param ... One or more output objects, to be combined to list of models.
#' @param model Name for new model. If \code{NULL} (default) a temporal name will be used.
#' @examples
#' mod1 <- runJJM(modelName = "mod2.1")
#' mod2 <- runJJM(modelName = "mod2.2")
#' mod3 <- runJJM(modelName = "mod2.3")
#' 
#' combinedMod_123 <- combineStocks(mod1, mod2, mod3, model = "mod_123")
combineStocks <- function(..., model = NULL)
{
  output <- .combineStocks(..., model = model)
  
  return(output)
}