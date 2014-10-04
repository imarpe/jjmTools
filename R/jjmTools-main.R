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
readJJM <- function(model, path = "", modelName=model, ...) {
  
  path <- .getPath(path)
  
  output <- .getJjmOutput(path = path, model = modelName, ...)      
  
  if(length(modelName) > 1)
    warning("The condition has length > 1 and only the first element will be used")
  
  return(output)
}


# Run JJM model -----------------------------------------------------------


#' @title Run a JJM model
#' @description Function to run one or several JJM models
#' @param models String with the name of the models to be run.
#' @param path Directory where the 'admb' folder is located.
#' @param ... Arguments passed from \code{system} function.
#' @examples
#' model = runJJM(models = "mod2.4")
runJJM = function(models, path = "", output="arc", useGuess=FALSE, 
                  guess=NULL, iprint=100, wait = TRUE, ...)
{
  path = .getPath(path)
  # Set working directory in /admb directory to run model
  oldwd = getwd()
  setwd(path)
  on.exit(setwd(oldwd))
  
  models = .checkModels(models)
  guess  = .checkGuess(models, guess) # to be updated
  
  # Set lower case for model name and filter repeated names
  
  # Run models
  for(model in models)
    .runJJM(model=model, output=output, useGuess=useGuess, 
            guess=guess, iprint=iprint, wait=wait, ...)  
  
  return(invisible())
}

# Diagnostics -------------------------------------------------------------

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

# Combine models ----------------------------------------------------------
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

# Combine stocks ----------------------------------------------------------
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
#' combinedMod_123 <- combineStocks(mod1, mod2, mod3, modelName = NULL)


combineStocks = function(..., modelName = NULL){
  
  output = .resultCombined(..., modelName = modelName)
  
  return(output)
  
}