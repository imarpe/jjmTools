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
#' @param model String with the name of model that will be readed or run.
#' @param path Directory where the 'admb' folder is located.
#' @param modelName alias for \code{model} (to be deprecated).
#' @param ... Extra arguments
#' @examples
#' readJJM(model = "mod2.4")
readJJM <- function(model, path = "", output="arc", modelName=model 
					, ...) {
  
  path <- .getPath(path)
  
  output <- .getJjmOutput(path = path, output=output, model = model
						  , ...)      
  
  if(length(modelName) > 1)
    warning("The condition has length > 1 and only the first element will be used")
  
  return(output)
}


# Run JJM model -----------------------------------------------------------


#' @title Run a JJM model
#' @description Function to run one or several JJM models
#' @param models String with the name of the models to be run.
#' @param path Directory where the 'admb' folder is located.
#' @param output Folder to save the outputs, 'arc' by default.
#' @param useGuess boolean, to use an initial guess for the parameters?
#' @param guess File with the initial guess for the parameters. If \code{NULL}, will use \code{model.par} in the output folder. 
#' @param iprint iprint parameter for the JJM model, 100 by default.
#' @param wait boolean, wait for the model to finish? Forced to be TRUE.
#' @param temp character, path for a temporal directory to run models, if \code{NULL} a temporal folder is automaticaly created.
#' @param ... Arguments passed from \code{system} function.
#' @examples
#' model = runJJM(models = "mod2.4")
runJJM = function(models, path = "", output="arc", useGuess=FALSE, 
                  guess=NULL, iprint=100, wait = TRUE, parallel=FALSE, 
                  temp=NULL, ...)
{
  
  path   = normalizePath(path)
  # Set working directory in /admb directory to run model
  oldwd = getwd()
  setwd(path)
  on.exit(setwd(oldwd))
  output = normalizePath(output, mustWork = FALSE)
  if(!file.exists(output)) dir.create(output)

  # Set lower case for model name and filter repeated names
  models = .checkModels(models)
  if(length(models)<1) {
    cat("No models to be run.")
    return(invisible())
  }

  guess  = .checkGuess(models, guess, output) 
  
  # Run models
  base  = getwd()
  start = proc.time() 
  
  if(is.null(temp)) temp = tempdir()
  
  if(!isTRUE(parallel)) {
    
    cat("\nRunning models", paste(models, collapse=", "), "\n")
    cat("\tStarting at", as.character(Sys.time()), "\n")
    
    res = NULL
    for(i in seq_along(models)) {
      rtime = .runJJM(model=models[i], output=output, useGuess=useGuess, 
                      guess=guess[i], iprint=iprint, wait=wait, temp=temp, ...)
      res = c(res, rtime)  
    }
    
  } else {
    
    cat("\nRunning models", paste(models, collapse=", "), "in parallel.\n")
    cat("\tStarting at", as.character(Sys.time()), "\n")
    tempDir = tempdir()
    res = foreach(i=seq_along(models), .combine=c) %dopar% {
      setwd(base)
      .runJJM(model=models[i], output=output, useGuess=useGuess, 
              guess=guess[i], iprint=iprint, wait=wait, temp=temp, ...)  
    }  
    
  }
  
  setwd(base)
  cat("\tEnded at", as.character(Sys.time()), "\n")
  
  elapsed = proc.time() - start
  names(res) = models
  cat("\nModel runs finished.\nTotal models run time:\n")
  print(res)
  cat("\nEffective running time:", round(elapsed[3], 1), "s.\n")
  
  cat("\n Models were run at", temp, "folder.")
  
  return(invisible(temp))
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
  output = list()
  
  for(i in seq_along(outputObject)) {
     
    jjmStocks = outputObject[[i]]$output
    
    output[[i]] = list()
    
    for(j in seq_along(jjmStocks)) {
      
      output[[i]][[j]] = .diagnostics(jjm.info = outputObject[[i]]$info$output,
                                  jjm.out  = jjmStocks[[j]], 
                                  jjm.in   = outputObject[[i]]$data, ...)
      
    }
    
    names(output[[i]]) = names(jjmStocks)
    
  }
  
  names(output) = names(outputObject)
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
