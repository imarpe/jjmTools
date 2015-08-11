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
#' @author Ricardo Oliveros-Ramos, Wencheng Lau-Medrano, Giancarlo Moron 
#' Josymar Torrejon and Niels Hintzen
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
#' @export
readJJM = function(model, path = "", output="arc", ...) {
  
  path = .getPath(path)
  output = .getJjmOutput(path=path, output=output, model=model, ...)      
    
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
#' @export
runJJM = function(models, ...) {
  UseMethod("runJJM")
}


# Diagnostics -------------------------------------------------------------

#' @title Generate Assessment plots from single model
#' @description Function to generate plots from results of readJJM function
#' @param outputObject Object ob class outputs.
#' @param ... Extra arguments
#' @examples
#' model <- readJJM(modelName = "mod2.4")
#' diagnostics(outputObject = model)
#' @export
diagnostics = function(outputObject, ...) {
  
  # Take an output object and get diagnostic plots extracting outputs, data and YPR
  output = list()
  
  for(i in seq_along(outputObject)) {
     
    jjmStocks = outputObject[[i]]$output
    version = outputObject[[i]]$info$data$version
	
    output[[i]] = list()
    
    for(j in seq_along(jjmStocks)) {
	
		if(version != "2015MS")	{
			outputObject[[i]]$data$wt_temp = outputObject[[i]]$data$Pwtatage[,1]
			outputObject[[i]]$data$mt_temp = outputObject[[i]]$data$Pmatatage[,1]
			toJjm.in = outputObject[[i]]$data
		} else {
			outputObject[[i]]$control$wt_temp = t(outputObject[[i]]$control$Pwtatage)[,j]
			outputObject[[i]]$control$mt_temp = t(outputObject[[i]]$control$Pmatatage)[,j]
			toJjm.in = c(outputObject[[i]]$data, outputObject[[i]]$control)
		}
	  
      output[[i]][[j]] = .diagnostics(jjm.info = outputObject[[i]]$info$output,
                                  jjm.out  = jjmStocks[[j]], 
                                  jjm.in   = toJjm.in, ...)
      
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
#' mod_123 = combineModels(mod1, mod2, mod3)
#' @export
combineModels = function(...)
{
  output = .combineModels(...)
  
  return(output)
}


# Read external files ---------------------------------------------------------------

readExFiles = function(fileName, type, version = "2015MS", parameters = FALSE,  
					   parData, nameFishery, nameIndex, nAges, nStock = NULL){
	
	if( type != "data" & type != "control") stop("File must be data or control type")
	if(is.null(nStock)) stop("The number of stocks is necessary")
	
	if(type == "data"){
		outList = .read.datEx(filename = fileName, version = version)
	}
	
	if(type == "control"){
		if(parameters){
			info = list()
			info$fisheryNames = .splitPor(parData$nameFish)
			info$indexModel = .splitPor(parData$nameIndex)
			info$nStock = nStock
			infoDat = list()
			infoDat$age = c(1, parData$LastAge)
		} 
		if(!parameters){
			info = list()
			info$fisheryNames = nameFishery
			info$indexModel = nameIndex
			info$nStock = nStock
			infoDat = list()
			infoDat$age = c(1, nAges)
		}
			
		if(version != "2015MS"){ 
			outList = .read.ctl(filename = fileName, info = info, infoDat = infoDat)
			} else {
			outList = .read.ctlMS(filename = fileName, info = info, infoDat = infoDat)
			}
	}
	
	return(outList)
	
}


# Write jjm files ---------------------------------------------------------------

writeJJM = function(object, outFile){
		
	.writeFiles(object = object, outFile = outFile)
	
	return(invisible(NULL))
	
}
