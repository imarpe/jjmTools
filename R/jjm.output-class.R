
.getJjmOutput = function(path, output, model, ...) {
  
  # Define path of input
  inputPath   = path
  
  Files = list.files(path = output, 
					 pattern = paste0(model, "_[[:digit:]]_R.rep"))
  
  # Set files .rep and .yld
  outpts = NULL
  for(i in seq_along(Files)){
	temp = file.path(output, Files[i])
	outpts = c(outpts, temp) 	
  }
  
  
  yprName     = file.path(output, paste0(model, ".yld"))
    
  # Verify if files exist
  necesaryFiles = c(paste0(model, ".ctl"), outpts)
  necesaryFiles = file.path(inputPath, necesaryFiles)

  for(ifile in necesaryFiles)
    if(!file.exists(ifile))
      stop(paste0("File", ifile, " doesn't exist, please check the name or the path."))
  
  # Read files .rep and .yld
  outputs = list(length(Files))
  for(i in seq_along(Files)){
	  outputs[[i]] = readList(file.path(inputPath, outpts[i]))
	  ypr     = .readYPR(file.path(inputPath, yprName))
	  outputs[[i]]$YPR = ypr
  }
  
  # Extract asociated .dat file
  nameD = scan(file = file.path(inputPath, paste0(model, ".ctl")), nlines = 4, 
                      what = character(), sep = "\n", quiet = TRUE,
					  comment.char = "#")
  nameD = nameD[! nameD %in% ""]
  dataName    = nameD
  modelName   = gsub(x = dataName[2], pattern = " ", replacement = "")
  dataName    = gsub(x = dataName[1], pattern = " ", replacement = "")
  
  # Read .dat file
  data        = .read.dat(filename = file.path(inputPath, dataName))
  
  # Generate extra info
  iFilename   = file.path(inputPath, dataName)
  info.data   = list(file = iFilename, variables = length(names(data)), year=c(data$years[1], data$years[2]),
                      age = c(data$ages[1], data$ages[2]), length = c(data$lengths[1], data$lengths[2]))
  
  indices = NULL
  fisheries = NULL
  
  for(i in seq_along(Files)){
		tempI = outputs[[i]]$Index_names
		tempF = outputs[[i]]$Fshry_names
	    indices = c(indices, tempI)
	    fisheries = c(fisheries, tempF)
  }
  
  indices = unique(indices)
  fisheries = unique(fisheries)
  
  info.output = list(model = modelName, fisheryNames = fisheries, modelYears = outputs$Yr,
                     indexModel = indices, nStock = length(Files))
  
  namesStock = paste0("Stock_", 1:length(Files)) # Puede ser modificado cuando se lea el ctl
  names(outputs) = namesStock
  
  output = list()												
  # Group in a list
  output[[1]]   = list(info = list(data = info.data, output = info.output),
                       data = data,
                       output = outputs)
  names(output) = modelName				  
  
  # Define jjm.output class
  class(output) = c("jjm.output")
  
  return(output)
}

print.jjm.output = function(x, ...) {
  
  for(i in seq_along(x)){
  
  obj = x[[i]]
  
  cat("Model name: ", obj$info$output$model, "\n")
  cat("jjm.data from ", sQuote(obj$info$data$file), "\n", sep = "")
  cat("Number of variables: ", obj$info$data$variables, "\n", sep = "")
  cat("Years from: ", obj$info$data$year[1] ,"to", obj$info$data$year[2], "\n", sep = " ")
  cat("Ages from: ", obj$info$data$age[1] ,"to", obj$info$data$age[2], "\n", sep = " ")
  cat("Lengths from: ", obj$info$data$length[1] ,"to", obj$info$data$length[2], "\n", sep = " ")
  cat("Fisheries names: ", paste(obj$info$output$fisheryNames, collapse = ", "), "\n")
  cat("Associated indices: ", paste(obj$info$output$indexModel, collapse = ", "), "\n")
  cat("Number of Stocks: ", paste(obj$info$output$nStock, collapse = ", "), "\n")
  #cat("Projection years number: ", paste(length(obj$output$SSB_fut_1), collapse = ", "), "\n")
  cat(" ", "\n")
  
  }
  
  return(invisible())
  
}

summary.jjm.output = function(object, Projections = FALSE, Fmult = NULL,
                              BiomProj = NULL, 
                              CapProj = NULL, 
                              MRS = NULL, ...) {
  
  pic = list()
  namesPlot = NULL
  for(i in seq_along(object)){
  
  jjm.out = object[[i]]$output
  jjm.in  = object[[i]]$data
  jjm.ypr = object[[i]]$output$YPR
  namesPlot[i] = object[[i]]$info$output$model
  
  pic[[i]] = .fit_summarySheet3FUN(jjm.out, scales = list(alternating = 1,
                                            y = list(relation = "free", rot = 0),
                                            axs = "i"), ...)
  
  }
  
  names(pic) = namesPlot

  output = list()
  output$like = .LikeTable(object)
  output$projections = .ProjTable(object, Projections = Projections,
                                  Fmult = Fmult, BiomProj = BiomProj, 
                                  CapProj = CapProj, MRS = MRS)
  output$plots = pic
    
  class(output) = "summary.jjm.output"

  return(output)
  
}

print.summary.jjm.output = function(x, ...) {
  
  
  cat("\nLikelihood Table:\n\n")
  print(x$like, ...)
  
  if(!is.null(x$projections)) cat("\nProjection Table (s):\n\n")
  
  for(i in seq_along(x$projections)){
    
    cat(names(x$projections)[i], "\n")
    print(x$projections[[i]], ...)
    cat(" ", "\n")
    
  }
  
  cat("\nSummary Plot (s):\n\n")
  
  for(i in seq_along(x$plots)){
    
    cat(names(x$plots)[i], "\n")
    print(x$plots[[i]])
    
  }
  
  return(invisible(x))
}



plot.jjm.output = function(x, what = "biomass", stack = TRUE, endvalue = FALSE 
                           , cols = NULL, poslegend = "right", scen = 1, ...){
  
    switch(what, biomass     = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, ...),
               recruitment   = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, ...),
               ssb           = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, ...),
               noFishTB      = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, ...),
               ftot          = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, ...),
               kobe          = .funPlotKobe(x, what, cols, stack, endvalue, poslegend, ...),
               catchProj     = .funPlotProj(x, what, cols, stack, endvalue, poslegend, ...),
               ssbProj       = .funPlotProj(x, what, cols, stack, endvalue, poslegend, ...),
			   totalProj     = .funPlotTotProj(x, what, cols, stack, endvalue, poslegend, scen, ...),
			   catchProjScen = .funPlotScen(x, what, cols, stack, endvalue, poslegend, ...),
			   ssbProjScen   = .funPlotScen(x, what, cols, stack, endvalue, poslegend, ...),
			   ratioSSB_F    = .funPlotRatioSSB_F(x, what, cols, stack, endvalue, poslegend, ...),
			   ratioSSB      = .funPlotRatioSSB(x, what, cols, stack, endvalue, poslegend, ...))
  
}

