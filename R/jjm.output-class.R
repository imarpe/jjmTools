
.getJjmOutput = function(path, output, model, ...) {
  
  # Define path of input
  inputPath   = path
  
  # Set files .rep and .yld
  outputs = file.path(output, paste0(model, "_R.rep")) 
  ypr     = file.path(output, paste0(model, ".yld"))
    
  # Verify if files exist
  necesaryFiles = c(paste0(model, ".ctl"), outputs)
  necesaryFiles = file.path(inputPath, necesaryFiles)
  
  for(ifile in necesaryFiles)
    if(!file.exists(ifile))
      stop(paste0("File", ifile, " doesn't exist, please check the name or the path."))
  
  # Read files .rep and .yld
  outputs = readList(file.path(inputPath, outputs))
  ypr     = .readYPR(file.path(inputPath, ypr))
  outputs$YPR = ypr
  
  # Extract asociated .dat file
  dataName    = scan(file = file.path(inputPath, paste0(model, ".ctl")), nlines = 2, 
                      what = character(), sep = "\n", quiet = TRUE)
  modelName   = gsub(x = dataName[2], pattern = " ", replacement = "")
  dataName    = gsub(x = dataName[1], pattern = " ", replacement = "")
  
  # Read .dat file
  data        = .read.dat(filename = file.path(inputPath, dataName))
  
  # Generate extra info
  iFilename   = file.path(inputPath, dataName)
  info.data   = list(file = iFilename, variables = length(names(data)), year=c(data$years[1], data$years[2]),
                      age = c(data$ages[1], data$ages[2]), length = c(data$lengths[1], data$lengths[2]))
  info.output = list(model = modelName, fisheryNames = outputs$Fshry_names, modelYears = outputs$Yr,
                                              indexModel = outputs$Index_names)

  output = list()												
  # Group in a list
  output[[1]]   = list(output = outputs,
                      data = data,
                      info = list(data = info.data, output = info.output))
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
  #cat("Projection years number: ", paste(length(obj$output$SSB_fut_1), collapse = ", "), "\n")
  cat(" ", "\n")
  
  }
  
  return(invisible())
  
}

summary.jjm.output = function(object, Projections = FALSE, Fmult = c(0, 0.5, 0.75, 1, 1.25),
					BiomProj = c(2015, 2020, 2024), 
					CapProj = c(2015, 2016), 
					MRS = 4500, ...) {
  
  pic = list()
  namesPlot = NULL
  for(i in seq_along(object)){
  
  jjm.out <- object[[i]]$output
  jjm.in  <- object[[i]]$data
  jjm.ypr <- object[[i]]$output$YPR
  namesPlot[i] = object[[i]]$info$output$model
  
  pic[[i]] = .fit_summarySheetFUN(jjm.out, scales = list(alternating = 1, 
						y = list(relation = "free", rot = 0),
                        axs = "i"), ...)
  
  }
  
  names(pic) = namesPlot

  output = list()
  output$like <- .LikeTable(object)
  output$projections <- .ProjTable(object, Projections = Projections, 
						  Fmult = Fmult, BiomProj = BiomProj, CapProj = CapProj, MRS = MRS)
  output$plots <- pic
  
  class(output) <- "summary.jjm.output"

  return(output)
  
}

print.summary.jjm.output = function(x, ...) {
  
  #cat("\nModel:\n", x$info$model, "\n\n")
  
  #cat("\nFishyNames:\n", paste(x$info$fisheryNames, collapse = ", "), "\n\n")
  
  #cat("\nYears:\n", paste(range(x$info$modelYears), collapse = "-"), "\n\n")
  
  #cat("\nModel's indices:\n", paste(x$info$indexModel, collapse = ", "), "\n\n")
  
  cat("\nLikelihood Table:\n\n")
  print(x$like, ...)
  
  cat("\nProjection Table (s):\n\n")

  for(i in seq_along(x$projections)){
  
	  cat(names(x$projections)[i], "\n")
	  print(x$projections[[i]], ...)
	  cat(" ", "\n")
  
  }
  
    cat("\nSummary Plot (s):\n\n")

  for(i in seq_along(x$plots)){
  
	  cat(names(x$plots)[i], "\n")
	  plot(x$plots[[i]])
  
  }
  
  #cat("\nFuture SSB and SD:\n\n")
  #print(x$fut, ...)
  
  #cat("\nSSB and SD:\n\n")
  #print(x$SSB, ...)
  
  return(invisible(x))
}


plot.jjm.output <- function(x, what = "biomass", stack = TRUE, ...){
      
  dataShape = .reshapeJJM(x, what = what)
  
  if(stack == !TRUE){
    pic = xyplot(mean ~ year, data = dataShape, groups = model, ylab = "",
                 auto.key = list(title = "", x = 0.8, y = 0.9, cex = 1.25,
                                 points = FALSE, border = FALSE, 
                                 lines = TRUE),
                 upper = dataShape$upper, lower = dataShape$lower,
                 panel = function(x, y, ...){
                   panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                   panel.xyplot(x, y, type ='l', cex = 0.6, lty = 1, lwd = 2, ...)
                 }
    )
  } else {pic = xyplot(mean ~ year | model, data = dataShape, groups = model, ylab = "",
                       upper = dataShape$upper, lower = dataShape$lower,
                       panel = function(x, y, ...){
                         panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                         panel.xyplot(x, y, type = 'l', cex = 0.6, lty = 1, lwd = 2, ...)
                       })
  }
  
  return(pic)
}


