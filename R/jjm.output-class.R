
.getJjmOutput <- function(path, output, model, ...) {
  
  # Define path of input
  inputPath   <- path
  
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
  
  # Extract asociated .dat file
  dataName    <- scan(file = file.path(inputPath, paste0(model, ".ctl")), nlines = 2, 
                      what = character(), sep = "\n", quiet = TRUE)
  modelName   <- gsub(x = dataName[2], pattern = " ", replacement = "")
  dataName    <- gsub(x = dataName[1], pattern = " ", replacement = "")
  
  # Read .dat file
  data        <- .read.dat(filename = file.path(inputPath, dataName))
  
  # Generate extra info
  iFilename   <- file.path(inputPath, dataName)
  info.data   <- list(file = iFilename, variables = length(names(data)), year=c(data$years[1], data$years[2]),
                      age = c(data$ages[1], data$ages[2]), length = c(data$lengths[1], data$lengths[2]))
  info.output <- list(model = modelName, fisheryNames = outputs$Fshry_names, modelYears = outputs$Yr,
                      indexModel = outputs$Index_names)
  
  # Group in a list
  output <- list(info = list(model = modelName),
                 output = list(info = info.output, output = outputs, YPR = ypr),
                 data = list(info = info.data, data = data))
  
  # Define jjm.output class
  class(output) <- c("jjm.output")
  
  return(output)
}

print.jjm.output <- function(x, ...) {
  
  cat("jjm.data from ", sQuote(x$data$info$file), "\n", sep = "")
  cat("Number of variables: ", x$data$info$variables, "\n", sep = "")
  cat("Years from: ", x$data$info$year[1] ,"to", x$data$info$year[2], "\n", sep = " ")
  cat("Ages from: ", x$data$info$age[1] ,"to", x$data$info$age[2], "\n", sep = " ")
  cat("Lengths from: ", x$data$info$length[1] ,"to", x$data$info$length[2], "\n", sep = " ")
  
  cat("Model name: ", x$output$info$model, "\n")
  cat("Fisheries names: ", paste(x$output$info$fisheryNames, collapse = ", "), "\n")
  cat("Asociated indices: ", paste(x$output$info$indexModel, collapse = ", "), "\n")
  
  return(invisible())
}

summary.jjm.output = function(object, ...) {
  
  output = list()
  
  output$info <- object$output$info
  output$like <- .LikeTable(object)
  output$fut  <- .Fut_SSB_SD(object)
  output$SSB  <- .SSB_SD(object)
  
  class(output) <- "summary.jjm.output"
  return(output)
  
}

print.summary.jjm.output <- function(x, ...) {
  
  cat("\nModel:\n", x$info$model, "\n\n")
  
  cat("\nFishyNames:\n", paste(x$info$fisheryNames, collapse = ", "), "\n\n")
  
  cat("\nYears:\n", paste(range(x$info$modelYears), collapse = "-"), "\n\n")
  
  cat("\nModel's indices:\n", paste(x$info$indexModel, collapse = ", "), "\n\n")
  
  cat("\nLikelihood Table:\n\n")
  print(x$like, ...)
  
  cat("\nFuture SSB and SD:\n\n")
  print(x$fut, ...)
  
  cat("\nSSB and SD:\n\n")
  print(x$SSB, ...)
  
  return(invisible(x))
}


# Kobe plot ---------------------------------------------------------------

#' @title Kobe plot
#' @description This function create a kobe plot from JJM  model outputs
#' @param model Name for new model created with the \code{readJJM} function.
#' @param Blim Limit reference point for B/B_MSY, default=0.5.
#' @param Flim Limit reference point for F/F_MSY, default=1.5.
#' @param Bref Reference point for B/B_MSY, default=1.
#' @param Fref Reference point for F/F_MSY, default=1.
#' @param xlim 'x' axis limits.
#' @param ylim 'y' axis limits.
#' @param ... Additional parameters passed to plot.
#' @examples
#' kobe(model)
kobe = function(model, Blim = 0.5, Flim = 1.5, Bref = 1, Fref = 1, 
                xlim = NULL, ylim = NULL, ...) {
  
  kob = model$output$output$msy_mt
  
  F_Fmsy = kob[,4]
  B_Bmsy = kob[,13]
  years  = kob[,1]
  
  n = length(B_Bmsy)
  
  if(is.null(xlim)) xlim= range(pretty(c(0, B_Bmsy)))
  if(is.null(ylim)) ylim= range(pretty(c(0, F_Fmsy)))
  
  plot.new()
  plot.window(xlim=xlim, ylim=ylim, xaxs="i")
  
  ylim = par()$usr[3:4]
  zero = ylim[1]
  
  
  polygon(x=c(0, 0, Bref, Bref),
          y=c(Fref, ylim[2], ylim[2], Fref),
          col=rgb(1, 165/255, 0, alpha = 0.5), border=NA)
  polygon(x=c(0, 0, Bref, Bref),
          y=c(zero, Fref, Fref, zero),
          col=rgb(1, 1, 0, alpha = 0.5), border=NA)
  polygon(x=c(Bref, Bref, xlim[2], xlim[2]),
          y=c(Fref, ylim[2], ylim[2], Fref),
          col=rgb(1, 1, 0, alpha = 0.5), border=NA)
  polygon(x=c(Bref, Bref, xlim[2], xlim[2]),
          y=c(zero, Fref, Fref, zero),
          col = rgb(0,1,0, alpha = 0.5), border=NA)
  polygon(x=c(0, 0, Blim, Blim),
          y=c(Flim, ylim[2], ylim[2], Flim),
          col=rgb(1, 0, 0, alpha = 0.5), border=NA)
  
  lines(B_Bmsy, F_Fmsy, type="b", pch=pch, cex=cex, col="black")
  points(B_Bmsy[c(1,n)], F_Fmsy[c(1,n)], pch=c(15, 17), col="blue")
  text(B_Bmsy[c(1,n)], F_Fmsy[c(1,n)], labels=range(years), cex=0.6,
       adj=-0.2)
  
  axis(1, las=1)
  axis(2, las=2)
  box()
  return(invisible())
}
