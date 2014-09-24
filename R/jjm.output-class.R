
.getJjmOutput <- function(path, model, ...) {
  
  # Define path of input
  inputPath   <- path
  
  # Set files .rep and .yld
  output  <- .getPath2(pattern = "_R.rep", target = model, path = path)
  ypr     <- .getPath2(pattern = ".yld", target = model, path = path)
  
  # Verify if files exist
  necesaryFiles <- c(paste0(model, ".ctl"), output, ypr)
  necesaryFiles <- file.path(inputPath, necesaryFiles)
  
  for(i in necesaryFiles)
    if(!file.exists(i))
      stop(paste0("File", i, " doesn't exist, please check the name or the path."))
  
  # Read files .rep and .yld
  output      <- readList(file.path(inputPath, output))
  ypr         <- .readYPR(file.path(inputPath, ypr))
  
  # Extract asociated .dat file
  dataName    <- scan(file = file.path(inputPath, paste0(model, ".ctl")), what = character(), sep = "\n", quiet = TRUE)
  modelName   <- gsub(x = dataName[2], pattern = " ", replacement = "")
  dataName    <- gsub(x = dataName[1], pattern = " ", replacement = "")
  
  # Read .dat file
  data        <- .read.dat(filename = file.path(inputPath, dataName))
  
  # Generate extra info
  iFilename   <- file.path(inputPath, dataName)
  info.data   <- list(file = iFilename, variables = length(names(data)), year=c(data$years[1], data$years[2]),
                      age = c(data$ages[1], data$ages[2]), length = c(data$lengths[1], data$lengths[2]))
  info.output <- list(model = modelName, fisheryNames = output$Fshry_names, modelYears = output$Yr,
                      indexModel = output$Index_names)
  
  # Group in a list
  output <- list(info = list(model = modelName),
                 output = list(info = info.output, output = output, YPR = ypr),
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
  
  cat("\nModel:\n\n")
  print(x$info$model, ...)
  
  cat("\nFishyNames:\n\n")
  print(x$info$fishyNames, ...)
  
  cat("\nYears:\n\n")
  print(x$info$modelYears, ...)
  
  cat("\nIndex Model:\n\n")
  print(x$info$indexModel, ...)
  
  cat("\nLikelihood Table:\n\n")
  print(x$like, ...)
  
  cat("\nFuture SSB and SD:\n\n")
  print(x$fut, ...)
  
  cat("\nSSB and SD:\n\n")
  print(x$SSB, ...)
  
  return(invisible(x))
}

# plot.jjm.output <- function(x, jjm.data, type = 'all',what = c("fit"), ...) {
# 
#   switch(type,
#          all = diagnostics(jjm.out = x$data, jjm.in = jjm.data$data, jjm.ypr = jjm.ypr, what = what),
#          
#          stop("Plot type not defined."))
#   
#   return(invisible())
# }