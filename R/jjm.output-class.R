
.getJjmOutput <- function(path = path, model = model, ...) {
  
  # Define path of input
  inputPath   <- path
  
  # Set files .rep and .yld
  output  <- .getPath2(pattern = "_R.rep", target = model, path = path)
  ypr     <- .getPath2(pattern = ".yld", target = model, path = path)
  
  # Read files .rep and .yld
  output      <- readList(file.path(inputPath, output))
  ypr         <- .readYPR(file.path(inputPath, ypr))
  
  # Extract asociated .dat file
  dataName    <- scan(file = file.path(inputPath, paste0(model, ".ctl")), what = character(), sep = "\n", quiet = TRUE)
  dataName    <- gsub(x = dataName[1], pattern = " ", replacement = "")
  
  # Read .dat file
  data        <- .read.dat(iPath = inputPath, iFilename = dataName)
  
  # Generate extra info
  iFilename   <- file.path(inputPath, dataName)
  info.data   <- list(file = iFilename, variables = length(names(output)), year=c(output$years[1], output$years[2]),
                      age = c(output$ages[1], output$ages[2]), length = c(output$lengths[1], output$lengths[2]))
  info.output <- list(model = model, fisheryNames = output$Fshry_names, modelYears = output$Yr,
                      indexModel = output$Index_names)
  
  # Group in a list
  output <- list(info = list(model = model),
                 output = list(info = info.output, output = output, YPR = ypr),
                 data = list(info = info.data, data = data))
  
  # Define jjm.output class
  class(output) <- c("jjm.output", class(output))
  
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
  
  #  x2        <- x
  #  class(x2) <- 'jjm.output'
  
  #  print(x2, ...)
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