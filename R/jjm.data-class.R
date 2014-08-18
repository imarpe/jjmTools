
.getJjmData <- function(path = path, model = model, ...) {
  
  # Define path of input files
  inputPath   <- file.path(path, "Code/admb/")
  
  # Extract asociated .dat file
  dataName    <- scan(file = file.path(inputPath, paste0(model, ".ctl")), what = character(), sep = "\n")
  dataName    <- gsub(x = dataName[1], pattern = " ", replacement = "")
  
  # Read .dat file
  output      <- .read.dat(iPath = inputPath, iFilename = dataName)
  
  # Generate extra info
  iFilename   <- file.path(inputPath, dataName)
  info        <- list(file = iFilename, variables = length(names(output)), year=c(output$years[1], output$years[2]),
                      age = c(output$ages[1], output$ages[2]), length = c(output$lengths[1], output$lengths[2]))
  
  # Group in a list
  output      <- list(data = output, info = info)
  
  # Define jjm.data class
  class(output) <- c("jjm.data", class(output))
  
  return(output)
}

print.jjm.data = function(x, ...) {
  
  cat("jjm.data from ", sQuote(x$info$file), "\n", sep = "")
  cat("Number of variables: ", x$info$variables, "\n", sep = "")
  cat("Years from: ", x$info$year[1] ,"to", x$info$year[2], "\n", sep = " ")
  cat("Ages from: ", x$info$age[1] ,"to", x$info$age[2], "\n", sep = " ")
  cat("Lengths from: ", x$info$length[1] ,"to", x$info$length[2], "\n", sep = " ")
  
  return(invisible())
  
}

summary.jjm.data = function(object,...) {
 
  output <- list()
  
  output$info <- object$info

  class(output) <- "summary.jjm.data"
  
  return(output)  
}

print.summary.jjm.data <- function(x, ...) {
  
  x2 <- x
  class(x2) <- 'jjm.data'
  
  print(x2, ...)  
  
  return(invisible(x))
}

#plot.jjm.data = function(x, ...) {
  

  
#  return(invisible())
#}
