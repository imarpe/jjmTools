
.getJjmOutput <- function(path = path, model = model, ...) {
  # Define path of input
  inputPath   <- file.path(path, "Code/admb/")
  
  # Read files .rep and .yld
  output1 <- readList(file.path(inputPath, paste("arc/", model, "_r.rep", sep = "")))
  output2 <- .readYPR(file.path(inputPath, paste( "arc/", modelName,".yld", sep = "")))
  info <- list(model = model)
  
  # Group in a list
  output <- list(info = info, out = output1, YPR = output2)
  
  # Define jjm.output class
  class(output) = c("jjm.output", class(output))
  
  return(output)
}


print.jjm.output <- function(x, ...) {
  # Get name of model
  cat("Output data from ", sQuote(x$info$model), "\n", sep = "")

  return(invisible())
}

summary.jjm.output = function(object,...) {
  
  output = list()
  
  output$info <- object$info
  output$like <- .LikeTable(object)
  output$fut  <- .Fut_SSB_SD(object)
  output$SSB  <- .SSB_SD(object)
  
  class(output) <- "summary.jjm.output"
  return(output)
  
}

print.summary.jjm.output <- function(x, ...) {
  
  x2        <- x
  class(x2) <- 'jjm.output'
  
  print(x2, ...)
  
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