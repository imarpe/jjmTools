
print.jjm.diag = function(x, ...) {
  
  cat("Model name:\n", x$info$model, "\n\n")
  cat("Input Plots:\n", paste(x$info$input, collapse = "\n "), "\n\n")
  cat("Fit Plots:\n", paste(x$info$fit, collapse = "\n "), "\n\n")
  cat("Projections Plots:\n", paste(x$info$projections, collapse = "\n "), "\n\n")
  cat("YPR Plots:\n", paste(x$info$ypr, collapse = "\n "), "\n")
  
  return(invisible())  
}

summary.jjm.diag = function(object,...) {
  
  namesPlots <- names(object$info)[-1]
  
  output <- lapply(namesPlots, .getResume, object = object)
  
  names(output) <- namesPlots
  
  class(output) <- "summary.jjm.diag"
  
  return(output)  
}

print.summary.jjm.diag <- function(x, ...) {
  
  class(x) <- "list"
  print(x, ...)
  
  return(invisible())
}

plot.jjm.diag <- function(x, what = c("input", "fit", "projections", "ypr"), ...)
{
  what <- tolower(what)
  
  if(!all(!is.na(match(what, c("input", "fit", "projections", "ypr")))))
    stop("Incorrect values for parameter 'what'.")
  
  for(i in what)
    print(x[[i]])
  
  return(invisible())
}