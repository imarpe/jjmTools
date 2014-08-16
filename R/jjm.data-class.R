
.readJjmData = function(...) {
  class(output) = c("jjm.data", class(output))
  return(output)
}

print.jjm.data = function(x, ...) {
  

  return(invisible())
  
}

summary.jjm.data = function(object,...) {
  

  class(output) = "summary.jjm.data"
  return(output)
  
}

print.summary.jjm.data = function(x, ...) {
  

  
  return(invisible(x))
}

plot.jjm.data = function(x, ,...) {
  

  
  return(invisible())
}
