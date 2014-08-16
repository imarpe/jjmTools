
.readJjmOutput = function(...) {
  class(output) = c("jjm.output", class(output))
  return(output)
}


print.jjm.output = function(x, ...) {
  

  return(invisible())
  
}

summary.jjm.output = function(object,...) {
  

  class(output) = "summary.jjm.output"
  return(output)
  
}

print.summary.jjm.output = function(x, ...) {
  

  
  return(invisible(x))
}

plot.jjm.output = function(x, ,...) {
  

  
  return(invisible())
}