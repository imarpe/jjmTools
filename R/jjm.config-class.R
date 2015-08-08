
print.jjm.config = function(x, ...) {
  
  return(invisible())
}

summary.jjm.config = function(object,...) {
  
  output = NULL
  
  class(output) = "summary.jjm.config"
  
  return(output)  
}

print.summary.jjm.config = function(x, ...) {
  
  return(invisible())
}
