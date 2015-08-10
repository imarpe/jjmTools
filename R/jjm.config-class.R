
.getJjmCongif = function(data, control, configName, ...) {
  
  out = list()
  out[[1]] = list()
  
  out[[1]]$Dat = data
  out[[1]]$Ctl = control
  names(out) = configName
  
  # Define jjm.output class
  class(out) = c("jjm.config")
  
  return(out)
  
}


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
