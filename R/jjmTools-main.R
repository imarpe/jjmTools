# To define generic classes and methods for the package

readJJM = function(path, type="output", ...) {
  
  out = switch(type,
               output = .getJjmOutput(path=path, ...),
               data    = .getJjmData(path=path, ...),
               stop("Invalid type.")
  )
  return(output)
}


diagnostics = function(...) {
  # 
  class(output) = c("jjm.diag", class(output))
  return(output)
}



