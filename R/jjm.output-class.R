
.readJjmOutput = function(outputPath, modelName, ...) {
  data = readList(file.path(outputPath, paste(modelName,"_R.rep",sep="")))
  info = list(model = modelName)
  output = list(data=data, info=info)
  class(output) = c("jjm.output", class(output))
  return(output)
}


print.jjm.output = function(x, ...) {
  cat("Output data from ", sQuote(x$info$model), "\n", sep="")

  return(invisible())
}

summary.jjm.output = function(object,...) {
  
  output = list()
  
  output$info = object$info
  
  class(output) = "summary.jjm.output"
  return(output)
  
}

print.summary.jjm.output = function(x, ...) {
  
  x2=x; class(x2)='jjm.output'
  print(x2,...)
  
  return(invisible(x))
}

plot.jjm.output = function(x, jjm.data, what = c("fit"),...) {
  
  diagnostics(jjm.out = x$data, jjm.in = jjm.data, jjm.ypr = jjm.ypr, what = what)

  return(invisible())
}