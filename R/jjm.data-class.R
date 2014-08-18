
.getJjmData = function(path=path,modelName,...) {
  
  iFilename=paste(modelName,".dat",sep="")
  out = read.dat(iPath=path,iFilename=iFilename)
  
  info = list(file=iFilename, variables=length(names(out)),
              year=c(out$years[1],out$years[2]),
              age=c(out$ages[1],out$ages[2]),
              length=c(out$lengths[1],out$lengths[2]))
  
  output = list(data=out, info=info)
  
  class(output) = c("jjm.data")
#  class(output) = c("jjm.data", class(output))
  return(output)
}

print.jjm.data = function(x, ...) {
  
  cat("jjm.data from ", sQuote(x$info$file), "\n", sep="")
  cat("Number of variables: ", x$info$variables, "\n", sep="")
  cat("Years from: ", x$info$year[1] ,"to", x$info$year[2], "\n", sep=" ")
  cat("Ages from: ", x$info$age[1] ,"to", x$info$age[2], "\n", sep=" ")
  cat("Lengths from: ", x$info$length[1] ,"to", x$info$length[2], "\n", sep=" ")
  
  return(invisible())
  
}

summary.jjm.data = function(object,...) {
 
  output = list()
  
  output$info = object$info

  class(output) = "summary.jjm.data"
  return(output)
  
}

print.summary.jjm.data = function(x, ...) {
  
  x2=x; class(x2)='jjm.data'
  print(x2,...)  

  
  return(invisible(x))
}

#plot.jjm.data = function(x, ...) {
  

  
#  return(invisible())
#}
