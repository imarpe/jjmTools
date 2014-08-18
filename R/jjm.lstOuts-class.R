
.getJjmOutputS = function(path, listName){
  compareList <- file.path(path, paste0(listName,"_R.rep"))
  lstOuts <- list()
  for(i in seq_along(compareList))
    lstOuts[[i]] <- readList(compareList[i])
  # Assign names to each model in lstOuts
  names(lstOuts) <- listName
  info = list(model = listName)
  output = list(data=lstOuts, info=info)
  class(output) = c("jjm.lstOuts", class(output))
  return(output)
}

print.jjm.lstOuts = function(x, ...) {
  #cat("Output list from ", sQuote(x$info$model), "\n", sep="")
  cat("\nOutput list from:\n\n")
  print(x$info$model, ...)
  return(invisible())
}

summary.jjm.lstOuts = function(object,...) {
  
  output = list()
  
  output$info = object$info
  
  class(output) = "summary.jjm.lstOuts"
  return(output)
  
}

print.summary.jjm.lstOuts = function(x, ...) {
  
  x2=x; class(x2)='jjm.output'
  print(x2, ...)
  
  return(invisible(x))
}