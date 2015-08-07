

print.jjm.diag = function(x, ...) {
  
  cat("Model name (s):\n")
  
  for(i in seq_along(x)) {
  
  obj = x[[i]]
  
  for(j in seq_along(obj)){
    
    OBJ = obj[[j]]  
  
    }
  
  cat(OBJ$info$model, "\n")
  
  }
  cat("\n")
  
#   cat("Input Plots:\n", paste(x[[1]]$info$data, collapse = "\n "), "\n\n")
#   cat("Output Plots:\n", paste(x[[1]]$info$output, collapse = "\n "), "\n\n")
  cat("Input Plots:\n", paste(OBJ$info$data, collapse = "\n "), "\n\n")
  cat("Output Plots:\n", paste(OBJ$info$output, collapse = "\n "), "\n\n")

  return(invisible())  
}

summary.jjm.diag = function(object,...) {
  
  namesPlots = names(object[[1]]$info)[-1]
  
  output = lapply(namesPlots, .getResume, object = object[[1]])
  
  names(output) = namesPlots
  
  class(output) = "summary.jjm.diag"
  
  return(output)  
}

print.summary.jjm.diag = function(x, ...) {
  
  class(x) = "list"
  
  cat("\nDetailed Input Plots:\n\n")
  
  print(x[[1]], ...)
  
  cat("\nDetailed Output Plots:\n\n")
  
  print(x[[2]], ...)
  
  return(invisible())
}

.plotDiagVar = function(x, fleet=NULL, plot=TRUE, ...) {
  if(!is.null(fleet) & all(fleet %in% names(x))) x = x[fleet]
  if(inherits(x, "trellis")) {
    x = update(x, ...) 
    if(isTRUE(plot)) print(x) 
    } else x = lapply(x, FUN=.plotDiagVar, fleet=NULL, plot=plot, ...)
  
  out = if(isTRUE(plot)) NULL else x
  
  return(invisible(out))
}

.plotDiag = function(x, var=NULL, fleet=NULL, plot=TRUE, ...) {
  
  if(is.null(var)) var = names(x)
  
  indVar = var %in% names(x)

  if(any(!indVar)) {
    msg = if(sum(!indVar)==1) 
      paste("Variable", sQuote(var[!indVar]), "does not exist.") else
        paste("Variables", sQuote(var[!indVar]), "do not exist.")
    stop(msg)
  }
  
  if(isTRUE(plot)) {
    for(ivar in var) .plotDiagVar(x[[ivar]], fleet=fleet, plot=plot, ...)
    return(invisible())
  }
  
  out = list()
  
  for(i in seq_along(var)) 
    out[[i]] = .plotDiagVar(x[[var[i]]], fleet=fleet, plot=plot, ...)
   
  return(invisible(out))
  
}


plot.jjm.diag = function(x, what = c("data", "output"), model=NULL, stock=NULL, 
                         var=NULL, fleet=NULL, plot=TRUE, ...) {
  what = tolower(what)
  
  if(any(grepl(pattern = "input", x=what))) {
    message("Parameter what='input' is deprecated, use 'data' instead.")
    what = gsub(pattern = "input", replacement = "data", x=what)
  }
  
  if(any(is.na(match(what, c("data", "output")))))
    stop("Incorrect values for parameter 'what'.")

  if(is.null(model)) model = names(x)
  stocks = stock

  if(isTRUE(plot)) {
    
    for(imodel in model) {
      if(is.null(stock)) stocks = names(x[[imodel]])
      for(istock in stocks)
        for(i in what)
          .plotDiag(x=x[[imodel]][[istock]][[i]], var=var, fleet=fleet, 
                    plot=plot, ...)
      
    }
    
    return(invisible())
    
  } else {
    
    out = list()
    j = 1
    
    for(imodel in model) {
      if(is.null(stock)) stocks = names(x[[imodel]])
      for(istock in stocks)
        for(i in what) {
          out[j] = .plotDiag(x=x[[imodel]][[istock]][[i]], var=var, fleet=fleet, 
                             plot=plot, ...)
          j = j+1
        }
    }
    
    return(invisible(out))
    
  }
  
}
