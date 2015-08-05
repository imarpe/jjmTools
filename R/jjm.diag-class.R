
print.jjm.diag = function(x, ...) {
  
  cat("Model name (s):\n")
  
  for(i in seq_along(x)) {
  
  obj = x[[i]]
  
  cat(obj$info$model, "\n")
  
  }
  cat("\n")
  
  cat("Input Plots:\n", paste(x[[1]]$info$data, collapse = "\n "), "\n\n")
  cat("Output Plots:\n", paste(x[[1]]$info$output, collapse = "\n "), "\n\n")
 # cat("Projections Plots:\n", paste(x$info$projections, collapse = "\n "), "\n\n")
 # cat("YPR Plots:\n", paste(x$info$ypr, collapse = "\n "), "\n")
  
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

.plotDiagVar = function(x, var, fleet=NULL, ...) {
  x = x[[var]]
  if(!is.null(fleet) & all(fleet %in% names(x))) x = x[fleet]
  if(inherits(x, "trellis")) print(update(x, ...)) else print(x)
  return(invisible())
}

.plotDiag = function(x, var=NULL, fleet=NULL, ...) {
  
  if(is.null(var)) var = names(x)
  
  indVar = var %in% names(x)

  if(any(!indVar)) {
    msg = if(sum(!indVar)==1) 
      paste("Variable", sQuote(var[!indVar]), "does not exist.") else
        paste("Variables", sQuote(var[!indVar]), "do not exist.")
    stop(msg)
  }
  
  for(ivar in var) .plotDiagVar(x, var=ivar, fleet=fleet, ...)
   
  return(invisible())
}


plot.jjm.diag = function(x, what = c("data", "output"), pdf = FALSE, file = NULL, ...) 
                          #var=NULL, fleet=NULL, ...)
{
  what = tolower(what)
  
  if(any(grepl(pattern = "input", x=what))) {
    message("Parameter what='input' is deprecated, use 'data' instead.")
    gsub(pattern = "input", replacement = "data", x=what)
  }
  
  if(any(is.na(match(what, c("data", "output")))))
    stop("Incorrect values for parameter 'what'.")

  if(isTRUE(pdf)) {

    for(j in seq_along(x)) {
      file = if(is.null(file)) paste0("Plots_", x[[j]]$info$model, ".pdf") else file
      pdf(file = paste0(file, ".pdf"), ...)
      for(i in what) print(x[[j]][[i]])
      dev.off()
      }
    } else { 
      
      for(model in names(x))
        for(stock in names(x[[model]]))
          for(i in what)
            .plotDiag(x=x[[model]][[stock]][[i]], var=var, fleet=fleet, ...)
    }
  
   return(invisible())
}
