
report = function(...) {
  UseMethod("report")
}

report.jjm.output = function(object, format="latex", Fmult = NULL,
                             BiomProj = NULL, CapProj = NULL, verbose=TRUE,
                             MRS = NULL, tangle=FALSE, tidy=TRUE, ...) {
  
  if(is.null(BiomProj)) nBiom = 3
  if(!is.null(BiomProj)) nBiom = length(BiomProj)
  if(is.null(CapProj)) nCap = 2
  if(!is.null(CapProj)) nCap = length(CapProj)
  
  modelName = deparse(substitute(object))
  
  f = system.file("reports", "report-jjm.output.Rmd", package = "jjmTools")
  knit(f)
  if(isTRUE(tangle)) {
    knit(f, tangle=TRUE)
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", f)
    file.rename(from=basename(f1), to=paste0(modelName, ".R"))
  }
  f2 = gsub(pattern = ".Rmd", replacement = "\\.md", f)
  f3 = gsub(pattern = ".Rmd", replacement = "\\.pdf", f)
  pandoc(basename(f2), format=format)
  outputFile = paste0(modelName, "_output.pdf")
  file.rename(from=basename(f3), to=outputFile)
  
  if(isTRUE(tidy)) file.remove(basename(f2))

  if(isTRUE(verbose)) shell.exec(outputFile)
  
  return(invisible(paste0(modelName, ".pdf")))
}

report.jjm.diag = function(object, format="latex", tangle=FALSE, tidy=TRUE, verbose=TRUE, ...) {
  
  modelName = deparse(substitute(object))
  
  f = system.file("reports", "report-jjm.diag.Rmd", package = "jjmTools")
  knit(f)
  if(isTRUE(tangle)) {
    knit(f, tangle=TRUE)
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", f)
    file.rename(from=basename(f1), to=paste0(modelName, ".R"))
  }
  f2 = gsub(pattern = ".Rmd", replacement = "\\.md", f)
  f3 = gsub(pattern = ".Rmd", replacement = "\\.pdf", f)
  pandoc(basename(f2), format=format)
  outputFile = paste0(modelName, "_diag.pdf")
  file.rename(from=basename(f3), to=outputFile)
  
  if(isTRUE(tidy)) file.remove(basename(f2))
  
  if(isTRUE(verbose)) shell.exec(outputFile)
  
  return(invisible(paste0(modelName, ".pdf")))
  
  return(invisible())
}