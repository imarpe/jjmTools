# To define generic classes and methods for the package
#.readJjmOutput = function(outputPath, modelName, ...)
readJJM = function(path, modelName, type="output", ...) {

  if(length(modelName)>1) type="lstOuts"
  output = switch(type,
               output = .getJjmOutput(path=path, modelName=modelName, ...),
               data   = .getJjmData(path=path, modelName=modelName, ...),
               lstOuts= .getJjmOutputS(path=path, listName=modelName),
               stop("Invalid type.")
  )
  return(output)
}


# diagnostics = function(...) {
#   # 
#   class(output) = c("jjm.diag", class(output))
#   return(output)
# }



