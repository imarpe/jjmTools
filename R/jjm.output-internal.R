.reshapeJJM = function(x, what="biomass"){
  
  var = switch(what,
               biomass     = "TotBiom",
               #ftot        = "Ftot",
               recruitment = "R",
               ssb         = "SSB",
               noFishTB    = "TotBiom_NoFish"
			   )
  
  out = NULL
  
  for(i in seq_along(x)) {
    
    jjm.out = x[[i]]$output
#     jjm.out$Ftot = cbind(jjm.out$Yr, rowMeans(jjm.out$TotF[, -1]), 
#                          apply(jjm.out$TotF[, -1], 1, sd), 
#                          rowMeans(jjm.out$TotF[, -1]) - rowMeans(jjm.out$TotF[, -1]),
#                          rowMeans(jjm.out$TotF[, -1]) - rowMeans(jjm.out$TotF[, -1]))            
    jjm.in  = x[[i]]$data
    jjm.ypr = x[[i]]$output$YPR
    model   = x[[i]]$info$output$model 
    
    outVar = jjm.out[[var]]
    year   = jjm.out$Yr
    ind    = which(outVar[, 1] %in% year) 
    outVar = outVar[ind, ]
    outVar = data.frame(outVar, model=model)
    
    out    = rbind(out, outVar)
    
  }
  
  colnames(out) = c("year", "mean", "sd", "lower", "upper", "model")
  
  return(out)
  
}


.combineModels = function(...){
  
  modelList = list(...)
  
  for(i in modelList)
    if(class(i) != "jjm.output")
      stop("Objects must be of class 'jjm.output'.")
  
  modelList = c(...)
  
  # Remove repeated models from modelList 
  modelList2 = modelList
  for(i in seq_along(modelList[-length(modelList)]))
    for(j in seq(from = i + 1, to = length(modelList)))
      if(identical(modelList[[i]], modelList[[j]]))
        modelList2[[j]] = NULL
  
  modelList = modelList2; rm("modelList2")
  
  modelNames = NULL
    for(j in seq_along(modelList)){
      modelNames = c(modelNames, modelList[[j]]$info$output$model)
    }
    
  # Models
  models = list()
  for(i in seq_along(modelList)){
    models[[i]] = modelList[[i]][c("info", "data", "output")]
  }
  
  names(models) = modelNames

  output = models 

  class(output) = c("jjm.output")
  
  return(output)

}


