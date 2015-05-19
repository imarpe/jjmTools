.reshapeJJM = function(x, what="biomass"){
  
  var = switch(what,
               biomass     = "TotBiom",
               #Fm          = "TotF",
               recruitment = "R",
               ssb         = "SSB")
  
  out = NULL
  
  for(i in seq_along(x)) {
    
    jjm.out = x[[i]]$output
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
