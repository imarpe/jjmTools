.reshapeJJM = function(x, what="biomass"){
  
  var = switch(what,
               biomass     = "TotBiom",
               ftot        = "Ftot",
               recruitment = "R",
               ssb         = "SSB",
               noFishTB    = "TotBiom_NoFish"
  )
  
  out = NULL
  
  for(i in seq_along(x)) {
    
    jjm.out = x[[i]]$output
    jjm.out$Ftot = cbind(jjm.out$Yr, rowMeans(jjm.out$TotF[, -1]), 
                         apply(jjm.out$TotF[, -1], 1, sd), 
                         rowMeans(jjm.out$TotF[, -1]),
                         rowMeans(jjm.out$TotF[, -1]))   
  
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

.funPlotSeries = function(x, what, cols, stack, endvalue, poslegend, ...){
  
  dataShape = .reshapeJJM(x, what = what)
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  mtheme = standard.theme("pdf", color=TRUE)
  mtheme$plot.line$lwd = 5
  mtheme$superpose.line$lwd = 5
  
  if(stack == !TRUE){
    pic = xyplot(mean ~ year, data = dataShape, groups = model, ylab = "", 
                 ylim = c(0.8*min(dataShape$lower), 1.1*max(dataShape$upper)),
                 xlim = c(min(dataShape$year - 1), max(dataShape$year + 1)),
                 key = list(lines = list(col = cols[1:length(x)], lwd = 3),
                            text = list(names(x))
                            , ...),                
                 par.settings=mtheme,
                 upper = dataShape$upper, lower = dataShape$lower,
                 panel = function(x, y, ...){
                   panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                   panel.xyplot(x, y, type ='l', cex = 0.6, lty = 1, lwd = 2, ...)
                   if(endvalue){
                     ltext(x=rev(x)[1], y=rev(y)[1], labels=rev(y)[1], pos=3, offset=1, cex=0.9,
                           font = 2, adj = 0)
                   }
                 }
                 , ...)
  } else {pic = xyplot(mean ~ year | model, data = dataShape, groups = model, ylab = "",
                       ylim = c(0.8*min(dataShape$lower), 1.1*max(dataShape$upper)),
                       xlim = c(min(dataShape$year - 1), max(dataShape$year + 1)),
                       upper = dataShape$upper, lower = dataShape$lower,
                       panel = function(x, y, ...){
                         panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                         panel.xyplot(x, y, type = 'l', cex = 0.6, lty = 1, lwd = 2, ...)
                         if(endvalue){
                           ltext(x=rev(x)[1], y=rev(y)[1], labels=rev(y)[1], pos=3, offset=1, cex=0.9,
                                 font = 2, adj = 0)
                         }
                       }, ...)
  }
  
  return(pic)
  
}


.funPlotKobe = function(x, what, cols, stack, endvalue, poslegend, ...){
  
  obj = x
  if(stack) {pic = .kobeFUN3(obj, cols = cols, endvalue = endvalue, ...)}
  else {pic = .kobeFUN2(obj, cols = cols, endvalue = endvalue, ...)}
  
  return(pic)
  
}


.funPlotProj = function(x, what, cols, stack, endvalue, poslegend, ...){
  
  if(!stack) warning("Series do not have to be in the same plot")
  
  if(length(x) < 2) jjm.out = x[[1]]$output
  
  if(what == "catchProj") pic = .projections_catchPredictionFUN(jjm.out, ...)
  if(what == "ssbProj")   pic = .projections_ssbPredictionFUN(jjm.out, ...)
  
  return(pic)
}
