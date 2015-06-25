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

.reshapeJJM2 = function(x, what, ...){
  
  var = switch(what,
               catchProj     = "Catch_fut_",
               ssbProj       = "SSB_fut_"
  )
  
  out = NULL
  
  for(i in seq_along(x)) {
    
    jjm.out = x[[i]]$output
    
  lastYear = jjm.out$R[nrow(jjm.out$R), 1]
  Nfutscen  = length(grep("SSB_fut_", names(jjm.out)))
  scenarios = c(paste0("F", lastYear ," SQ"), 
				paste0("F", lastYear, " 0.75x"), 
				paste0("F", lastYear, " 1.25x"), 
				paste0("F", lastYear, " 0.5x"), 
				paste0("F", lastYear, " 0x"))
  
  if(var == "Catch_fut_") {
  totCatch  = 0
  for(iFlt in grep("Obs_catch_", names(jjm.out)))
    totCatch = jjm.out[[iFlt]] + totCatch
  
  totCatch  = cbind(jjm.out$Yr, totCatch)
  colnames(totCatch) = c("year", "catch")
  }
  
  for(iScen in 1:length(scenarios)){
	
	if(var == "SSB_fut_") {
	idx = nrow(get("jjm.out")[["SSB"]][,c(1, 2)])
    tot = rbind(get("jjm.out")[["SSB"]][-idx,c(1, 2)],
                 get("jjm.out")[[paste(var, iScen, sep = "")]][,c(1, 2)])
    colnames(tot) = c("year", "SSB")
	}
	
	if(var == "Catch_fut_") {
	    tot = rbind(totCatch, jjm.out[[paste(var, iScen, sep = "")]])
		colnames(tot) = c("year", "catch")
	}
	
	  if(iScen == 1){
		totres = data.frame(tot)
		totres$scenario = scenarios[iScen]
      } else {
		res = data.frame(tot)
        res$scenario = scenarios[iScen]
        totres  = rbind(totres, res)
      }

  }

  colnames(totres) = c("year", "data", "scenario")	
  
    model   = x[[i]]$info$output$model 
	totres$model = model
    
    out    = rbind(out, totres)
    
  }
  
  colnames(out) = c("year", "data", "scenario", "model")
  
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
  
  Nfutscen  = length(grep("SSB_fut_", names(x[[1]]$output)))
  scenarios = c("F SQ", "F 0.75x", "F 1.25x", "F 0.5x", "F 0x")
  
  dataShape = .reshapeJJM2(x, what, ...)
  ikey           = simpleKey(text=scenarios, points = FALSE, lines = TRUE, columns = 2)
  ikey$lines$col = 1:length(scenarios)
  ikey$lines$lwd = 4
  ikey$lines$lty = 1
  
  pic = xyplot(data ~ year | model, data = dataShape, type = "l", groups = scenario, 
				key = ikey,
				prepanel = function(...) {list(ylim = c(0, max(dataShape$data, na.rm = TRUE)))},
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx = mapply(seq(length(x)/Nfutscen, length(x), length.out = Nfutscen) - length(x)/Nfutscen + 1,
                                seq(length(x)/Nfutscen, length(x), length.out = Nfutscen), FUN = seq)
                  #scen1 = idx[,1]; scen2 = idx[,2]; scen3 = idx[,3]; scen4 = idx[,4]; scen5 = idx[,5]
                  for(iScen in 2:Nfutscen) panel.xyplot(x[idx[,iScen]], y[idx[,iScen]], type = "l", col = iScen, lwd = 3)
                  panel.xyplot(x[idx[,1]], y[idx[,1]], type = "l", col = 1, lwd = 4)                  
                }, ...)
  
  return(pic)
}
