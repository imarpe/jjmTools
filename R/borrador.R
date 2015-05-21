.fit_biomassFUN = function(jjm.out, ...){

  summaryData = rbind(cbind(jjm.out$Yr, jjm.out$TotBiom[, -1], "Total Biomass"))
  
  summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"),
                       cbind(summaryData[,c(1, 4, 6)], "lower"),
                       cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) = c("year", "data", "class", "estim")
  summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$year = as.integer(summaryData$year)
  summaryData$data = as.numeric(summaryData$data)
  
  summaryData$class= factor(summaryData$class, levels = unique(summaryData$class))
  
  alpha.f = 0.5
  
  pic = xyplot(data ~ year | class, data = summaryData, groups = class, ylab="",
                prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
                  panel = function(x, y){
                    panel.grid(h = -1, v = -1)
                    point = 1:length(jjm.out$Yr)
                    lower = (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                    upper = (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                    
                    # BIOMASS
                    if(panel.number() == 1){
                      panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey90", border = NA)
                      panel.xyplot(x[point], y[point], type = "l", lwd = 3, lty = 1, col = 1)
                    }
                  })
  
  return(pic)
}


.fit_recruitFUN = function(jjm.out, ...){
  
  summaryData = rbind(cbind(jjm.out$Yr, jjm.out$R[, -1], "Recruitment"))
  
  summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"),
                      cbind(summaryData[,c(1, 4, 6)], "lower"),
                      cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) = c("year", "data", "class", "estim")
  summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$year = as.integer(summaryData$year)
  summaryData$data = as.numeric(summaryData$data)
  
  summaryData$class= factor(summaryData$class, levels = unique(summaryData$class))
  
  alpha.f = 0.5
  
  pic = xyplot(data ~ year | class, data = summaryData, groups = class, ylab = "",
               prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
               panel = function(x, y){
                 panel.grid(h = -1, v = -1)
                 point = 1:length(jjm.out$Yr)
                 lower = (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                 upper = (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                 
                 # Recruitment
                                  
                 if(panel.number() == 1){
                   panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey90", border = NA)
                   panel.xyplot(x[point], y[point], type = "l", lwd = 3, lty = 1, col = 1)
                 }
                 
               })
  
  return(pic)
}


.fit_ssbFUN = function(jjm.out, ...){
  
  summaryData = rbind(cbind(jjm.out$SSB[which(jjm.out$SSB[, 1] %in% jjm.out$Yr), 1],
                             jjm.out$SSB[which(jjm.out$SSB[, 1] %in% jjm.out$Yr), -1], "SSB"))
  
  summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"), 
                       cbind(summaryData[,c(1, 4, 6)], "lower"),
                       cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) = c("year", "data", "class", "estim")
  summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$class= factor(summaryData$class, levels = unique(summaryData$class))
  summaryData$year = as.integer(summaryData$year)
  summaryData$data = as.numeric(summaryData$data)
  
  alpha.f = 0.45
  
  pic <- xyplot(data ~ year | class, data = summaryData, groups = class,
                prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  point <- 1:length(jjm.out$Yr)
                  lower <- (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                  upper <- (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                  
                  # SSB
                  if(panel.number() == 1){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey90", border = NA)
                    panel.xyplot(x[point], y[point], type = "l", lwd = 3, lty = 1, col = 1)
                  }
                })
  
  return(pic)
}


.fit_fFUN = function(jjm.out, ...){
  
  summaryData = rbind(cbind(jjm.out$Yr, cbind(rowMeans(jjm.out$TotF[,-1]), rowMeans(jjm.out$TotF[,-1]),
                                              rowMeans(jjm.out$TotF[,-1]), rowMeans(jjm.out$TotF[,-1])),
                            "Fishing mortality"))
  
  summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"), 
                      cbind(summaryData[,c(1, 4, 6)], "lower"),
                      cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) = c("year", "data", "class", "estim")
  summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$class= factor(summaryData$class, levels = unique(summaryData$class))
  summaryData$year = as.integer(summaryData$year)
  summaryData$data = as.numeric(summaryData$data)
  
  alpha.f = 0.45
  
  pic <- xyplot(data ~ year | class, data = summaryData, groups = class,
                prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  point <- 1:length(jjm.out$Yr)
                  lower <- (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                  upper <- (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                                         
                  panel.xyplot(x[point], y[point], lwd = 2, lty = 1, type = "l", col = 1)
                  
                })
  
  return(pic)
  
}


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











