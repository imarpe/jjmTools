# 
# .fit_biomassFUN = function(jjm.out, ...){
#   
#   summaryData = rbind(cbind(jjm.out$Yr, jjm.out$TotBiom[, -1], "Total Biomass"))
#   
#   summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"),
#                       cbind(summaryData[,c(1, 4, 6)], "lower"),
#                       cbind(summaryData[,c(1, 5, 6)], "upper"))
#   
#   colnames(summaryData) = c("year", "data", "class", "estim")
#   summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
#   summaryData$year = as.integer(summaryData$year)
#   summaryData$data = as.numeric(summaryData$data)
#   
#   summaryData$class= factor(summaryData$class, levels = unique(summaryData$class))
#   
#   alpha.f = 0.5
#   
#   pic = xyplot(data ~ year | class, data = summaryData, groups = class, ylab="",
#                prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
#                panel = function(x, y){
#                  panel.grid(h = -1, v = -1)
#                  point = 1:length(jjm.out$Yr)
#                  lower = (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
#                  upper = (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
#                  
#                  # BIOMASS
#                  panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey90", border = NA)
#                  panel.xyplot(x[point], y[point], type = "l", lwd = 3, lty = 1, col = 1)
#                  
#                })
#   
#   return(pic)
# }
# 
# 
# .fit_recruitFUN = function(jjm.out, ...){
#   
#   summaryData = rbind(cbind(jjm.out$Yr, jjm.out$R[, -1], "Recruitment"))
#   
#   summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"),
#                       cbind(summaryData[,c(1, 4, 6)], "lower"),
#                       cbind(summaryData[,c(1, 5, 6)], "upper"))
#   
#   colnames(summaryData) = c("year", "data", "class", "estim")
#   summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
#   summaryData$year = as.integer(summaryData$year)
#   summaryData$data = as.numeric(summaryData$data)
#   
#   summaryData$class= factor(summaryData$class, levels = unique(summaryData$class))
#   
#   alpha.f = 0.5
#   
#   pic = xyplot(data ~ year | class, data = summaryData, groups = class, ylab = "",
#                prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
#                panel = function(x, y){
#                  panel.grid(h = -1, v = -1)
#                  point = 1:length(jjm.out$Yr)
#                  lower = (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
#                  upper = (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
#                  
#                  # Recruitment
#                  panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey90", border = NA)
#                  panel.xyplot(x[point], y[point], type = "l", lwd = 3, lty = 1, col = 1)
#                  
#                })
#   
#   return(pic)
# }
# 
# 
# .fit_ssbFUN = function(jjm.out, ...){
#   
#   summaryData = rbind(cbind(jjm.out$SSB[which(jjm.out$SSB[, 1] %in% jjm.out$Yr), 1],
#                             jjm.out$SSB[which(jjm.out$SSB[, 1] %in% jjm.out$Yr), -1], "SSB"))
#   
#   summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"), 
#                       cbind(summaryData[,c(1, 4, 6)], "lower"),
#                       cbind(summaryData[,c(1, 5, 6)], "upper"))
#   
#   colnames(summaryData) = c("year", "data", "class", "estim")
#   summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
#   summaryData$class= factor(summaryData$class, levels = unique(summaryData$class))
#   summaryData$year = as.integer(summaryData$year)
#   summaryData$data = as.numeric(summaryData$data)
#   
#   alpha.f = 0.45
#   
#   pic <- xyplot(data ~ year | class, data = summaryData, groups = class,
#                 prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
#                 panel = function(x, y){
#                   panel.grid(h = -1, v = -1)
#                   point <- 1:length(jjm.out$Yr)
#                   lower <- (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
#                   upper <- (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
#                   
#                   panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey90", border = NA)
#                   panel.xyplot(x[point], y[point], type = "l", lwd = 3, lty = 1, col = 1)
#                   
#                 })
#   
#   return(pic)
# }
# 
# 
# .fit_fFUN = function(jjm.out, ...){
#   
#   summaryData = rbind(cbind(jjm.out$Yr, cbind(rowMeans(jjm.out$TotF[,-1]), rowMeans(jjm.out$TotF[,-1]),
#                                               rowMeans(jjm.out$TotF[,-1]), rowMeans(jjm.out$TotF[,-1])),
#                             "Fishing mortality"))
#   
#   summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"), 
#                       cbind(summaryData[,c(1, 4, 6)], "lower"),
#                       cbind(summaryData[,c(1, 5, 6)], "upper"))
#   
#   colnames(summaryData) = c("year", "data", "class", "estim")
#   summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
#   summaryData$class= factor(summaryData$class, levels = unique(summaryData$class))
#   summaryData$year = as.integer(summaryData$year)
#   summaryData$data = as.numeric(summaryData$data)
#   
#   alpha.f = 0.45
#   
#   pic <- xyplot(data ~ year | class, data = summaryData, groups = class,
#                 prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
#                 panel = function(x, y){
#                   panel.grid(h = -1, v = -1)
#                   point <- 1:length(jjm.out$Yr)
#                   
#                   panel.xyplot(x[point], y[point], lwd = 2, lty = 1, type = "l", col = 1)
#                   
#                 })
#   
#   return(pic)
#   
# }
# 
# 
# plot.jjm.output <- function(x, what = "biomass", ...){
#   jjm.out <- x[[1]]$output
#   jjm.in  <- x[[1]]$data
#   jjm.ypr <- x[[1]]$output$YPR
#   
#   model   <- x[[1]]$info$output$model
#   
#   #- Generic attributes of the stock assessment
#   Nfleets   <- length(c(jjm.out$Fshry_names))
#   Nsurveys  <- length(c(jjm.out$Index_names))
#   ages      <- jjm.in$ages[1]:jjm.in$ages[2]
#   lengths   <- jjm.in$lengths[1]:jjm.in$lengths[2]
#   
#   #- Get the age-structured fleets and length-structured fleets out
#   if(length(grep("pobs_fsh_", names(jjm.out))) > 0){
#     ageFleets <- unlist(strsplit(names(jjm.out)[grep("pobs_fsh_", names(jjm.out))], split = "_"))
#     ageFleets <- ageFleets[seq(3, length(ageFleets), 3)]
#   } else { ageFleets <- 0}
#   
#   if(length(grep("pobs_len_fsh_", names(jjm.out))) > 0){
#     lgtFleets <- unlist(strsplit(names(jjm.out)[grep("pobs_len_fsh_", names(jjm.out))], split = "_"))
#     lgtFleets <- lgtFleets[seq(4, length(lgtFleets), 4)]
#   } else {lgtFleets <- 0}
#   
#   #- Get the age-structured surveys and length-structured surveys out
#   if(length(grep("pobs_ind_", names(jjm.out))) > 0){
#     ageSurveys <- unlist(strsplit(names(jjm.out)[grep("pobs_ind_", names(jjm.out))], "_"))
#     ageSurveys <- ageSurveys[seq(3, length(ageSurveys), 3)]
#   } else {ageSurveys <- 0}
#   
#   if(length(grep("pobs_len_ind_", names(jjm.out))) > 0){
#     lgtSurveys <- unlist(strsplit(names(jjm.out)[grep("pobs_len_ind_", names(jjm.out))], "_"))
#     lgtSurveys <- lgtSurveys[seq(4, length(lgtSurveys), 4)]
#   } else {lgtSurveys <- 0}
#   
#   pic <- switch(what,
#                 
#                 biomass = .fit_biomassFUN(jjm.out, scales = list(alternating = 1, y = list(relation = "free", rot = 0),
#                                                                  axs = "i"), ...),
#                 
#                 recruitment = .fit_recruitFUN(jjm.out, scales = list(alternating = 1, y = list(relation = "free", rot = 0),
#                                                                      axs = "i"), ...),
#                 
#                 ssb = .fit_ssbFUN(jjm.out, scales = list(alternating = 1, y = list(relation = "free", rot = 0),
#                                                          axs = "i"), ...),
#                 
#                 Fm = .fit_fFUN(jjm.out, scales = list(alternating = 1, y = list(relation = "free", rot = 0),
#                                                       axs = "i"), ...))
#   
#   return(pic)
# }