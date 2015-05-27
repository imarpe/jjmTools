jjm.out  = model$Model_North_1.0$output
jjm.in   = model$Model_North_1.0$data
jjm.info = model$Model_North_1.0$info$output

ages=(jjm.in$ages[1]:jjm.in$ages[2])
cols=(rainbow(length(ages)))


.fit_stockRecruitmentFUN = function(jjm.out, cols, ...)
{
  
  county = grep("SR_Curve_years_", names(jjm.out))
  colyear = NULL
  for(i in seq_along(county)){
    namesy = paste("SR_Curve_years_", i, sep = "")
    colyear[[i]] = jjm.out[[namesy]]
  }
  
  seqYears = NULL
  for(i in seq_along(colyear)){
    seqYears[[i]] = seq(from = colyear[[i]][1], to = colyear[[i]][2], by = 1)
  }
  
  
  res1 = data.frame(jjm.out[["Stock_Rec"]][, c(2, 4)])
  res1$class = "Simulated"
  res1$year = jjm.out[["Stock_Rec"]][, 1]
  
  res1$color = numeric(nrow(res1))
  for(i in seq_along(seqYears)){
    res1$color[which(res1$year %in% seqYears[[i]])] = i
    res1$color[which(res1$year %in% seqYears[[i]])] = i
  }
  res1 = res1[1:(nrow(res1) - 1), ]
  
  
  count = grep("stock_Rec_Curve", names(jjm.out))
  res2 = NULL
  for(i in seq_along(count)){
    namesres2 = paste("stock_Rec_Curve_", i, sep = "")
    res2[[i]] = data.frame(jjm.out[[namesres2]])
    res2[[i]] = res2[[i]][1:(nrow(res2[[i]])-1), ] 
    res2[[i]]$class = paste("Regime", i, sep ="")
    res2[[i]]$year = NA
    res2[[i]]$color = NA
  }
  res2 = do.call("rbind", res2)
  
  res  = rbind(res1, res2) 
  colnames(res) = c("SSB", "Rec", "class", "year", "col")
  
labelLeg = NULL
  for(i in seq_along(colyear)){
    labelLeg[1] = "Simulated"
    labelLeg[i+1] = paste(colyear[[i]][1], " - ", colyear[[i]][2], sep = "")
  }

labelCol = NULL
labelCol[1] = "darkgrey"
labelCol[seq_along(seqYears) + 1] = rev(cols)[seq_along(seqYears)]

idxobs = list()
  pic = xyplot(Rec ~ SSB, data = res, groups = class,
               panel = function(x, y){
                 panel.grid(h = -1, v = -1)
                 
                 for(i in c(0, seq_along(seqYears))){
                  idxobs[[i+1]] = which(res$SSB %in% x & res$class == "Simulated" & res$col == i)
                 }
                                  
                 countm = grep("Regime", unique(res$class))
                 idxmod = NULL
                  for(i in seq_along(idxobs)){
                    if(i == 1) {panel.points(x[idxobs[[i]]], y[idxobs[[i]]], type = "p", cex = 2, pch = 19, col = "darkgrey")}
                    else {panel.points(x[idxobs[[i]]], y[idxobs[[i]]], type = "p", cex = 2, pch = 19, col = rev(cols)[i-1])}
                  }

                  for(i in seq_along(countm)){
                    namesid = paste("Regime", i, sep = "")
                    idxmod = which(res$SSB %in% x & res$class == namesid)
                    panel.xyplot(x[idxmod], y[idxmod], type = "l", lwd = 5, col = rev(cols)[i], lty = 1)                
                 }
              },
              
                auto.key = list(title = "", 
                                text = labelLeg,
                                x = 0.85, y = 1, cex = 1.75,
                                points = TRUE, border = FALSE, 
                                lines = FALSE, col = labelCol), ...)
  
  return(pic)
}


 .fit_stockRecruitmentFUN(jjm.out, cols,
                          ylab = "Recruitment", xlab = "Spawning Stock Biomass", 
                          main = "Stock Recruitment")