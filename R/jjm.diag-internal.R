
# Internal functions of diagnostic ----------------------------------------
.createDataFrame <- function(data, years, class){
  dims  <- dim(data)
  res   <- data.frame(year = rep(years, dims[2]), data = c(data), class = rep(class, each = dims[1]))
  return(res)}

.bubbles         <- function(x, data, bub.scale = 2.5, col = c("black", "black"),...){
  dots <- list(...)
  dots$data <- data
  dots$cex <- bub.scale*(abs(data$resids)/max(abs(data$resids), na.rm = TRUE)) + bub.scale*0.1
  dots$col <- ifelse(data$resids > 0, col[1], col[2])
  dots$pch <- ifelse(data$resids>0, 19, 1)
  dots$panel <- function(x, y, ..., cex, subscripts){
    panel.grid(h = -1, v = -1)
    panel.xyplot(x, y, cex = cex[subscripts], ...)
  }
  call.list <- c(x = x, dots)
  ans <- do.call("xyplot", call.list)
  ans
}

.ac   <- function(x) {return(as.character(x))}
.an   <- function(x) {return(as.numeric(x))}
.anf  <- function(x) {return(as.numeric(as.character(x)))}

.setOutputNames <- function(out){
  
  names(out)     <- c("Years", "Total Fishing mortality", "Total biomass no Fishing", "SSB no fishing", "Total biomass",
                      "SSB in the future under scenario 1", "SSB in the future under scenario 2",
                      "SSB in the future under scenario 3", "SSB in the future under scenario 4",
                      "SSB in the future under scenario 5", "Catch in the future under scenario 1",
                      "Catch in the future under scenario 2", "Catch in the future under scenario 3",
                      "Catch in the future under scenario 4", "Catch in the future under scenario 5",
                      "SSB", "Recruitment", "Numbers at age", "F at age fishery 1", "F at age fishery 2",
                      "F at age fishery 3", "F at age fishery 4", "Fishery names", "Indices names",
                      "Observations at age survey 1", "Observations at age survey 2", "Observations at age survey 3",
                      "Observations at age survey 4", "Observations at age survey 5", "Observations at age survey 6",
                      "Observations at age survey 7", "Observations at age survey 8", "Observations at age survey 9",
                      "Survey catchabilities", "Proportions at age fishery 1 observed",
                      "Proportions at age fishery 2 observed", "Proportions at age fishery 3 observed",
                      "Proportions at age fishery 4 observed", "Proportions at age fishery 1 predicted",
                      "Proportions at age fishery 2 predicted", "Proportions at age fishery 3 predicted",
                      "Proportions at age fishery 4 predicted", "Proportions at age survey 1 observed",
                      "Proportions at age survey 4 observed", "Proportions at age survey 1 predicted",
                      "Proportions at age survey 4 predicted", "Total catch fishery 1 observed",
                      "Total catch fishery 1 predicted", "Total catch fishery 2 observed",
                      "Total catch fishery 2 predicted", "Total catch fishery 3 observed",
                      "Total catch fishery 3 predicted", "Total catch fishery 4 observed",
                      "Total catch fishery 4 predicted", "F_fsh_1", "F_fsh_2", "F_fsh_3", "F_fsh_4", 
                      "Selectivity fishery 1", "Selectivity fishery 2", "Selectivity fishery 3", "Selectivity fishery 4",
                      "Selectivity survey 1", "Selectivity survey 2", "Selectivity survey 3", "Selectivity survey 4",
                      "Selectivity survey 5", "Selectivity survey 6", "Selectivity survey 7", "Selectivity survey 8",
                      "Selectivity survey 9", "Stock recruitment", "Stock recruitment curve", "Likelihood composition",
                      "Likelihood composition names", "Sel_Fshry_1", "Sel_Fshry_2", "Sel_Fshry_3", "Sel_Fshry_4",
                      "Survey_Index_1", "Survey_Index_2", "Survey_Index_3", "Survey_Index_4", "Survey_Index_5",
                      "Survey_Index_6", "Survey_Index_7", "Survey_Index_8", "Survey_Index_9", "Age_Survey_1",
                      "Age_Survey_2", "Age_Survey_3", "Age_Survey_4", "Age_Survey_5", "Age_Survey_6", "Age_Survey_7",
                      "Age_Survey_8", "Age_Survey_9", "Sel_Survey_1", "Sel_Survey_2", "Sel_Survey_3", "Sel_Survey_4",
                      "Sel_Survey_5", "Sel_Survey_6", "Sel_Survey_7", "Sel_Survey_8", "Sel_Survey_9",
                      "Recruitment penalty", "F penalty", "Survey 1 catchability penalty",
                      "Survey 1 catchability power function", "Survey 2 catchability penalty",
                      "Survey 2 catchability power function", "Survey 3 catchability penalty",
                      "Survey 3 catchability power function", "Survey 4 catchability penalty",
                      "Survey 4 catchability power function", "Survey 5 catchability penalty",
                      "Survey 5 catchability power function", "Survey 6 catchability penalty",
                      "Survey 6 catchability power function", "Survey 7 catchability penalty",
                      "Survey 7 catchability power function", "Survey 8 catchability penalty",
                      "Survey 8 catchability power function", "Survey 9 catchability penalty",
                      "Survey 9 catchability power function", "Natural mortality", "Steepness of recruitment",
                      "Sigma recruitment", "Number of parameters estimated", "Steepness prior", "Sigma recruitment prior",
                      "Rec_estimated_in_styr_endyr", "SR_Curve_fit__in_styr_endyr", "Model_styr_endyr",
                      "Natural mortality prior", "q prior", "q power prior", "cv catch biomass", "Projection year range",
                      "Fsh_sel_opt_fish", "Survey_Sel_Opt_Survey", "Phase_survey_Sel_Coffs", "Fishery selectivity ages",
                      "Survey selectivity ages", "Phase_for_age_spec_fishery", "Phase_for_logistic_fishery",
                      "Phase_for_dble_logistic_fishery", "Phase_for_age_spec_survey", "Phase_for_logistic_survey",
                      "Phase_for_dble_logistic_srvy", "EffN_Fsh_1", "EffN_Fsh_2", "EffN_Fsh_3", "EffN_Fsh_4",
                      "C_fsh_1", "C_fsh_2", "C_fsh_3", "C_fsh_4", "Weight at age in the population","Maturity at age",
                      "Weight at age in fishery 1", "Weight at age in fishery 2", "Weight at age in fishery 3",
                      "Weight at age in fishery 4", "Weight at age in survey 1", "Weight at age in survey 2",
                      "Weight at age in survey 3", "Weight at age in survey 4", "Weight at age in survey 5",
                      "Weight at age in survey 6", "Weight at age in survey 7", "Weight at age in survey 8",
                      "Weight at age in survey 9", "EffN_Survey_1", "EffN_Survey_4")
  return(out)}

.readYPR <- function(fileName){
  jjm.ypr            <- read.table(fileName, sep = " ", skip = 4, header = TRUE, fill = TRUE)
  jjm.ypr[1,]        <- jjm.ypr[1, c(1, 8, 2, 3, 4, 5, 6, 7)]
  colnames(jjm.ypr)  <- c("F", "X", "SSB", "Yld", "Recruit", "SPR", "B", "X2")
  jjm.ypr            <- jjm.ypr[,-grep("X", colnames(jjm.ypr))]
  return(jjm.ypr)
}

# Function that have not been used
.plot.bubbles    <- function(x, xlab = " ", ylab = " ", main = " ", factx = 0, facty = 0, amplify = 1){
  my.col <- c("white", " ", "black")
  xval <- c(col(x)) + factx
  yval <- c(row(x)) + facty
  
  # area of bubble is proportional to value.
  plot(x = xval, y = yval, cex = amplify*c(sqrt(abs(x/pi))), xlab = xlab, ylab = ylab, main = main,
       pch = 19, xaxt = "n", yaxt = "n", col = my.col[2 + check.zero(c(x)/check.zero(abs(c(x))))])
  
  # area of bubble is proportional to value.
  points(x = xval, y = yval, cex = amplify*c(sqrt(abs(x/pi))))
}

# Function that have not been used
.check.zero      <- function(x){
  ## checks if there are zeros and replaces them with 1.
  x[x == 0] <- 1
  return(x)
}


# Plots of diagnostic function --------------------------------------------
.input_weightFisheryFUN <- function(Nfleets, jjm.out, ages, ...)
{
  for(iFleet in 1:Nfleets){
    res <- .createDataFrame(jjm.out[[paste("wt_fsh_", iFleet, sep = "")]][,-1], jjm.out$Yr, ages)
    
    if(iFleet == 1)
      tot <- cbind(res, c(jjm.out$Fshry_names[iFleet]))
    
    if(iFleet != 1)
      tot <- rbind(tot,cbind(res,c(jjm.out$Fshry_names[iFleet])))
  }
  colnames(tot) <- c("year","data","age","fleet"); res <- tot
  
  pic <- xyplot(data~year|fleet, data = res, groups = age,
                type = "l",
                auto.key = list(space = "right", points = FALSE, lines = TRUE, type = "b"),
                par.settings = list(superpose.symbol = list(pch = as.character(ages), cex = 1)),
                ...)
  
  return(pic)
}

.input_weightAgeFUN <- function(Nsurveys, jjm.out, ages, ...)
{
  for(iSurvey in 1:Nsurveys){
    res <- .createDataFrame(jjm.out[[paste("wt_ind_", iSurvey, sep = "")]][,-1],jjm.out$Yr, ages)
    
    if(iSurvey == 1)
      tot <- cbind(res, c(jjm.out$Index_names[iSurvey]))
    
    if(iSurvey != 1)
      tot <- rbind(tot, cbind(res, c(jjm.out$Index_names[iSurvey])))
  }
  colnames(tot) <- c("year","data","age","survey"); res <- tot
  
  pic <- xyplot(data~year|survey, data = res, groups = age,
                par.settings = list(superpose.symbol = list(pch = as.character(ages), cex = 1)), ...)
  
  return(pic)
}

.input_weightByCohortFleetFUN <- function(Nfleets, jjm.out, ages, ...)
{
  for(iFleet in 1:Nfleets){
    res <- .createDataFrame(jjm.out[[paste("wt_fsh_", iFleet, sep = "")]][,-1], jjm.out$Yr, ages)
    
    if(iFleet == 1)
      tot <- cbind(res, c(jjm.out$Fshry_names[iFleet]))
    
    if(iFleet != 1)
      tot <- rbind(tot, cbind(res, c(jjm.out$Fshry_names[iFleet])))
  }
  colnames(tot) <- c("year","data","age","fleet"); res <- tot
  res$cohort <- res$year - res$age
  yrs <- sort(rev(jjm.out$Yr)[1:13])
  
  pic <- xyplot(data ~ age|fleet, data = subset(res, cohort%in%yrs), groups = cohort,
                par.settings = list(superpose.symbol = list(pch = .ac(ages), cex = 1)),
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(...)
                }, ...)
  
  return(pic)
}

.input_weightByCohortSurveyFUN <- function(Nsurveys, jjm.out, ages, ...)
{
  for(iSurvey in 1:Nsurveys){
    res <- .createDataFrame(get("jjm.out")[[paste("wt_ind_",iSurvey,sep="")]][,-1],jjm.out$Yr,ages)
    if(iSurvey == 1) tot <- cbind(res,c(jjm.out$Index_names[iSurvey]))
    if(iSurvey != 1) tot <- rbind(tot,cbind(res,c(jjm.out$Index_names[iSurvey])))
  }
  colnames(tot) <- c("year","data","age","survey"); res <- tot
  res$cohort <- res$year - res$age
  
  yrs <- sort(rev(jjm.out$Yr)[1:13])
  
  
  pic <- xyplot(data ~ age|survey, data = subset(res, cohort %in% yrs), groups = cohort,
                par.settings = list(superpose.symbol = list(pch = as.character(ages), cex = 1)),                
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(...)
                }, ...)
  
  return(pic)
}

.input_ageFleetsFUN <- function(jjm.in, ageFleets, ...)
{
  for(iFleet in .an(ageFleets)){
    res <- .createDataFrame(sweep(jjm.in$Fagecomp[,,iFleet], 1,
                                  apply(jjm.in$Fagecomp[,,iFleet], 1, sum, na.rm = T), "/"),
                            jjm.in$years[1]:jjm.in$years[2],
                            ages)
    
    res <- cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == .an(ageFleets)[1])
      tot <- res
    
    if(iFleet != .an(ageFleets)[1])
      tot <- rbind(tot,res)
  }
  
  colnames(tot) <- c("year","data","age","fleet"); res <- tot
  yrs           <- rev(sort(unique(res$year)))[1:10]
  
  pic <- barchart(data ~ age | fleet* as.factor(year), data = subset(res, year %in% yrs),
                  scales = list(rotation = 90, alternating = 3, y = list(axs = "i")), horizontal = FALSE,
                  groups = fleet, strip = FALSE, strip.left = strip.custom(style = 1), reverse.rows = TRUE,
                  panel = function(...){
                    panel.grid(h = -1, v = -1)
                    panel.barchart(...)
                  }, ...)
  
  return(pic)
}

.input_ageFleets2FUN <- function(jjm.in, ageFleets, cols)
{
  for(iFleet in .an(ageFleets)){
    res <- .createDataFrame(sweep(jjm.in$Fagecomp[,,iFleet], 1,
                                  apply(jjm.in$Fagecomp[,,iFleet], 1, sum, na.rm = T), "/"),
                            jjm.in$years[1]:jjm.in$years[2],
                            ages)
    
    res <- cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == .an(ageFleets)[1])
      tot <- res
    
    if(iFleet != .an(ageFleets)[1])
      tot <- rbind(tot,res)
  }
  
  colnames(tot) <- c("year","data","age","fleet"); res <- tot
  
  pic <- levelplot(data ~ age*year | fleet, data = res,
                   col.regions = cols, cuts = 10,
                   main = "Age composition in fleets",
                   zlab = "", ylab = "", colorkey = TRUE,
                   layout = c(length(ageFleets), 1))
  
  return(pic)
}

.input_ageFleetsPlotsFUN <- function(jjm.in, ages, cols)
{
  for(iFleet in .an(ageFleets)){
    res <- .createDataFrame(sweep(jjm.in$Fagecomp[,,iFleet], 1,
                                  apply(jjm.in$Fagecomp[,,iFleet], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2],
                            ages)
    
    res <- cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == .an(ageFleets)[1])
      tot <- res
    
    if(iFleet != .an(ageFleets)[1])
      tot <- rbind(tot,res)
  }
  
  colnames(tot) <- c("year","data","age","fleet"); res <- tot
  
  res$cohort <- (res$year - res$age) %% length(ages) + 1
  ageFleetsPlots <- list()
  for(iFleet in unique(res$fleet)){
    tmpres <- subset(res, fleet == iFleet)
    pic <- xyplot(data ~ age | as.factor(year), data = tmpres,
                  xlab = "Age", ylab = "Proportion at age", main = paste("Age composition in fleets", iFleet),
                  as.table = TRUE,
                  panel = function(x,y){
                    yr      <- names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  <- tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x, y, horizontal = FALSE, origin = 0, box.width = 1,
                                   col = cols[tmpres$cohort[colidx]])
                  })
    
    ageFleetsPlots[[iFleet]] <- pic
  }
  
  return(ageFleetsPlots)
}

.input_ageCompositionSurvey1FUN <- function(jjm.in, ages)
{
  for(iSurvey in which(jjm.in$Inumageyears>0)){
    res <- .createDataFrame(sweep(jjm.in$Ipropage[,,iSurvey], 1,
                                  apply(jjm.in$Ipropage[,,iSurvey], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2], ages)
    res <- cbind(res, jjm.in$Inames[iSurvey])
    
    yrs <- rev(sort(unique(res$year)))[1:10]
    
    if(iSurvey == 1)
      tot <- res
    
    if(iSurvey != 1)
      tot <- rbind(tot, res)
  }
  
  colnames(tot) <- c("year", "data", "age", "survey"); res <- tot
  
  pic <- barchart(data ~ age | survey * as.factor(year), data = subset(res, year %in% yrs),
                  scales = list(rotation = 90, alternating = 3, y = list(axs = "i")), 
                  horizontal = FALSE, groups = survey, strip = FALSE, strip.left = strip.custom(style = 1),
                  reverse.rows = TRUE,
                  main = "Age composition in surveys", as.table = TRUE,
                  panel = function(...){
                    panel.grid(h = -1, v = -1)
                    panel.barchart(...)
                  }, ylab = "Proportion at age")
  
  return(pic)
}

.input_ageCompositionSurvey2FUN <- function(jjm.in, cols)
{
  for(iSurvey in which(jjm.in$Inumageyears > 0)){
    res <- .createDataFrame(sweep(jjm.in$Ipropage[,,iSurvey], 1,
                                  apply(jjm.in$Ipropage[,,iSurvey], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2], ages)
    res <- cbind(res, jjm.in$Inames[iSurvey])
    
    yrs <- rev(sort(unique(res$year)))[1:10]
    
    if(iSurvey == 1)
      tot <- res
    
    if(iSurvey != 1)
      tot <- rbind(tot, res)
  }
  
  colnames(tot) <- c("year", "data", "age", "survey"); res <- tot
  
  pic <- levelplot(data ~ age*year | survey, data = res,
                   col.regions = cols, cuts = 10,
                   main = "Age composition in surveys",
                   zlab = "", ylab = "", colorkey = TRUE, as.table = FALSE,
                   scales = list(rotation = 90, alternating = 3))
  
  return(pic)
}

.input_weightPopulationFUN <- function(jjm.in, ages)
{
  res <- data.frame(year = 1, data = jjm.in$Pwtatage, age = ages)
  pic <- xyplot(weight~age, data = res,
                xlab = "Age", ylab = "Weight (kg)", main = "Weight in the stock",
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., type = "b", pch = 19, cex = 0.6, lwd = 2, col = 1)
                })
  
  return(pic)
}

.input_maturityPopulationFUN <- function(jjm.in, ages)
{
  res <- data.frame(year = 1, data = jjm.in$Pmatatage, age = ages)
  pic <- xyplot(maturity~age, data = res,
                xlab = "Age", ylab = "Proportion mature", main = "Maturity in the stock",
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., type = "b", pch = 19, cex = 0.6, lwd = 2, col = 1)
                })
  
  return(pic)
}

.input_lengthComposition1FUN <- function(cols, lgtFleets, jjm.in, lengths)
{
  for(iFleet in .an(lgtFleets)){
    res <- .createDataFrame(sweep(jjm.in$Flengthcomp[,,iFleet], 1,
                                  apply(jjm.in$Flengthcomp[,,iFleet], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2], lengths)
    res <- cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == .an(lgtFleets)[1])
      tot <- res
    
    if(iFleet != .an(lgtFleets)[1]) 
      tot <- rbind(tot, res)
  }
  colnames(tot) <- c("year", "data", "length", "fleet"); res <- tot
  
  lengthComposition1 <- list()
  for(iFleet in unique(tot$fleet)){
    pic <- levelplot(data ~ length*year | fleet,data=subset(tot,fleet==iFleet),as.table=T,
                     col.regions=cols,cuts=10,
                     main=paste("Length composition in fleet",iFleet),
                     zlab="",ylab="",colorkey=T)
    
    lengthComposition1[[iFleet]] <- pic
  }
  
  return(lengthComposition1)
}

.input_lengthComposition2FUN <- function(lgtFleets, jjm.in, lengths)
{
  for(iFleet in .an(lgtFleets)){
    res <- .createDataFrame(sweep(jjm.in$Flengthcomp[,,iFleet], 1,
                                  apply(jjm.in$Flengthcomp[,,iFleet], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2], lengths)
    res <- cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == .an(lgtFleets)[1])
      tot <- res
    
    if(iFleet != .an(lgtFleets)[1]) 
      tot <- rbind(tot, res)
  }
  colnames(tot) <- c("year", "data", "length", "fleet"); res <- tot
  
  tot$cohort <- (tot$year - tot$length) %% length(lengths) + 1
  lengthComposition2 <- list()
  for(iFleet in unique(tot$fleet)){
    tmpres <- subset(tot, fleet == iFleet)
    
    pic <- xyplot(data ~ length | as.factor(year), data = tmpres,
                  xlab = "Length", ylab = "Proportion at length", main = paste("Length composition in fleets", iFleet),
                  as.table = TRUE,
                  panel = function(x,y){
                    yr      <- names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  <- tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x, y, horizontal = FALSE, origin = 0, box.width = 1, col = tmpres$cohort[colidx],
                                   border = 'transparent')
                  })
    
    lengthComposition2[[iFleet]] <- pic
  }
  
  return(lengthComposition2)
}

.fit_totalCatchFUN <- function(Nfleets, jjm.out)
{
  for(iFleet in 1:Nfleets){
    res <- cbind(jjm.out$Yr, jjm.out[[paste("Obs_catch_", iFleet, sep = "")]])
    
    if(Nfleets == 1) res <- cbind(res, jjm.out$Fshry_names[iFleet])
    if(Nfleets > 1) res <- cbind(res, jjm.out$Fshry_names[iFleet, 1])
    if(iFleet == 1) tot <- res
    if(iFleet != 1) tot <- rbind(tot, res)
  }
  colnames(tot) <- c("year", "catch", "fleet"); res <- data.frame(tot)
  res$catch <- as.numeric(as.character(res$catch))
  
  totcatch <- numeric()
  for(iYr in sort(unique(res$year))){
    totcatch[which(iYr == sort(unique(res$year)))] <- sum(subset(res, year == iYr)$catch)
  }
  
  pic <- xyplot(totcatch~jjm.out$Yr,
                xlab = "Years", ylab = "Catch in kt", main = "Total catch",
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.barchart(..., horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                }, scales = list(y = list(axs = "i")))
  
  return(pic)
}

.fit_totalCatchByFleetFUN <- function(jjm.out, Nfleets)
{
  for(iFleet in 1:Nfleets){
    res <- cbind(jjm.out$Yr, jjm.out[[paste("Obs_catch_", iFleet, sep = "")]])
    
    if(Nfleets == 1) res <- cbind(res, jjm.out$Fshry_names[iFleet])
    if(Nfleets > 1) res <- cbind(res, jjm.out$Fshry_names[iFleet, 1])
    if(iFleet == 1) tot <- res
    if(iFleet != 1) tot <- rbind(tot, res)
  }
  colnames(tot) <- c("year", "catch", "fleet"); res <- data.frame(tot)
  res$catch <- as.numeric(as.character(res$catch))
  
  res$year <- .an(.ac(res$year))
  
  for(iFleet in 2:Nfleets){
    idx <- which(res$fleet == jjm.out$Fshry_names[iFleet])
    res[idx,"catch"] <- subset(res, fleet == jjm.out$Fshry_names[iFleet])$catch +
      subset(res, fleet == jjm.out$Fshry_names[iFleet - 1])$catch
  }
  
  pic <- xyplot(catch ~ year, data = res, groups = fleet,
                xlab = "Years", ylab = "Catch by fleet in kt", main = "Total catch by fleet",
                type = "l",
                auto.key = list(space = "right", points = FALSE, lines = FALSE, col = 1:Nfleets),
                panel = function(...){
                  lst <- list(...)
                  idx <- mapply(seq,
                                from = seq(1, length(lst$y), length(lst$y)/4),
                                to = seq(1, length(lst$y), length(lst$y)/4) + (length(lst$y)/4 - 1))
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., col = "white")
                  for(iFleet in Nfleets:1){
                    panel.polygon(x = c(lst$x[idx[,iFleet]], rev(lst$x[idx[,iFleet]])),
                                  y = c(rep(0, length(lst$y[idx[,iFleet]])), rev(lst$y[idx[,iFleet]])),
                                  col = (4:1)[iFleet], border = 0)
                  }
                  
                }, scales=list(y = list(axs = "i")))
  
  return(pic)
}

.fit_catchResidualsByFleetFUN <- function(Nfleets, jjm.out)
{
  for(iFleet in 1:Nfleets){
    res <- data.frame(year = jjm.out$Yr, obs = jjm.out[[paste("Obs_catch_", iFleet, sep = "")]],
                      model = jjm.out[[paste("Pred_catch_", iFleet, sep = "")]], fleet = jjm.out$Fshry_names[iFleet])
    
    if(iFleet == 1) tot <- res
    if(iFleet != 1) tot <- rbind(tot, res)
  }
  tot$obs[tot$obs<=0]   <- NA
  tot$obs[tot$model<=0] <- NA
  res                   <- tot
  resids                <- log(res$obs + 1) - log(res$model + 1); res$resids <- resids
  scalar                <- 3/max(resids, na.rm = TRUE)
  residRange            <- range(resids, na.rm = TRUE)
  ikey                  <- simpleKey(text = .ac(round(seq(residRange[1], residRange[2], length.out = 6), 2)),
                                     points = TRUE, lines = FALSE, columns = 2)
  ikey$points$cex       <- abs(round(seq(residRange[1], residRange[2], length.out = 6), 2))*scalar
  ikey$points$col       <- 1
  ikey$points$pch       <- ifelse(test = round(seq(residRange[1], residRange[2], length.out = 6), 2) > 0,
                                  yes = 19, no = 1)
  
  
  pic <- xyplot(resids*scalar ~ year | as.factor(fleet), data = res,
                xlab = "Years", ylab = "Residuals", main = "Catch residuals by fleet",
                prepanel = function(...) {list(ylim = c(1, 1))},
                layout = c(1, Nfleets),
                type = "p", lwd = 3, key = ikey,
                cex.axis = 1.2, font = 2,
                scales = list(y = list(draw = FALSE), alternating = 3),
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  panel.points(x, 1, cex = abs(y), col = ifelse(test = y > 0, yes = "black",  "white"), pch = 19)
                  panel.points(x, 1, cex = abs(y), col = 1, pch = 1)
                })
  
  return(pic)
}

.fit_absoluteResidualCatchByFleetFUN <- function(Nfleets, jjm.out)
{
  for(iFleet in 1:Nfleets){
    res <- data.frame(year = jjm.out$Yr, obs = jjm.out[[paste("Obs_catch_", iFleet, sep = "")]],
                      model = jjm.out[[paste("Pred_catch_", iFleet, sep = "")]], fleet = jjm.out$Fshry_names[iFleet])
    
    if(iFleet == 1) tot <- res
    if(iFleet != 1) tot <- rbind(tot, res)
  }
  tot$obs[tot$obs <= 0]   <- NA
  tot$obs[tot$model <= 0] <- NA
  res                   <- tot
  
  pic <- xyplot(model - obs ~ year | fleet, data = res, allow.multiple = TRUE,
                xlab = "Years", ylab = "Absolute residual catch", main = "Absolute residual catch by fleet",
                panel = function(...){
                  panel.grid(h = -1, v = -1, lty = 3)
                  panel.barchart(..., horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                }, scales = list(alternating = 3))
  
  return(pic)
}

.fit_residualsCatchAtAgeByFleetFUN <- function(ageFleets, jjm.out)
{
  for(iFleet in .an(ageFleets)){
    obs <- .createDataFrame(jjm.out[[paste("pobs_fsh_", iFleet, sep = "")]][,-1],
                            jjm.out[[paste("pobs_fsh_", iFleet, sep = "")]][,1], ages)
    mod <- .createDataFrame(jjm.out[[paste("phat_fsh_", iFleet, sep = "")]][,-1],
                            jjm.out[[paste("phat_fsh_", iFleet, sep = "")]][,1], ages)
    
    if(iFleet == .an(ageFleets)[1]) tot <- cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet, 1], nrow(obs)))
    if(iFleet != .an(ageFleets)[1]) tot <- rbind(tot, cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet,1], nrow(obs))))
  }
  colnames(tot)   <- c("year","obs","age","model","fleet")
  res             <- tot
  
  resids          <- log(res$obs + 1) - log(res$model + 1)
  res$resids      <- resids
  scalar          <- 3/max(resids,na.rm=T)
  residRange      <- range(resids,na.rm=T)
  ikey            <- simpleKey(text=as.character(round(seq(residRange[1],residRange[2],length.out=6),2)),
                               points=T,lines=F,columns = 2)
  ikey$points$cex <- abs(round(seq(residRange[1],residRange[2],length.out=6),2))*scalar
  ikey$points$col <- 1
  ikey$points$pch <- ifelse(round(seq(residRange[1],residRange[2],length.out=6),2)>0,19,1)
  
  pic <- .bubbles(age ~ year|fleet, data = res, allow.multiple = TRUE,
                  xlab = "Years", ylab = "Age", main = "Residuals catch-at-age by fleet",
                  key = ikey, scales = list(alternating = 3))
  
  return(pic)
}

.fit_residualsCatchAtLengthByFleetFUN <- function(lgtFleets, jjm.out, lengths, Nfleets)
{
  for(iFleet in .an(lgtFleets)){
    obs <- .createDataFrame(jjm.out[[paste("pobs_len_fsh_", iFleet, sep = "")]][,-1], 
                            jjm.out[[paste("pobs_len_fsh_", iFleet, sep = "")]][,1], lengths)
    mod <- .createDataFrame(jjm.out[[paste("phat_len_fsh_", iFleet, sep = "")]][,-1],
                            jjm.out[[paste("phat_len_fsh_", iFleet, sep = "")]][,1], lengths)
    
    if(Nfleets == 1){
      if(iFleet == .an(lgtFleets)[1]) tot <- cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet], nrow(obs)))
      if(iFleet != .an(lgtFleets)[1]) tot <- rbind(tot, cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet], nrow(obs))))
    }
    if(Nfleets != 1){
      if(iFleet == .an(lgtFleets)[1]) tot <- cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet,1], nrow(obs)))
      if(iFleet != .an(lgtFleets)[1]) tot <- rbind(tot, cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet,1], nrow(obs))))
    }
  }
  colnames(tot)   <- c("year", "obs", "length", "model", "fleet")
  res             <- tot
  
  resids          <- log(res$obs + 1) - log(res$model + 1)
  res$resids      <- resids
  scalar          <- 3/max(resids, na.rm = TRUE)
  residRange      <- range(resids, na.rm = TRUE)
  ikey            <- simpleKey(text = .ac(round(seq(residRange[1], residRange[2], length.out = 6), 2)),
                               points = TRUE, lines = FALSE, columns = 2)
  ikey$points$cex <- abs(round(seq(residRange[1], residRange[2], length.out = 6), 2))*scalar
  ikey$points$col <- 1
  ikey$points$pch <- ifelse(test = round(seq(residRange[1], residRange[2], length.out = 6),2) > 0, yes = 19, no = 1)
  
  pic <- .bubbles(length ~ year|fleet, data = res, allow.multiple = TRUE,
                  xlab = "Years", ylab = "Length", main = "Residuals catch-at-length by fleet", key = ikey,
                  scales = list(alternating = 3))
  
  return(pic)
}

.fit_ageFitsCatchFUN <- function(ageFleets, jjm.out)
{
  for(iFleet in .an(ageFleets)){
    obs <- .createDataFrame(jjm.out[[paste("pobs_fsh_", iFleet, sep = "")]][,-1],
                            jjm.out[[paste("pobs_fsh_", iFleet, sep = "")]][,1], ages)
    mod <- .createDataFrame(jjm.out[[paste("phat_fsh_", iFleet, sep = "")]][,-1],
                            jjm.out[[paste("phat_fsh_", iFleet, sep = "")]][,1], ages)
    
    if(iFleet == .an(ageFleets)[1]){
      x <- cbind(obs, rep("obs", nrow(obs)), jjm.out$Fshry_names[iFleet])
      colnames(x) <- c("year", "data", "age", "class", "fleet")
      
      y <- cbind(mod, rep("model", nrow(mod)), jjm.out$Fshry_names[iFleet])
      colnames(y) <- c("year", "data", "age", "class", "fleet")
      
      tot <- rbind(x,y)
    }
    
    if(iFleet != .an(ageFleets)[1]){
      x <- cbind(obs, rep("obs", nrow(obs)), jjm.out$Fshry_names[iFleet])
      colnames(x) <- c("year", "data", "age", "class", "fleet")
      
      y <- cbind(mod, rep("model", nrow(mod)), jjm.out$Fshry_names[iFleet])
      colnames(y) <- c("year", "data", "age", "class", "fleet")
      
      res <- rbind(x,y)
      tot <- rbind(tot,res)
    }
  }
  res <- tot
  res$cohort <- (res$year - res$age) %% length(ages) + 1
  
  ikey          <- simpleKey(text = c("Observed", "Predicted"),
                             points = TRUE, lines = FALSE, rect = TRUE, columns = 2)
  ikey$rectangles$alpha <- c(1, 0)
  ikey$rectangles$col   <- "white"
  ikey$rectangles$lty   <- c(1, 0)
  ikey$points$pch       <- c(-1, 19)
  ikey$points$col       <- c("white", "black")
  ikey$points$cex       <- c(0, 1.1)
  
  cols  <- rainbow(length(ages))
  ageFitsCatch <- list()
  for(iFleet in c(jjm.out$Fshry_names)[.an(ageFleets)]){
    tmpres  <- subset(res, fleet == iFleet)
    pic <- xyplot(data ~ age | factor(year), data = tmpres,
                  groups = class,
                  xlab = "Age", ylab = "Proportion at age", main = paste("Age fits", iFleet),
                  key = ikey,
                  as.table = TRUE,
                  panel = function(x, y){
                    idx     <- mapply(seq, from = seq(1, length(y), length(ages)),
                                      to = seq(1, length(y), length(ages)) + (length(ages) - 1))
                    first   <- c(idx[,seq(from = 1, to = dim(idx)[2], by = 3)])
                    second  <- c(idx[,seq(from = 2, to = dim(idx)[2], by = 3)])
                    #cols <- tmpres$cohort[which(is.na(pmatch(tmpres$data,y))==F & is.na(pmatch(tmpres$age,x))==F)]
                    yr      <- names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  <- tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x[first], y[first], horizontal = FALSE, origin = 0, box.width = 1, col = cols[colidx])
                    panel.points(x[second], y[second], pch = 19, col = 1, cex = 0.5)
                  }, scales = list(alternating = 3))
    
    ageFitsCatch[[iFleet]] <- pic
  }
  
  return(ageFitsCatch)
}

.fit_lengthFitsCatchFUN <- function(lgtFleets, jjm.out, lengths)
{
  for(iFleet in .an(lgtFleets)){
    obs <- .createDataFrame(jjm.out[[paste("pobs_len_fsh_", iFleet, sep = "")]][,-1],
                            jjm.out[[paste("pobs_len_fsh_", iFleet, sep = "")]][,1], lengths)
    mod <- .createDataFrame(jjm.out[[paste("phat_len_fsh_", iFleet, sep = "")]][,-1],
                            jjm.out[[paste("phat_len_fsh_", iFleet, sep = "")]][,1], lengths)
    
    if(iFleet == .an(lgtFleets)[1]){
      x <- cbind(obs, rep("obs", nrow(obs)), jjm.out$Fshry_names[iFleet])
      colnames(x) <- c("year", "data", "length", "class", "fleet")
      
      y <- cbind(mod, rep("model", nrow(mod)), jjm.out$Fshry_names[iFleet])
      colnames(y) <- c("year", "data", "length", "class", "fleet")
      
      tot <- rbind(x, y)
    }
    
    if(iFleet != .an(lgtFleets)[1]){
      x <- cbind(obs, rep("obs", nrow(obs)), jjm.out$Fshry_names[iFleet])
      colnames(x) <- c("year", "data", "length", "class", "fleet")
      
      y <- cbind(mod, rep("model", nrow(mod)), jjm.out$Fshry_names[iFleet])
      colnames(y) <- c("year", "data", "length", "class", "fleet")
      
      res <- rbind(x, y)
      tot <- rbind(tot, res)
    }
  }
  res <- tot
  res$cohort <- res$length
  
  ikey                  <- simpleKey(text = c("Observed", "Predicted"),
                                     points = TRUE, lines = FALSE, rect = TRUE, columns = 2)
  ikey$rectangles$alpha <- c(1, 0)
  ikey$rectangles$col   <- "white"
  ikey$rectangles$lty   <- c(1, 0)
  ikey$points$pch       <- c(-1, 19)
  ikey$points$col       <- c("white", "black")
  ikey$points$cex       <- c(0, 1.1)
  
  cols  <- rainbow(length(lengths))
  lengthFitsCatch <- list()
  for(iFleet in c(jjm.out$Fshry_names)[.an(lgtFleets)]){
    tmpres  <- subset(res, fleet == iFleet)
    pic <- xyplot(data ~ length | factor(year), data = tmpres,
                  groups = class,
                  xlab = "Length", ylab = "Proportion at length", main = paste("Length fits", iFleet),
                  key = ikey,
                  as.table = TRUE,
                  panel = function(x,y){
                    idx     <- mapply(seq, from = seq(1, length(y), length(lengths)),
                                      to = (seq(from = 1, to = length(y), by = length(lengths)) + length(lengths) - 1))
                    first   <- c(idx[,seq(1, dim(idx)[2], 3)])
                    second  <- c(idx[,seq(2, dim(idx)[2], 3)])
                    #cols <- tmpres$cohort[which(is.na(pmatch(tmpres$data,y))==F & is.na(pmatch(tmpres$age,x))==F)]
                    yr      <- names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  <- tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x[first], y[first], horizontal = FALSE, origin = 0, box.width = 1, col = cols[colidx])
                    panel.points(x[second], y[second], col = 1, pch = 19, cex = 0.25)
                  })
    
    lengthFitsCatch[[iFleet]] <- pic
  }
  
  return(lengthFitsCatch)
}

.fit_predictedObservedCatchesByFleetFUN <- function(Nfleets, cols, jjm.out)
{
  for(iFleet in 1:Nfleets){
    res <- data.frame(year = jjm.out$Yr,obs = jjm.out[[paste("Obs_catch_", iFleet,sep="")]],
                      model = jjm.out[[paste("Pred_catch_", iFleet, sep = "")]],
                      fleet = jjm.out$Fshry_names[iFleet])
    
    if(iFleet == 1) tot <- rbind(cbind(res$year, res$obs, .ac(res$fleet), rep("obs", nrow(res))),
                                 cbind(res$year, res$model, .ac(res$fleet), rep("model", nrow(res))))
    if(iFleet != 1) tot <- rbind(tot, rbind(cbind(res$year, res$obs, .ac(res$fleet), rep("obs",nrow(res))),
                                            cbind(res$year, res$model, .ac(res$fleet), rep("model", nrow(res)))))
  }
  colnames(tot) <- c("year", "data", "fleet", "classing")
  res <- data.frame(tot, stringsAsFactors = FALSE)
  res$year <- .an(.ac(res$year))
  res$data <- .an(.ac(res$data))
  
  ikey                  <- simpleKey(text = c("Observed","Predicted"),
                                     points = FALSE, lines = TRUE, rect = TRUE, columns = 2)
  ikey$rectangles$alpha <- c(1,0)
  ikey$rectangles$col   <- "grey"
  ikey$rectangles$lty   <- c(1,0)
  ikey$lines$lty        <- c(0,1)
  ikey$lines$col        <- 1
  ikey$lines$lwd        <- 3
  
  pic <- xyplot(data ~ year | as.factor(fleet), data = res,
                groups = as.factor(classing),
                key = ikey,
                main = "Predicted and observed catches by fleet",
                xlab = "Years", ylab = "Thousand tonnes",
                panel = function(x, y){
                  first = 1:length(jjm.out$Yr)
                  second = (length(jjm.out$Yr) + 1):(length(jjm.out$Yr)*2)
                  panel.grid(h = -1, v = -1)
                  panel.barchart(x[first], y[second], horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                  panel.xyplot(x[first], y[second], type = "l", lwd = 5, col = 1, lty = 1)
                })
  
  return(pic)
}

.fit_predictedObservedIndicesFUN <- function(Nsurveys, jjm.out)
{
  for(iSurvey in 1:Nsurveys){
    if(any(jjm.out$Yr %in% jjm.out[[paste("Obs_Survey_", iSurvey,sep="")]][,1] == TRUE)){
      addToDF <- jjm.out$Yr[which(!jjm.out$Yr %in% jjm.out[[paste("Obs_Survey_", iSurvey, sep = "")]][,1])]
      addToDF <- as.data.frame(rbind(cbind(addToDF, NA, "model"), 
                                     cbind(addToDF, NA, "obs"),
                                     cbind(addToDF, NA, "sd"),
                                     cbind(addToDF, NA, "stdres"),
                                     cbind(addToDF, NA, "lstdres")))
      colnames(addToDF) <- c("year", "data", "class")
      addToDF$year <- .an(.ac(addToDF$year))
      addToDF$data <- .an(.ac(addToDF$data))
      addToDF$class <- .ac(addToDF$class)
    }
    
    res <- .createDataFrame(jjm.out[[paste("Obs_Survey_", iSurvey, sep = "")]][,-1],
                            jjm.out[[paste("Obs_Survey_", iSurvey, sep = "")]][,1],
                            c("obs", "model", "sd", "stdres", "lstdres"))
    res$class <- .ac(res$class)
    res$data  <- res$data/max(subset(res, class %in% c("model", "obs", "sd"))$data, na.rm = TRUE)
    resSort   <- rbind(res, addToDF)
    resSort   <- orderBy(~year + class, data = resSort)
    
    if(iSurvey == 1)
      tot   <- cbind(resSort, rep(jjm.out$Index_names[iSurvey, 1], nrow(resSort)))
    
    if(iSurvey != 1){
      res2  <- cbind(resSort, rep(jjm.out$Index_names[iSurvey, 1], nrow(resSort)))
      tot   <- rbind(tot, res2)
    }
  }
  
  colnames(tot)               <- c("year", "data", "classing", "surveys")
  tot                         <- tot[duplicated(paste(tot$year, tot$classing, tot$surveys)) == FALSE,]
  tot$data[which(tot$data<0)] <- NA
  res                         <- tot
  
  ikey            <- simpleKey(text = c("Observed", "Predicted"), points = TRUE, lines = TRUE, columns = 2)
  
  ikey$lines$lty  <- c(0, 1)
  ikey$lines$lwd  <- c(0, 2)
  ikey$lines$col  <- c(0, 1)
  ikey$points$pch <- c(19, -1)
  ikey$points$col <- c("grey", "white")
  ikey$points$cex <- 0.9
  
  pic <- xyplot(data ~ year|as.factor(surveys), data = subset(res, classing %in% c("obs", "model", "sd")),
                groups = classing,
                main="Predicted and observed indices",xlab="Years",ylab="Normalized index value",
                key = ikey, ylim = c(-0.2, 2),
                panel = function(...){
                  tmp     <- list(...)
                  first   <- which(tmp$groups[1:length(tmp$x)] == "model")
                  second  <- which(tmp$groups[1:length(tmp$x)] == "obs")
                  third   <- which(tmp$groups[1:length(tmp$x)] == "sd")
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(tmp$x[first], tmp$y[first], type = "l", col = "black", lwd = 3)
                  panel.points(tmp$x[second], tmp$y[second], col = "grey", pch = 19, cex = 0.6)
                  panel.segments(tmp$x[third], c(tmp$y[second] + 1.96*tmp$y[third]),
                                 tmp$x[third], c(tmp$y[second] - 1.96*tmp$y[third]))
                  panel.lines(tmp$x[first], tmp$y[first], col = "black", lwd = 3)
                }, scales = list(alternating = 3, y = list(draw = FALSE)))
  
  return(pic)
}

.fit_ageFitsSurveyFUN <- function(ageSurveys, jjm.out)
{
  for(iSurvey in .an(ageSurveys)){
    obs <- .createDataFrame(jjm.out[[paste("pobs_ind_", iSurvey, sep = "")]][,-1],
                            jjm.out[[paste("pobs_ind_", iSurvey, sep = "")]][,1], ages)
    mod <- .createDataFrame(jjm.out[[paste("phat_ind_", iSurvey, sep = "")]][,-1], 
                            jjm.out[[paste("phat_ind_", iSurvey, sep = "")]][,1], ages)
    
    if(iSurvey == .an(ageSurveys)[1]){
      x <- cbind(obs, rep("obs", nrow(obs)), jjm.out$Index_names[iSurvey])
      colnames(x) <- c("year", "data", "age", "class", "survey")
      
      y <- cbind(mod, rep("model", nrow(mod)), jjm.out$Index_names[iSurvey])
      colnames(y) <- c("year", "data", "age", "class", "survey")
      
      tot <- rbind(x, y)
    }
    
    if(iSurvey != .an(ageSurveys)[1]){
      x <- cbind(obs, rep("obs", nrow(obs)), jjm.out$Index_names[iSurvey])
      colnames(x) <- c("year", "data", "age", "class", "survey")
      
      y <- cbind(mod, rep("model", nrow(mod)), jjm.out$Index_names[iSurvey])
      colnames(y) <- c("year", "data", "age", "class", "survey")
      
      res <- rbind(x, y)
      tot <- rbind(tot, res)
    }
  }
  res <- tot
  res$cohort <- (res$year - res$age) %% length(ages) + 1
  
  ikey                  <- simpleKey(text = c("Observed","Predicted"),
                                     points = TRUE, lines = FALSE, rect = TRUE, columns = 2)
  ikey$rectangles$alpha <- c(1, 0)
  ikey$rectangles$col   <- "white"
  ikey$rectangles$lty   <- c(1, 0)
  ikey$points$pch       <- c(-1, 19)
  ikey$points$col       <- c("white", "black")
  ikey$points$cex       <- c(0, 1.1)
  
  cols  <- rainbow(length(ages))
  ageFitsSurvey <- list()
  for(iSurvey in c(jjm.out$Index_names)[.an(ageFleets)]){
    tmpres <- subset(res, survey == iSurvey)
    pic <- xyplot(data ~ age | factor(year), data = tmpres,
                  groups = class,
                  xlab = "Age", ylab = "Proportion at age", main = paste("Age fits", iSurvey),
                  key = ikey,
                  as.table = TRUE,
                  panel = function(x, y){
                    idx     <- mapply(seq, from = seq(1, length(y), length(ages)),
                                      to = seq(from = 1, to = length(y), by = length(ages)) + (length(ages) - 1))
                    first   <- c(idx[,seq(from = 1, to = dim(idx)[2], by = 3)])
                    second  <- c(idx[,seq(from = 2, to = dim(idx)[2], by = 3)])
                    #cols <- tmpres$cohort[which(is.na(pmatch(tmpres$data,y))==F & is.na(pmatch(tmpres$age,x))==F)]
                    yr      <- names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  <- tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x[first], y[first], horizontal = FALSE, origin = 0, box.width = 1, col = cols[colidx])
                    panel.points(x[second], y[second], pch = 19, col = 1, cex = 0.5)
                  }, scales = list(alternating = 3))
    
    ageFitsSurvey[[iSurvey]] <- pic
  }
  
  return(ageFitsSurvey)
}

.fit_standardizedSurveyResidualsFUN <- function(Nsurveys, jjm.out, cols)
{
  for(iSurvey in 1:Nsurveys){
    if(any(jjm.out$Yr %in% jjm.out[[paste("Obs_Survey_", iSurvey, sep = "")]][,1] == FALSE)){
      addToDF <- jjm.out$Yr[which(!jjm.out$Yr %in% jjm.out[[paste("Obs_Survey_", iSurvey, sep = "")]][,1])]
      addToDF <- data.frame(year = addToDF, obs = NA, model = NA, sd = NA, stdres = NA, lstdres = NA)
      colnames(addToDF) <- c("year", "obs", "model", "sd", "stdres", "lstdres")
      addToDF$year <- .an(.ac(addToDF$year))
    }
    
    res <- .createDataFrame(jjm.out[[paste("Obs_Survey_", iSurvey, sep = "")]][,-1],
                            jjm.out[[paste("Obs_Survey_", iSurvey, sep = "")]][,1],
                            c("obs", "model", "sd", "stdres", "lstdres"))
    res <- cbind(subset(res, class == "obs")[,1:2], 
                 subset(res, class == "model")$data,
                 subset(res, class == "sd")$data,
                 subset(res, class == "stdres")$data,
                 subset(res, class == "lstdres")$data)
    colnames(res) <- c("year", "obs", "model", "sd", "stdres", "lstdres")
    res <- rbind(res, addToDF)
    res <- orderBy(~year, data = res)
    
    if(iSurvey == 1) 
      tot <- cbind(res, rep(jjm.out$Index_names[iSurvey, 1], nrow(res)))
    
    if(iSurvey != 1){
      res2  <- cbind(res, rep(jjm.out$Index_names[iSurvey, 1], nrow(res)))
      tot   <- rbind(tot, res2)
    }
  }
  
  colnames(tot)           <- c("year", "obs", "model", "sd", "stdres", "lstdres", "survey")
  tot                     <- tot[duplicated(paste(tot$year, tot$survey) == FALSE),]
  tot$obs[tot$obs < 0]    <- NA
  tot$obs[tot$model < 0]  <- NA
  tot$obs[tot$sd < 0]     <- NA
  res                     <- tot
  scalar                  <- 3/max(abs(res$lstdres), na.rm = TRUE)
  resRange                <- range(res$lstdres, na.rm = TRUE)
  ikey                    <- simpleKey(text = .ac(round(seq(resRange[1], resRange[2], length.out = 6), 2)),
                                       points = TRUE, lines = FALSE, columns = 2)
  ikey$points$cex         <- abs(round(seq(resRange[1], resRange[2], length.out = 6), 2))*scalar
  ikey$points$col         <- 1
  ikey$points$pch         <- ifelse(test = round(seq(resRange[1], resRange[2], length.out = 6), 2) > 0,
                                    yes = 19, no = 1)
  
  
  pic <- xyplot(lstdres*scalar ~ year | as.factor(survey), data = res,
                xlab = "Years", ylab = "Log residuals", main = "Standardized survey residuals",
                prepanel = function(...) {list(ylim = c(1, 1))},
                layout = c(1, Nsurveys),
                type = "p", col = cols, lwd = 3,
                cex.axis = 1.2, font = 2,
                key = ikey,
                scales = list(y = list(draw = FALSE)),
                panel = function(x, y){
                  panel.grid(v = -1, h = 1)
                  panel.points(x, 1, cex = abs(y), col = ifelse(y > 0, "black", "white"), pch = 19)
                  panel.points(x, 1, cex = abs(y), col = 1, pch = 1)
                })
  
  return(pic)
}

.fit_sdPerInputSeriesFUN <- function(jjm.out)
{
  for(iSDnr in names(jjm.out)[grep("sdnr", names(jjm.out))]){
    dat <- jjm.out[[iSDnr]]
    if(length(grep("age", iSDnr)) > 0 & length(grep("fsh", iSDnr)) > 0)
      iName <- paste("SD_age_", jjm.out$Fshry_names[.an(substr(iSDnr, nchar(iSDnr), nchar(iSDnr)))], sep = "")
    
    if(length(grep("length", iSDnr)) > 0 & length(grep("fsh", iSDnr)) > 0)
      iName <- paste("SD_length_", jjm.out$Fshry_names[.an(substr(iSDnr, nchar(iSDnr), nchar(iSDnr)))], sep = "")
    
    if(length(grep("age", iSDnr)) > 0 & length(grep("ind", iSDnr)) > 0)
      iName <- paste("SD_age_", jjm.out$Index_names[.an(substr(iSDnr, nchar(iSDnr), nchar(iSDnr)))], sep = "")
    
    if(length(grep("length",iSDnr)) > 0 & length(grep("ind", iSDnr)) > 0) 
      iName <- paste("SD_length_", jjm.out$Index_names[.an(substr(iSDnr, nchar(iSDnr), nchar(iSDnr)))], sep = "")
    
    if(iSDnr == names(jjm.out)[grep("sdnr", names(jjm.out))][1]){
      totdat <- cbind(dat, iName)
    }else {
      totdat <- rbind(totdat, cbind(dat, iName))
    }
  }
  
  totdat            <- as.data.frame(totdat)
  colnames(totdat)  <- c("year", "data", "class")
  totdat$year       <- .an(.ac(totdat$year))
  totdat$data       <- .an(.ac(totdat$data))
  res <- totdat
  
  pic <- xyplot(data ~ year | class, data = res, type = "h",
                main = "SD per input series", ylab = "SD", xlab = "Years",
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.abline(h = 1, col = "blue", lty = 3)
                  panel.xyplot(..., col = 1)
                }, scales = list(alternating = 3))
  
  return(pic)
}

.fit_selectivityFisheryByPentadFUN <- function(Nfleets, jjm.out, ages)
{
  for(iFleet in 1:Nfleets){
    res <- .createDataFrame(jjm.out[[paste("sel_fsh_", iFleet, sep = "")]][,-c(1,2)], jjm.out$Yr, ages)
    res <- cbind(res, jjm.out$Fshry_names[iFleet])
    
    if(iFleet == 1) tot <- res
    if(iFleet != 1) tot <- rbind(tot, res)
  }
  colnames(tot) <- c("year", "data", "age", "fleet"); res <- tot
  
  res$cohort <- res$year - res$age
  pic <- xyplot(data ~ age|sprintf("%i's",floor((year+2)/5)*5) * fleet, data = res,
                groups = year, type = "l", as.table = TRUE,
                scale = list(alternating = FALSE),
                main = "Selectivity of the Fishery by Pentad", xlab = "Age", ylab = "Selectivity")
  
  return(pic)
}

.fit_selectivitySurveyByPentadFUN <- function(Nsurveys, jjm.out, ages)
{
  for(iSurvey in 1:Nsurveys){
    res <- .createDataFrame(jjm.out[[paste("sel_ind_", iSurvey, sep = "")]][,-c(1,2)], jjm.out$Yr, ages)
    res <- cbind(res, jjm.out$Index_names[iSurvey])
    
    if(iSurvey == 1) tot <- res
    if(iSurvey != 1) tot <- rbind(tot, res)
  }
  colnames(tot) <- c("year", "data", "age", "survey"); res <- tot
  
  res$cohort <- res$year - res$age
  pic <- xyplot(data ~ age|sprintf("%i's",floor((year+2)/5)*5) * survey, data = res,
                groups = year, type = "l", as.table = TRUE,
                scale = list(alternating = FALSE),
                main = "Selectivity of the survey by Pentad", xlab = "Age", ylab = "Selectivity")
  
  return(pic)
}

.fit_fAtAGeFUN <- function(jjm.out, ages, cols)
{
  res <- jjm.out$TotF
  dimnames(res) <- list(Years = jjm.out$Yr, Age = ages)
  
  pic <- levelplot(t(res), xlab = "Age", ylab = "Years", col.regions = cols, cuts = 10, main = "F at age")
  
  return(pic)
}

.fit_fProportionAtAGeFUN <- function(jjm.out, ages, cols)
{
  res <- .createDataFrame(t(apply(sweep(jjm.out$TotF, 1, rowSums(jjm.out$TotF), "/"), 1, cumsum)),
                          jjm.out$N[,1], class = ages)
  
  pic <- xyplot(data ~ year, data = res, groups = class,
                xlab = "Years", ylab = "Proportion of F at age", main = "F proportion at age",
                type = "l", ylim = c(0, 1),
                auto.key = list(space = "right", points = FALSE, lines = FALSE, col = cols, cex = 1.2),
                panel = function(...){
                  lst <- list(...)
                  idx <- mapply(seq, from = seq(1, length(lst$y), length(lst$y)/length(ages)),
                                to = seq(1, length(lst$y),
                                         length(lst$y)/length(ages)) + (length(lst$y)/length(ages) - 1))
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., col = "white")
                  
                  for(iAge in rev(ages)){
                    panel.polygon(x = c(lst$x[idx[, iAge]], rev(lst$x[idx[,iAge]])),
                                  y = c(rep(0, length(lst$y[idx[,iAge]])), rev(lst$y[idx[,iAge]])),
                                  col = cols[iAge], border = 0)
                  }                  
                }, scales = list(y = list(axs = "i")))
  
  return(pic)
}

.fit_nAtAGeFUN <- function(jjm.out, cols)
{
  res <- .createDataFrame(jjm.out$N[,-1], jjm.out$N[,1], rep("Ns", prod(dim(jjm.out$N[,-1]))))
  pic <- levelplot(t(jjm.out$N[,-1]), xlab = "Age", ylab = "Years", col.regions = cols, cuts = 10, main = "N at age")
  
  return(pic)
}

.fit_nProportionAtAGeFUN <- function(jjm.out, cols, ages)
{
  res <- .createDataFrame(t(apply(sweep(jjm.out$N[,-1], 1, rowSums(jjm.out$N[,-1]), "/"), 1, cumsum)),
                          jjm.out$N[,1], class = ages)
  
  pic <- xyplot(data ~ year, data = res, groups = class,
                xlab = "Years", ylab = "Proportion of N at age", main = "Proportion at age",
                type = "l", ylim = c(0, 1),
                auto.key = list(space = "right", points = FALSE, lines = FALSE, col = cols, cex = 1.2),
                panel = function(...){
                  lst <- list(...)
                  idx <- mapply(seq, from = seq(1, length(lst$y), length(lst$y)/length(ages)),
                                to = seq(1, length(lst$y), length(lst$y)/length(ages)) + (length(lst$y)/length(ages) - 1))
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., col = "white")
                  
                  for(iAge in rev(ages)){
                    panel.polygon(x = c(lst$x[idx[,iAge]], rev(lst$x[idx[,iAge]])),
                                  y = c(rep(0, length(lst$y[idx[,iAge]])), rev(lst$y[idx[,iAge]])),
                                  col = cols[iAge], border = 0)
                  }
                  
                }, scales = list(y = list(axs = "i")))
  
  return(pic)
}

.fit_fisheryMeanAgeFUN <- function(jjm.out, ageFleets)
{
  for(iFleet in .an(ageFleets)){
    res <- data.frame(jjm.out[[paste("EffN_Fsh_", iFleet, sep = "")]][,c(1, 4, 5, 7, 8)])
    colnames(res) <- c("Year", "Obs", "Model", "Obs5", "Obs95")
    
    for(i in 2:5){
      tot <- data.frame(cbind(res[,1], res[,i]))
      tot$class <- names(res)[i]
      tot$Fleet <- jjm.out$Fshry_names[iFleet]
      if(iFleet == .an(ageFleets)[1] & i == 2) total <- tot
      if(iFleet != .an(ageFleets)[1] | i != 2) total <- rbind(total, tot)
    }
  }
  colnames(total) <- c("year", "data", "class", "fleet")
  
  ikey            <- simpleKey(text = c("Observed", "Modelled"),
                               points = TRUE, lines = TRUE, columns = 2)
  ikey$lines$col  <- c("white","black")
  ikey$lines$lwd  <- c(0, 2)
  ikey$lines$lty  <- c(0, 1)
  ikey$lines$pch  <- c(0, 0)
  ikey$points$pch <- c(16, 0)
  ikey$points$col <- c("grey", "white")
  
  pic <- xyplot(data ~ year | fleet, data = total,
                type = "l", lwd = 3, lty = c(1, 3), col = 1,
                ylab = "Age", xlab = "Years", main = "Fishery mean age",
                key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx <- mapply(seq(length(x)/4, length(x), length.out = 4) - length(x)/4 + 1,
                                seq(length(x)/4, length(x), length.out = 4), FUN = seq)
                  obs   <- idx[,1]
                  mod   <- idx[,2]
                  obs5  <- idx[,3]
                  obs95 <- idx[,4]
                  
                  panel.xyplot(x[obs], y[obs], type = "p", col = "grey", pch = 19, cex = 0.6)
                  panel.segments(x[obs], y[obs5], x[obs], y[obs95])
                  panel.xyplot(x[obs], y[mod], type = "l", lwd = 2, col = "black")                    
                })
  
  return(pic)
}

.fit_fisheryMeanLengthFUN <- function(lgtFleets, jjm.out)
{
  for(iFleet in .an(lgtFleets)){
    res <- data.frame(jjm.out[[paste("EffN_Length_Fsh_", iFleet, sep = "")]][,c(1, 4, 5, 7, 8)])
    colnames(res) <- c("Year", "Obs", "Model", "Obs5", "Obs95")
    
    for(i in 2:5){
      tot <- data.frame(cbind(res[,1], res[,i]))
      tot$class <- names(res)[i]
      tot$Fleet <- jjm.out$Fshry_names[iFleet]
      
      if(iFleet == .an(lgtFleets)[1] & i == 2) total <- tot
      if(iFleet != .an(lgtFleets)[1] | i != 2) total <- rbind(total, tot)
    }
  }
  colnames(total) <- c("year", "data", "class", "fleet")
  
  ikey           <- simpleKey(text = c("Observed", "Modelled"),
                              points = TRUE, lines = TRUE, columns = 2)
  ikey$lines$col <- c("white", "black")
  ikey$lines$lwd <- c(0, 2)
  ikey$lines$lty <- c(0, 1)
  ikey$lines$pch <- c(0, 0)
  ikey$points$pch<- c(16, 0)
  ikey$points$col<- c("grey", "white")
  
  pic <- xyplot(data ~ year | fleet, data = total,
                type = "l", lwd = 3, lty = c(1, 3), col = 1,
                ylab = "Length (cm)", xlab = "Years", main = "Fishery mean length",
                key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx <- mapply(seq(length(x)/4, length(x), length.out = 4) - length(x)/4 + 1,
                                seq(length(x)/4, length(x), length.out = 4), FUN = seq)
                  obs   <- idx[,1]
                  mod   <- idx[,2]
                  obs5  <- idx[,3]
                  obs95 <- idx[,4]
                  
                  panel.xyplot(x[obs], y[obs], type = "p", col = "grey", pch = 19, cex = 0.6)
                  panel.segments(x[obs], y[obs5], x[obs], y[obs95])
                  panel.xyplot(x[obs], y[mod], type = "l", lwd = 2, col = "black")
                })
  
  return(pic)
}

.fit_surveyMeanAgeFUN <- function(Nsurveys, jjm.out)
{
  for(iSurvey in 1:Nsurveys){
    res <- data.frame(jjm.out[[paste("EffN_Survey_", iSurvey, sep = "")]][,c(1, 4, 5, 7, 8)])
    if(nrow(res) > 1){
      colnames(res) <- c("Year", "Obs", "Model", "Obs5", "Obs95")
      
      for(i in 2:5){
        tot <- data.frame(cbind(res[,1], res[,i]))
        tot$class <- names(res)[i]
        tot$Survey <- jjm.out$Index_names[iSurvey]
        if(iSurvey == 1 & i == 2) total <- tot
        if(iSurvey != 1 | i != 2) total <- rbind(total, tot)
      }
    }
  }
  colnames(total) <- c("year", "data", "class", "survey")
  
  ikey           <- simpleKey(text = c("Observed", "Modelled"),
                              points = TRUE, lines = TRUE, columns = 2)
  ikey$lines$col <- c("white", "black")
  ikey$lines$lwd <- c(0, 2)
  ikey$lines$lty <- c(0, 1)
  ikey$lines$pch <- c(0, 0)
  ikey$points$pch<- c(16, 0)
  ikey$points$col<- c("grey", "white")
  
  pic <- xyplot(data ~ year | survey, data = total,
                type = "l", lwd = 3, lty = c(1, 3), col = 1,
                ylab = "Age", xlab = "Years", main = "Survey mean age",
                key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx <- mapply(seq(length(x)/4, length(x), length.out = 4) - length(x)/4 + 1,
                                seq(length(x)/4, length(x), length.out = 4),FUN = seq)
                  obs   <- idx[,1]
                  mod   <- idx[,2]
                  obs5  <- idx[,3]
                  obs95 <- idx[,4]
                  
                  panel.xyplot(x[obs], y[obs], type = "p", col = "grey", pch = 19, cex = 0.6)
                  panel.segments(x[obs], y[obs5], x[obs], y[obs95])
                  panel.xyplot(x[obs], y[mod], type = "l", lwd = 2, col = "black")
                }, scales = list(alternating = 3))
  
  return(pic)
}

.fit_summarySheetFUN <- function(jjm.out)
{
  TotCatch <- 0
  for(iFlt in grep("Obs_catch_", names(jjm.out)))
    TotCatch    <- jjm.out[[iFlt]] + TotCatch
  summaryData <- rbind(cbind(jjm.out$Yr, jjm.out$TotBiom[,-1], "Total biomass"),
                       cbind(jjm.out$SSB[which(jjm.out$SSB[,1] %in% jjm.out$Yr), 1],
                             jjm.out$SSB[which(jjm.out$SSB[,1] %in% jjm.out$Yr), -1], "Spawning Stock Biomass"),
                       cbind(jjm.out$Yr, jjm.out$R[,-1], "Recruitment"),
                       cbind(jjm.out$Yr, cbind(rowMeans(jjm.out$TotF[,-1]), rowMeans(jjm.out$TotF[,-1]),
                                               rowMeans(jjm.out$TotF[,-1]), rowMeans(jjm.out$TotF[,-1])),
                             "Fishing mortality"),
                       cbind(jjm.out$Yr, TotCatch, TotCatch, TotCatch, TotCatch, "Catches"))
  
  summaryData <- rbind(cbind(summaryData[,c(1:2, 6)], "point"),
                       cbind(summaryData[,c(1, 4, 6)], "lower"),
                       cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) <- c("year", "data", "class", "estim")
  summaryData <- data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$year <- as.integer(summaryData$year)
  summaryData$data <- as.numeric(summaryData$data)
  
  pic <- xyplot(data ~ year | class, data = summaryData,
                groups = class,
                main = "Summary sheet",
                prepanel = function(...) {list(ylim = range(pretty(c(0, list(...)$y))))},
                layout=c(1, 5),
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  point <- 1:length(jjm.out$Yr)
                  lower <- (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                  upper <- (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                  #catches
                  if(panel.number() == 1){
                    panel.barchart(x[point], y[point], horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                  }
                  #SSB
                  if(panel.number() == 4){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey")
                    panel.xyplot(x[point],y[point],type="l",lwd=6,lty=3,col=1)
                  }
                  #Recruitment
                  if(panel.number() == 3){
                    panel.barchart(x[point], y[point], horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                    panel.segments(x[lower], y[lower], x[lower], y[upper])
                  }
                  #F
                  if(panel.number() == 2) panel.xyplot(x[point], y[point], lwd = 4, lty = 2, type = "l", col = 1)
                  
                  #Total biomass
                  if(panel.number() == 5){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey")
                    panel.xyplot(x[point], y[point], lwd = 4, lty = 1, type = "l", col = 1)
                  }
                },
                scales = list(alternating = 1, y = list(relation = "free", rot = 0)))
  
  return(pic)
}

.fit_summarySheet2FUN <- function(jjm.out)
{
  TotCatch <- 0
  for(iFlt in grep("Obs_catch_", names(jjm.out)))
    TotCatch    <- jjm.out[[iFlt]] + TotCatch
  
  summaryData <- rbind(cbind(jjm.out$Yr, jjm.out$TotBiom[,-1], "Total biomass"),
                       cbind(jjm.out$Yr, cbind(rowMeans(jjm.out$TotF[,-1]), 
                                               rowMeans(jjm.out$TotF[,-1]), 
                                               rowMeans(jjm.out$TotF[,-1]), 
                                               rowMeans(jjm.out$TotF[,-1])), "Fishing mortality"),
                       cbind(jjm.out$Yr, jjm.out$R[,-1], "Recruitment"),
                       cbind(jjm.out$Yr, jjm.out$TotBiom_NoFish[,-1], "Unfished biomass"))
  
  summaryData <- rbind(cbind(summaryData[,c(1:2, 6)], "point"), 
                       cbind(summaryData[,c(1, 4, 6)], "lower"),
                       cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) <- c("year", "data", "class", "estim")
  summaryData <- data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$class <- factor(summaryData$class, ordered = FALSE,
                              levels = c("Unfished biomass", "Recruitment", "Fishing mortality", "Total biomass"))
  summaryData$year <- as.integer(summaryData$year)
  summaryData$data <- as.numeric(summaryData$data)
  
  pic <- xyplot(data ~ year | class, data = summaryData,
                groups = class,
                main = NA,
                prepanel = function(...) {list(ylim = range(pretty(c(0, list(...)$y))))},
                layout = c(1, 4),
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  point <- 1:length(jjm.out$Yr)
                  lower <- (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                  upper <- (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                  
                  if(panel.number() == 1){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey")
                    panel.xyplot(x[point], y[point], lwd = 4, lty = 1, type = "l", col = 1)
                  }
                  
                  if(panel.number() == 2){
                    panel.barchart(x[point], y[point], horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                    panel.segments(x[lower], y[lower], x[lower], y[upper])
                  }
                  
                  if(panel.number() == 3){
                    panel.xyplot(x[point], y[point], lwd = 4, lty = 2, type = "l", col = 1)
                  }
                  
                  if(panel.number() == 4){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey")
                    panel.xyplot(x[point], y[point], lwd = 4, lty = 1, type = "l", col = 1)
                  }
                }, scales = list(alternating = 1, y = list(relation = "free", rot = 0)))
  
  return(pic)
}

.fit_uncertaintyKeyParamsFUN <- function(jjm.out)
{
  res <- rbind(data.frame(CV = jjm.out$SSB[,3]/jjm.out$SSB[,2], years = jjm.out$SSB[,1], class = "SSB"),
               data.frame(CV = jjm.out$TotBiom[,3]/jjm.out$TotBiom[,2], years = jjm.out$TotBiom[,1], class = "TSB"),
               data.frame(CV = jjm.out$R[,3]/jjm.out$R[,2], years = jjm.out$R[,1], class = "R"))
  
  pic <- xyplot(CV ~ years, data = res, groups = class,
                auto.key = list(space = "right", points = FALSE, lines = FALSE, type = "l", col = 1:3),
                type = "l", col = 1:3, lwd = 2, main = "Uncertainty of key parameters")
  
  return(pic)
}

.fit_matureInmatureFishesFUN <- function(jjm.out)
{
  N   <- jjm.out$N[,-1]
  Mat <- jjm.out$mature_a
  Wt  <- jjm.out$wt_a_pop
  
  MatureBiom <- rowSums(sweep(N, 2, Mat*Wt, "*"))
  ImmatureBiom <- rowSums(sweep(N, 2, (1 - Mat)*Wt, "*"))
  
  res <- data.frame(rbind(cbind(jjm.out$Yr, MatureBiom, "Mature"),
                          cbind(jjm.out$Yr, ImmatureBiom, "Immature")), stringsAsFactors = FALSE)
  colnames(res) <- c("year", "data", "classing")
  res$data <- as.numeric(res$data)
  res$year <- as.integer(res$year)
  res$classing <- as.factor(res$classing)
  
  ikey           <- simpleKey(text = c("Mature", "Immature"),
                              points = FALSE, lines = TRUE, columns = 2)
  ikey$lines$col <- 1
  ikey$lines$lwd <- 3
  ikey$lines$lty <- c(3, 1)
  ikey$points$col <- "white"
  
  pic <- xyplot(data ~ year, data = res, groups = classing,
                type = "l", lwd = 3, lty = c(1, 3), col = 1,
                ylab = "Biomass in kt", xlab = "Years", main = "Mature - Immature fish",
                key = ikey,
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(...)
                })
  
  return(pic)
}

.fit_stockRecruitmentFUN <- function(jjm.out, cols)
{
  res1 <- data.frame(jjm.out[["Stock_Rec"]][,c(2, 4)])
  res1 <- res1[1:(nrow(res1) - 1),]
  res1$class <- "observed"
  
  res2 <- data.frame(jjm.out[["stock_Rec_Curve"]])
  res2 <- res2[1:(nrow(res2) - 1),]
  res2$class <- "modelled"
  
  res  <- rbind(res1, res2)
  colnames(res) <- c("SSB", "Rec", "class")
  
  ikey           <- simpleKey(text = c("Observed", "Modelled"),
                              points = TRUE, lines = TRUE, columns = 2)
  ikey$lines$col <- c(1, rev(cols)[1])
  ikey$lines$lwd <- c(2, 3)
  ikey$lines$lty <- c(3, 2)
  
  ikey$points$pch <- c(19, 0)
  ikey$points$col <- c("darkgrey", "white")
  
  pic <- xyplot(Rec ~ SSB, data = res, groups = class,
                ylab = "Recruitment", xlab = "Spawning Stock Biomass", main = "Stock Recruitment",
                key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idxobs <- which(res$SSB %in% x & res$class == "observed")
                  idxmod <- which(res$SSB %in% x & res$class == "modelled")
                  panel.xyplot(x[idxobs], y[idxobs], type = "l", lwd = 3, col = 1, lty = 3)
                  panel.points(x[idxobs], y[idxobs], type = "p", cex = 0.6, pch = 19, col = "darkgrey")
                  panel.xyplot(x[idxmod], y[idxmod], type = "l", lwd = 5, col = rev(cols)[1], lty = 2)
                })
  
  return(pic)
}

.fit_fishedUnfishedBiomassFUN <- function(jjm.out)
{
  BnoFish <- jjm.out$TotBiom_NoFish[,2]
  BFish   <- jjm.out$TotBiom[,2]
  res     <- as.data.frame(rbind(cbind(jjm.out$TotBiom[,1], BnoFish, "notfished"),
                                 cbind(jjm.out$TotBiom[,1], BFish, "fished")), stringsAsFactors = FALSE)
  colnames(res) <- c("year", "data", "class")
  res$data      <- .an(res$data)
  res$year      <- .an(res$year)
  
  ikey           <- simpleKey(text = c("Fished", "Unfished"),
                              points = FALSE, lines = TRUE, columns = 2)
  ikey$lines$col <- c(1, 1)
  ikey$lines$lwd <- c(2, 2)
  ikey$lines$lty <- c(1, 3)
  
  pic <- xyplot(data ~ year, data = res, groups = class,
                ylab = "Total biomass", xlab = "Years", main = "Fished vs. unfished biomass",
                key = ikey, col = c(1, 1), lwd = 3, lty = c(1, 3), type = "l",
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(...)
                })
  
  return(pic)
}

.projections_ssbPredictionFUN <- function(jjm.out)
{
  Nfutscen  <- length(grep("SSB_fut_", names(jjm.out)))
  scenarios <- c("F2012 SQ", "F2012 0.75x", "F2012 0.5x", "F2012 0.25x", "F2012 0x")[1:Nfutscen]
  
  for(iScen in 1:length(scenarios)){
    idx <- nrow(get("jjm.out")[["SSB"]][,c(1, 2, 4, 5)])
    tot <- rbind(get("jjm.out")[["SSB"]][-idx,c(1, 2, 4, 5)],
                 get("jjm.out")[[paste("SSB_fut_", iScen, sep = "")]][,c(1, 2, 4, 5)])
    colnames(tot) <- c("year", "SSB", "SSB5", "SSB95")
    
    for(i in 2:4){
      if(iScen == 1 & i == 2){
        totres <- data.frame(cbind(tot[,1], tot[,2]))
        totres$class <- colnames(tot)[i]
        totres$scenario <- scenarios[iScen]
      }
      if(iScen != 1 | i != 2){
        res <- data.frame(cbind(tot[,1], tot[,i]))
        res$class <- colnames(tot)[i]
        res$scenario <- scenarios[iScen]
        totres <- rbind(totres, res)
      }
    }
  }
  colnames(totres) <- c("year", "data", "class", "scenario")
  
  ikey           <- simpleKey(text = scenarios, points = FALSE, lines = TRUE, columns = 2)
  ikey$lines$col <- 1:length(scenarios)
  ikey$lines$lwd <- 4
  ikey$lines$lty <- 1
  
  pic <- xyplot(data ~ year, data = totres, type = "l", groups = scenario,
                ylab = "Spawning Stock Biomass", xlab = "Years", main = "SSB prediction", xlim = c(2000, max(totres$year)),
                key = ikey,
                prepanel = function(...) {list(ylim = c(0, max(totres$data, na.rm = TRUE)))},
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx <- mapply(seq(length(x)/Nfutscen, length(x), length.out = Nfutscen) - length(x)/Nfutscen + 1,
                                seq(length(x)/Nfutscen, length(x), length.out = Nfutscen), FUN = seq)
                  idx2 <- mapply(seq(length(idx[,1])/3, length(idx[,1]), length.out = 3) - length(idx[,1])/3 + 1,
                                 seq(length(idx[,1])/3, length(idx[,1]), length.out = 3), FUN = seq)
                  #scen1 <- idx[,1]; scen2 <- idx[,2]; scen3 <- idx[,3]; scen4 <- idx[,4]; scen5 <- idx[,5]
                  for(iScen in 2:Nfutscen){
                    panel.xyplot(x[idx[,iScen][idx2[,1]]], y[idx[,iScen][idx2[,1]]], type = "l", col = iScen, lwd = 3)
                    iCol  <- col2rgb(iScen)
                    iCol  <- rgb(iCol[1]/255, iCol[2]/255, iCol[3]/255, 0.25)
                    panel.polygon(c(x[idx[,iScen][idx2[,2]]], rev(x[idx[,iScen][idx2[,3]]])),
                                  c(y[idx[,iScen][idx2[,2]]], rev(y[idx[,iScen][idx2[,3]]])), col = iCol, border = iCol)
                    panel.lines(x[idx[,iScen][idx2[,1]]], y[idx[,iScen][idx2[,1]]], col = iScen, lwd = 3)
                  }
                  panel.xyplot(x[idx[,1][idx2[,1]]], y[idx[,1][idx2[,1]]], type = "l", col = 1, lwd = 4)
                  iCol  <- col2rgb(1)
                  iCol  <- rgb(iCol[1]/255, iCol[2]/255, iCol[3]/255, 0.15)
                  panel.polygon(c(x[idx[,1][idx2[,2]]], rev(x[idx[,1][idx2[,3]]])),
                                c(y[idx[,1][idx2[,2]]], rev(y[idx[,1][idx2[,3]]])), col = iCol, border = iCol)
                  panel.lines(x[idx[,1][idx2[,1]]], y[idx[,1][idx2[,1]]], col = 1, lwd = 4)
                })
  
  return(pic)
}

.projections_ssbPredictionFUN <- function(jjm.out)
{
  Nfutscen  <- length(grep("SSB_fut_", names(jjm.out)))
  scenarios <- c("F2012 SQ", "F2012 0.75x", "F2012 0.5x", "F2012 0.25x", "F2012 0x")[1:Nfutscen]
  
  totCatch  <- 0
  for(iFlt in grep("Obs_catch_", names(jjm.out)))
    totCatch <- jjm.out[[iFlt]] + totCatch
  
  totCatch  <- cbind(jjm.out$Yr, totCatch)
  colnames(totCatch) <- c("year", "catch")
  
  for(iScen in 1:length(scenarios)){
    tot <- rbind(totCatch, jjm.out[[paste("Catch_fut_", iScen, sep = "")]])
    colnames(tot) <- c("year", "catch")
    if(iScen == 1){
      totres <- data.frame(tot)
      totres$scenario <- scenarios[iScen]
    }else {
      res <- data.frame(tot)
      res$scenario <- scenarios[iScen]
      totres  <- rbind(totres, res)
    }
  }
  
  colnames(totres) <- c("year", "data", "scenario")
  
  ikey           <- simpleKey(text=scenarios, points = FALSE, lines = TRUE, columns = 2)
  ikey$lines$col <- 1:length(scenarios)
  ikey$lines$lwd <- 4
  ikey$lines$lty <- 1
  
  pic <- xyplot(data ~ year, data = totres, type = "l", groups = scenario,
                ylab = "Catch", xlab = "Years", main = "Catch prediction",
                key = ikey,
                prepanel = function(...) {list(ylim = c(0, max(totres$data, na.rm = TRUE)))},
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx <- mapply(seq(length(x)/Nfutscen, length(x), length.out = Nfutscen) - length(x)/Nfutscen + 1,
                                seq(length(x)/Nfutscen, length(x), length.out = Nfutscen), FUN = seq)
                  #scen1 <- idx[,1]; scen2 <- idx[,2]; scen3 <- idx[,3]; scen4 <- idx[,4]; scen5 <- idx[,5]
                  for(iScen in 2:Nfutscen) panel.xyplot(x[idx[,iScen]], y[idx[,iScen]], type = "l", col = iScen, lwd = 3)
                  panel.xyplot(x[idx[,1]], y[idx[,1]], type = "l", col = 1, lwd = 4)                  
                })
  
  return(pic)
}

.ypr_yieldSsbPerRecruitFUN <- function(jjm.ypr)
{
  res <- rbind(data.frame(cbind(jjm.ypr$F, jjm.ypr$SSB), class = "SSB"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$Yld), class = "Yield"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$Recruit), class = "Recruit"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$SPR), class = "SPR"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$B), class = "Biomass"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$Yld/jjm.ypr$Recruit), class = "YPR"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$SSB/jjm.ypr$Recruit), class = "SpawPR"))
  colnames(res) <- c("F", "data", "class")
  res           <- subset(res, class %in% c("YPR", "SpawPR"))
  
  pic <- xyplot(data ~ F | class, data = res, type = "l",
                main = "Yield and spawing stock biomass per recruit",
                xlab = "Fishing mortality", ylab = "Spawing biomass / Yield per recruit",
                prepanel = function(...) {list(ylim = range(pretty(c(0, list(...)$y))))},
                layout = c(1, 2),
                panel = function(...){
                  lst <- list(...)
                  panel.grid(h = -1, v = -1)
                  if(panel.number() == 1) panel.xyplot(lst$x, lst$y, type = "l", lwd = 3, lty = 1, col = 1)
                  if(panel.number() == 2) panel.xyplot(lst$x, lst$y, type = "l", lwd = 3, lty = 1, col = 1)
                }, scales = list(alternating = 1, y = list(relation = "free", rot = 0)))
  
  return(pic)
}