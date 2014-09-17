###########################################################################
# INTERNAL FUNCTIONS
###########################################################################

# .diagnostic function -----------------------------------------------------
.diagnostics <- function(jjm.out, jjm.in, jjm.ypr){
  
  # Get model name
  model <- jjm.out$info$model
  jjm.out <- jjm.out$output
  
  #- Generic attributes of the stock assessment
  Nfleets   <- length(c(jjm.out$Fshry_names))
  Nsurveys  <- length(c(jjm.out$Index_names))
  ages      <- jjm.in$ages[1]:jjm.in$ages[2]
  lengths   <- jjm.in$lengths[1]:jjm.in$lengths[2]
  
  #- Get the age-structured fleets and length-structured fleets out
  if(length(grep("pobs_fsh_", names(jjm.out))) > 0){
    ageFleets <- unlist(strsplit(names(jjm.out)[grep("pobs_fsh_", names(jjm.out))], split = "_"))
    ageFleets <- ageFleets[seq(3, length(ageFleets), 3)]
  } else { ageFleets <- 0}
  
  if(length(grep("pobs_len_fsh_", names(jjm.out))) > 0){
    lgtFleets <- unlist(strsplit(names(jjm.out)[grep("pobs_len_fsh_", names(jjm.out))], split = "_"))
    lgtFleets <- lgtFleets[seq(4, length(lgtFleets), 4)]
  } else {lgtFleets <- 0}
  
  #- Get the age-structured surveys and length-structured surveys out
  if(length(grep("pobs_ind_", names(jjm.out))) > 0){
    ageSurveys <- unlist(strsplit(names(jjm.out)[grep("pobs_ind_", names(jjm.out))], "_"))
    ageSurveys <- ageSurveys[seq(3, length(ageSurveys), 3)]
  } else {ageSurveys <- 0}
  
  if(length(grep("pobs_len_ind_", names(jjm.out))) > 0){
    lgtSurveys <- unlist(strsplit(names(jjm.out)[grep("pobs_len_ind_", names(jjm.out))], "_"))
    lgtSurveys <- lgtSurveys[seq(4, length(lgtSurveys), 4)]
  } else {lgtSurveys <- 0}
  
  
  # Plots of the INPUT data
  allPlots <- list()
  
  inputPlots <- list()
  
  # 1: Weight in the fishery by fleet
  inputPlots$weightFishery <- .input_weightFisheryFUN(Nfleets, jjm.out, ages,
                                                      lwd = 1, xlab = "Years", ylab = "Weight",
                                                      main = "Weight at age in the fishery",
                                                      scales = list(alternating = 3))
  
  # 2: Weight at age in the survey
  inputPlots$weightAge <- .input_weightAgeFUN(Nsurveys, jjm.out, ages,
                                              type = "l", lwd = 1,
                                              xlab = "Years", ylab = "Weight", main = "Weight at age in the survey",
                                              auto.key = list(space = "right", points = FALSE, lines = TRUE, type = "b"),
                                              scales = list(alternating = 3))
  
  
  # 3: Weight by cohort in the fleet  
  inputPlots$weightByCohortFleet <- .input_weightByCohortFleetFUN(Nfleets, jjm.out, ages,
                                                                  type = "b", lwd = 1, pch = 19, cex = 0.6,
                                                                  xlab = "Age", ylab = "Weight",
                                                                  main = "Weight at age by cohort in the fleet",
                                                                  auto.key = list(space = "right", points = FALSE,
                                                                                  lines = TRUE, type = "b"), 
                                                                  scales = list(alternating = 3))
  
  # 4: Weight by cohort in the survey  
  inputPlots$weightByCohortSurvey <- .input_weightByCohortSurveyFUN(Nsurveys, jjm.out, ages,
                                                                    type = "l", lwd = 1, pch = 19, cex = 0.6,
                                                                    xlab = "Age", ylab = "Weight",
                                                                    main = "Weight at age by cohort in the survey",
                                                                    auto.key = list(space = "right", points = FALSE,
                                                                                    lines = TRUE, type = "b"),
                                                                    scales = list(alternating = 3))
  
  # 5: Age composition of the catch
  if(.an(ageFleets)[1] != 0){
    inputPlots$ageFleets1 <- .input_ageFleetsFUN(jjm.in, ageFleets, ages,
                                                 main = "Age composition in fleets", 
                                                 as.table = TRUE, ylab = "Proportion at age")    
    
    cols <- rev(heat.colors(11))
    inputPlots$ageFleets2 <- .input_ageFleets2FUN(jjm.in, ageFleets, cols, ages,
                                                  main = "Age composition in fleets",
                                                  zlab = "", ylab = "")    
    
    cols       <- rainbow(length(ages))
    inputPlots$ageFleetsPlots <- .input_ageFleetsPlotsFUN(jjm.in, ages, cols, ageFleets,
                                                          xlab = "Age", ylab = "Proportion at age")
  }
  
  # 6: Age composition of the survey
  if(length(which(jjm.in$Inumageyears > 0)) > 0){
    inputPlots$ageCompositionSurvey1 <- .input_ageCompositionSurvey1FUN(jjm.in, ages,
                                                                        scales = list(rotation = 90,
                                                                                      alternating = 3,
                                                                                      y = list(axs = "i")),
                                                                        horizontal = FALSE, strip = FALSE, 
                                                                        strip.left = strip.custom(style = 1),
                                                                        main = "Age composition in surveys", 
                                                                        ylab = "Proportion at age")
    
    cols  <- rev(heat.colors(11))
    inputPlots$ageCompositionSurvey2 <- .input_ageCompositionSurvey2FUN(jjm.in, cols, ages,
                                                                        main = "Age composition in surveys",
                                                                        zlab = "", ylab = "",
                                                                        scales = list(rotation = 90, alternating = 3))
  }
  
  # 7: Weight in the population
  inputPlots$weightPopulation <- .input_weightPopulationFUN(jjm.in, ages,
                                                            xlab = "Age", ylab = "Weight (kg)",
                                                            main = "Weight in the stock")
  
  # 8: Maturity at age in the population  
  inputPlots$maturityPopulation <- .input_maturityPopulationFUN(jjm.in, ages,
                                                                xlab = "Age", ylab = "Proportion mature",
                                                                main = "Maturity in the stock")
  
  # 9: Length composition of the catch
  if(.an(lgtFleets)[1] != 0){
    cols  <- rev(heat.colors(11))
    inputPlots$lengthComposition1 <- .input_lengthComposition1FUN(cols, lgtFleets, jjm.in, lengths,
                                                                  zlab = "", ylab = "")    
    
    inputPlots$lengthComposition2 <- .input_lengthComposition2FUN(lgtFleets, jjm.in, lengths,
                                                                  xlab = "Length", ylab = "Proportion at length")
  }
  
  # Plots of the fit of the catch data
  fitPlots <- list()
  
  # 9a: Trend in catch
  fitPlots$totalCatch <- .fit_totalCatchFUN(Nfleets, jjm.out,
                                            xlab = "Years", ylab = "Catch in kt", main = "Total catch", 
                                            scales = list(y = list(axs = "i")))
  
  #9b: trends in catch by fleet as polygon
  if(Nfleets > 1){
    fitPlots$totalCatchByFleet <- .fit_totalCatchByFleetFUN(jjm.out, Nfleets,
                                                            xlab = "Years", ylab = "Catch by fleet in kt", 
                                                            main = "Total catch by fleet",
                                                            scales=list(y = list(axs = "i")))
  }
  
  # 10: Log residual total catch by fleet
  fitPlots$catchResidualsByFleet <- .fit_catchResidualsByFleetFUN(Nfleets, jjm.out,
                                                                  xlab = "Years", ylab = "Residuals", 
                                                                  main = "Catch residuals by fleet", 
                                                                  lwd = 3, cex.axis = 1.2, font = 2)
  
  # 11: Absolute residual catch by fleet
  fitPlots$absoluteResidualCatchByFleet <- .fit_absoluteResidualCatchByFleetFUN(Nfleets, jjm.out,
                                                                                scales = list(y = list(draw = FALSE), 
                                                                                              alternating = 3))
  
  # 12a: Proportions catch by age modelled and observed
  if(.an(ageFleets)[1] != 0){        
    fitPlots$residualsCatchAtAgeByFleet <- .fit_residualsCatchAtAgeByFleetFUN(ageFleets, jjm.out, ages,
                                                                              xlab = "Years", ylab = "Absolute residual catch", 
                                                                              main = "Absolute residual catch by fleet",
                                                                              scales = list(alternating = 3))
  }
  
  # 12b: Proportions catch by length modelled and observed
  if(.an(lgtFleets)[1] != 0){    
    fitPlots$residualsCatchAtLengthByFleet <- .fit_residualsCatchAtLengthByFleetFUN(lgtFleets, jjm.out, lengths, Nfleets,
                                                                                    xlab = "Years", ylab = "Age", 
                                                                                    main = "Residuals catch-at-age by fleet",
                                                                                    scales = list(alternating = 3))
  }
  
  # 13a: Fitted age by year by fleet
  
  if(.an(ageFleets)[1] != 0){
    fitPlots$ageFitsCatch <- .fit_ageFitsCatchFUN(ageFleets, jjm.out, ages,
                                                  xlab = "Age", ylab = "Proportion at age", 
                                                  scales = list(alternating = 3))
  }
  
  # 13b: Fitted length by year by fleet
  if(.an(lgtFleets)[1] != 0){
    fitPlots$lengthFitsCatch <- .fit_lengthFitsCatchFUN(lgtFleets, jjm.out, lengths,
                                                        xlab = "Length", ylab = "Proportion at length")
  }
  
  # 14: Absolute catch by fleet modelled and observed
  cols  <- rainbow(11)  
  fitPlots$predictedObservedCatchesByFleet <- .fit_predictedObservedCatchesByFleetFUN(Nfleets, cols, jjm.out,
                                                                                      main = "Predicted and observed catches by fleet",
                                                                                      xlab = "Years", ylab = "Thousand tonnes")
  
  #-----------------------------------------------------------------------------
  #- Plots of the fit of the survey data
  #-----------------------------------------------------------------------------
  
  # 15: Standardized indices observed with error and modelled  
  fitPlots$predictedObservedIndices <- .fit_predictedObservedIndicesFUN(Nsurveys, jjm.out,
                                                                        main = "Predicted and observed indices",
                                                                        xlab = "Years", ylab = "Normalized index value", 
                                                                        ylim = c(-0.2, 2),
                                                                        scales = list(alternating = 3, y = list(draw = FALSE)))
  
  # 15b: Fitted age by year by survey
  if(.an(ageSurveys)[1] != 0){
    fitPlots$ageFitsSurvey <- .fit_ageFitsSurveyFUN(ageSurveys, jjm.out, ages, ageFleets,
                                                    xlab = "Age", ylab = "Proportion at age", 
                                                    scales = list(alternating = 3))
  }
  
  # 16: Log residuals in survey
  cols  <- rainbow(length(ages))  
  fitPlots$standardizedSurveyResiduals <- .fit_standardizedSurveyResidualsFUN(Nsurveys, jjm.out, cols,
                                                                              xlab = "Years", ylab = "Log residuals", 
                                                                              main = "Standardized survey residuals",
                                                                              lwd = 3, cex.axis = 1.2, font = 2,
                                                                              scales = list(y = list(draw = FALSE)))
  
  # 16b: standard deviation of time series variances
  fitPlots$sdPerInputSeries <- .fit_sdPerInputSeriesFUN(jjm.out,
                                                        main = "SD per input series", ylab = "SD", xlab = "Years",
                                                        scales = list(alternating = 3))
  
  #-----------------------------------------------------------------------------
  #- Plots of selectivity in fleet and survey + F's
  #-----------------------------------------------------------------------------  
  # 17: Selectivity at age in the fleet
  fitPlots$selectivityFisheryByPentad <- .fit_selectivityFisheryByPentadFUN(Nfleets, jjm.out, ages,
                                                                            scale = list(alternating = FALSE),
                                                                            main = "Selectivity of the Fishery by Pentad",
                                                                            xlab = "Age", ylab = "Selectivity")
  
  # 18: selecitivity at age in the survey
  fitPlots$selectivitySurveyByPentad <- .fit_selectivitySurveyByPentadFUN(Nsurveys, jjm.out, ages,
                                                                          scale = list(alternating = FALSE),
                                                                          main = "Selectivity of the survey by Pentad",
                                                                          xlab = "Age", ylab = "Selectivity")
  
  # 19a: F at age
  cols  <- rev(heat.colors(11))  
  fitPlots$fAtAGe <- .fit_fAtAGeFUN(jjm.out, ages, cols,
                                    xlab = "Age", ylab = "Years", main = "F at age")
  
  # 19b: Prop F at age
  cols  <- rainbow(length(ages))
  fitPlots$fProportionAtAGe <- .fit_fProportionAtAGeFUN(jjm.out, ages, cols,
                                                        xlab = "Years", ylab = "Proportion of F at age", 
                                                        main = "F proportion at age",
                                                        ylim = c(0, 1), scales = list(y = list(axs = "i")))
  
  #19b: N at age
  cols <- rev(heat.colors(11))  
  fitPlots$nAtAGe <- .fit_nAtAGeFUN(jjm.out, cols,
                                    xlab = "Age", ylab = "Years", main = "N at age")
  
  #19c: Prop N at age
  cols  <- rainbow(length(ages))
  fitPlots$nProportionAtAGe <- .fit_nProportionAtAGeFUN(jjm.out, cols, ages,
                                                        xlab = "Years", ylab = "Proportion of N at age", 
                                                        main = "Proportion at age",
                                                        ylim = c(0, 1), scales = list(y = list(axs = "i")))
  
  #20: Fisheries mean age  
  if(.an(ageFleets)[1] != 0){
    fitPlots$fisheryMeanAge <- .fit_fisheryMeanAgeFUN(jjm.out, ageFleets,
                                                      lwd = 3, lty = c(1, 3), col = 1,
                                                      ylab = "Age", xlab = "Years", main = "Fishery mean age")
  } 
  
  #20: Fisheries mean length  
  if(.an(lgtFleets)[1] != 0){
    fitPlots$fisheryMeanLength <- .fit_fisheryMeanLengthFUN(lgtFleets, jjm.out,
                                                            lwd = 3, lty = c(1, 3), col = 1,
                                                            ylab = "Length (cm)", xlab = "Years", 
                                                            main = "Fishery mean length")
  }
  
  #21: Survey mean age
  if(.an(ageFleets)[1] != 0){
    fitPlots$surveyMeanAge <- .fit_surveyMeanAgeFUN(Nsurveys, jjm.out,
                                                    lwd = 3, lty = c(1, 3), col = 1,
                                                    ylab = "Age", xlab = "Years", main = "Survey mean age",
                                                    scales = list(alternating = 3))
  }
  
  #-----------------------------------------------------------------------------
  #- Plots of stock summary
  #-----------------------------------------------------------------------------
  # 22a: summary sheet with SSB, R, F and Biomass and Catch
  fitPlots$summarySheet <- .fit_summarySheetFUN(jjm.out,
                                                main = "Summary sheet",
                                                scales = list(alternating = 1, y = list(relation = "free", rot = 0)))
  
  # 22b: Summary sheet 2
  fitPlots$summarySheet2 <- .fit_summarySheet2FUN(jjm.out,
                                                  main = NA, 
                                                  scales = list(alternating = 1, y = list(relation = "free", rot = 0)))
  
  # 22b Uncertainties of key parameters
  fitPlots$uncertaintyKeyParams <- .fit_uncertaintyKeyParamsFUN(jjm.out,
                                                                auto.key = list(space = "right", 
                                                                                points = FALSE, 
                                                                                lines = FALSE, 
                                                                                type = "l", col = 1:3),
                                                                col = 1:3, lwd = 2, 
                                                                main = "Uncertainty of key parameters")
  
  # 23: Mature - immature ratio
  cols  <- rainbow(length(ages))
  fitPlots$matureInmatureFishes <- .fit_matureInmatureFishesFUN(jjm.out, 
                                                                lwd = 3, lty = c(1, 3), col = 1,
                                                                ylab = "Biomass in kt", xlab = "Years", 
                                                                main = "Mature - Immature fish")
  
  # 24: Stock-recruitment
  fitPlots$stockRecruitment <- .fit_stockRecruitmentFUN(jjm.out, cols,
                                                        ylab = "Recruitment", xlab = "Spawning Stock Biomass", 
                                                        main = "Stock Recruitment")
  
  # 25: SSB not fished over SSB fished
  fitPlots$fishedUnfishedBiomass <- .fit_fishedUnfishedBiomassFUN(jjm.out,
                                                                  ylab = "Total biomass", xlab = "Years", 
                                                                  main = "Fished vs. unfished biomass",
                                                                  col = c(1, 1), lwd = 3, lty = c(1, 3))
  
  # Plots of catch and ssb projections
  projectionsPlots <- list()
  
  # 25: SSB projections
  projectionsPlots$ssbPrediction <- .projections_ssbPredictionFUN(jjm.out,
                                                                  ylab = "Spawning Stock Biomass", xlab = "Years", 
                                                                  main = "SSB prediction")
  
  # 26: Catch projections    
  projectionsPlots$catchPrediction <- .projections_catchPredictionFUN(jjm.out,
                                                                      ylab = "Catch", xlab = "Years", 
                                                                      main = "Catch prediction")
  
  
  # Plots of yield per recruit and yield biomass
  yprPlots <- list()
  
  yprPlots$yieldSsbPerRecruit <- .ypr_yieldSsbPerRecruitFUN(jjm.ypr,
                                                            main = "Yield and spawing stock biomass per recruit",
                                                            xlab = "Fishing mortality", ylab = "Spawing biomass / Yield per recruit",
                                                            scales = list(alternating = 1, y = list(relation = "free", rot = 0)))
  
  # Join all plots
  plotTree <- list(model = model, input = names(inputPlots), fit = names(fitPlots),
                   projections = names(projectionsPlots), ypr = names(yprPlots))
  allPlots <- list(input = inputPlots, fit = fitPlots, projections = projectionsPlots, ypr = yprPlots,
                   info = plotTree)
  
  class(allPlots) <- "jjm.diag"
  
  return(allPlots)
}

# Code to read in final data ----------------------------------------------
.read.dat <- function(filename){
  ###-Read in the raw datafile-###
  res1      <- scan(file = filename, what = 'numeric', quiet = TRUE, sep = "\n",
                    comment.char = "#", allowEscapes = TRUE)
  res1      <- strsplit(res1, "\t")
  
  #- Get some initial dimensions
  nY        <- length(.an(unlist(res1[[1]][1])):.an(unlist(res1[[2]][1]))) #number of years
  Ys        <- na.omit(.an(unlist(res1[1:2]))) #Years
  nA        <- length(.an(unlist(res1[[3]][1])):.an(unlist(res1[[4]][1]))) #number of ages
  As        <- na.omit(.an(unlist(res1[3:4]))) #Ages
  nL        <- na.omit(.an(unlist(res1[[5]]))) #number of lengths
  Ls        <- na.omit(.an(unlist(strsplit(res1[[6]], "  ")))) #Lengths
  nF        <- na.omit(.an(unlist(res1[[7]]))) #number of fisheries
  
  #- Define storage object
  cols      <- list()
  
  ###-Fill cols with data from res1-###
  
  #-Common data
  cols$years        <- matrix(NA, ncol = 2, nrow = 1 , dimnames = list("years", c("first year", "last year")))
  cols$years[]      <- na.omit(.an(unlist(res1[1:2])))
  cols$ages         <- matrix(NA, ncol = 2, nrow = 1, dimnames = list("age", c("age recruit", "oldest age")))
  cols$ages[]       <- na.omit(.an(unlist(res1[3:4])))
  cols$lengths      <- matrix(NA, ncol = 2, nrow = 1, dimnames = list("lengths", c("first length", "last length")))
  cols$lengths[]    <- na.omit(c(min(Ls), max(Ls)))
  cols$lengthbin    <- numeric()
  
  #-Fisheries data
  cols$Fnum         <- numeric()
  cols$Fnum         <- na.omit(.an(unlist(res1[7])))
  
  #-Start of dynamic rows
  counter           <- 8 #first dynamic row
  
  cols$Fnames       <- list()
  cols$Fnames       <- strsplit(unlist(res1[counter]), "%")[[1]]; counter <- counter + 1
  cols$Fcaton       <- matrix(NA, ncol = nF, nrow = nY,
                            dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  cols$Fcaton[]     <- matrix(na.omit(.an(unlist(res1[counter:(counter + nF - 1)]))),
                            ncol = nF, nrow = nY); counter <- counter + nF
  cols$Fcatonerr    <- matrix(NA, ncol = nF, nrow = nY, dimnames = list(years = Ys[1]:Ys[2],
                                                                     paste("fishery", 1:nF, sep = "")))
  cols$Fcatonerr[]  <- matrix(na.omit(.an(unlist(res1[counter:(counter + nF - 1)]))), 
                             ncol = nF, nrow = nY); counter <- counter + nF
  cols$FnumyearsA   <- matrix(NA, ncol = nF, nrow = 1, dimnames = list("years", paste("Fyears", 1:nF, sep = "")))
  cols$FnumyearsA[] <- na.omit(.an(unlist(res1[counter:(counter+nF-1)]))); counter <- counter + nF
  cols$FnumyearsL   <- matrix(NA, ncol = nF, nrow = 1, dimnames = list("years", paste("Fyears", 1:nF, sep = "")))
  cols$FnumyearsL[] <- na.omit(.an(unlist(res1[counter:(counter + nF - 1)]))); counter <- counter + nF
  cols$Fageyears    <- matrix(NA, ncol = nF, nrow = nY,
                           dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  
  for(iFs in 1:nF){
    if(cols$FnumyearsA[iFs] > 0){
      Fageyears <- c(na.omit(.an(res1[[counter]])))
      wFyears   <- pmatch(Fageyears, cols$years[1]:cols$years[2])
      cols$Fageyears[wFyears, paste("fishery", iFs, sep = "")] <- Fageyears
      counter   <- counter + 1
    }
  }
  cols$Flengthyears <- matrix(NA, ncol = nF, nrow = nY, 
                              dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsL[iFs] > 0){
      Flengthyears  <- c(na.omit(.an(res1[[counter]])))
      lFyears       <- pmatch(Flengthyears, cols$years[1]:cols$years[2])
      cols$Flengthyears[lFyears,paste("fishery", iFs, sep = "")] <- Flengthyears
      counter       <- counter + 1
    }
  }
  
  cols$Fagesample <- matrix(NA, ncol = nF, nrow = nY, 
                            dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsA[iFs] > 0){
      wFyears <- rownames(cols$Fageyears)[which(is.na(cols$Fageyears[,paste("fishery", iFs, sep = "")]) == FALSE)]
      cols$Fagesample[wFyears, paste("fishery", iFs, sep = "")] <- na.omit(.an(unlist(res1[counter])))
      counter <- counter + 1
    }
  }
  
  cols$Flengthsample <- matrix(NA, ncol = nF, nrow = nY, 
                               dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsL[iFs] > 0){
      lFyears <- rownames(cols$Flengthyears)[which(is.na(cols$Flengthyears[,paste("fishery", iFs, sep = "")]) == FALSE)]
      cols$Flengthsample[lFyears, paste("fishery", iFs, sep = "")] <- na.omit(.an(unlist(res1[counter])))
      counter <- counter + 1
    }
  }
  
  cols$Fagecomp <- array(NA, dim = c(nY, nA, nF), 
                         dimnames = list(years = Ys[1]:Ys[2], age = As[1]:As[2], paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsA[iFs] > 0){
      wFyears <- rownames(cols$Fageyears)[which(is.na(cols$Fageyears[,paste("fishery", iFs, sep = "")]) == FALSE)]
      cols$Fagecomp[wFyears,,paste("fishery", iFs, sep = "")] <- 
        matrix(na.omit(.an(unlist(res1[counter:(counter + length(wFyears) - 1)]))), ncol = nA,
               nrow = length(wFyears), byrow = TRUE)
      counter <- counter + length(wFyears)
    }
  }
  
  cols$Flengthcomp <- array(NA, dim = c(nY, nL, nF), 
                            dimnames = list(years = Ys[1]:Ys[2], lengths = Ls[1]:Ls[length(Ls)],
                                            paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsL[iFs] > 0){
      lFyears <- rownames(cols$Flengthyears)[which(is.na(cols$Flengthyears[,paste("fishery", iFs, sep = "")]) == FALSE)]
      cols$Flengthcomp[lFyears,,paste("fishery", iFs, sep = "")] <- 
        matrix(na.omit(.an(unlist(res1[counter:(counter + length(lFyears) - 1)]))),
               ncol = nL, nrow = length(lFyears), byrow = TRUE)
      counter <- counter +length(lFyears)
    }
  }
  
  cols$Fwtatage <- array(NA, dim = c(nY, nA, nF),
                         dimnames = list(years = Ys[1]:Ys[2], age = As[1]:As[2], paste("fishery",1:nF,sep="")))
  for(iFs in 1:nF){
    cols$Fwtatage[,,iFs] <- matrix(na.omit(.an(unlist(res1[counter:(counter + nY - 1)]))),
                                   ncol = nA, nrow = nY, byrow = TRUE)
    counter <- counter + nY
  } 
  
  #-Indices data
  nI <- na.omit(.an(res1[[counter]]))
  cols$Inum <- numeric()
  cols$Inum <- na.omit(.an(res1[[counter]])); counter <- counter + 1
  cols$Inames <- list()
  cols$Inames <- strsplit(res1[[counter]], "%")[[1]] 
  counter <- counter + 1
  cols$Inumyears <- matrix(NA, ncol = nI, nrow = 1, dimnames = list("years", paste("index", 1:nI, sep = "")))
  cols$Inumyears[] <- na.omit(.an(unlist(res1[counter:(counter + cols$Inum - 1)])))
  counter <- counter + cols$Inum
  
  cols$Iyears <- matrix(NA, ncol = nI, nrow = nY, dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumyears[iSu] > 0){
      Iyears <- na.omit(.an(res1[[counter]])); wIyears <- pmatch(Iyears, cols$years[1]:cols$years[2])
      cols$Iyears[wIyears, paste("index", iSu, sep = "")] <- Iyears
      counter <- counter + 1
    }
  }
  
  cols$Imonths <- matrix(NA, ncol = nI, nrow = 1, dimnames = list("month", paste("index", 1:nI, sep = "")))
  cols$Imonths[] <- na.omit(.an(unlist(res1[counter:(counter + cols$Inum - 1)])))
  counter <- counter + cols$Inum
  cols$Index <- matrix(NA, ncol = nI, nrow = nY, dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumyears[iSu] > 0){
      wIyears <- rownames(cols$Iyears)[which(is.na(cols$Iyears[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Index[wIyears, paste("index", iSu, sep = "")] <- na.omit(.an(res1[[counter]]))
      counter <- counter + 1
    }
  }
  
  cols$Indexerr <- matrix(NA, ncol = nI, nrow = nY, dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumyears[iSu] > 0){
      wIyears <- rownames(cols$Iyears)[which(is.na(cols$Iyears[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Indexerr[wIyears, paste("index", iSu, sep = "")] <- na.omit(.an(res1[[counter]]))
      counter <- counter + 1
    }
  }
  
  cols$Inumageyears <- matrix(NA, ncol = nI, nrow = 1, dimnames = list("years", paste("index", 1:nI, sep = "")))
  cols$Inumageyears[] <- na.omit(.an(unlist(res1[counter:(counter + cols$Inum - 1)])))
  counter <- counter + cols$Inum
  cols$Inumlengthyears <- matrix(NA, ncol = nI, nrow = 1, dimnames = list("years", paste("index", 1:nI, sep = "")))
  cols$Inumlengthyears[] <- na.omit(.an(unlist(res1[counter:(counter + cols$Inum - 1)])))
  counter <- counter + cols$Inum
  
  cols$Iyearslength <- matrix(NA, ncol = nI, nrow = nY, 
                              dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumlengthyears[iSu] > 0){
      Iyearslength <- na.omit(.an(res1[[counter]]))
      wIyearslength <- pmatch(Iyearslength, cols$years[1]:cols$years[2])
      cols$Iyearslength[wIyearslength, iSu] <- Iyearslength
      counter <- counter + 1
    }
  }
  
  cols$Iyearsage <- matrix(NA, ncol = nI, nrow = nY, 
                           dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumageyears[iSu] > 0){
      Iyearsage <- na.omit(.an(res1[[counter]])); wIyearsage <- pmatch(Iyearsage, cols$years[1]:cols$years[2])
      cols$Iyearsage[wIyearsage, iSu] <- Iyearsage
      counter <- counter + 1
    }
  }
  
  cols$Iagesample <- matrix(NA, ncol = nI, nrow = nY, 
                            dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumageyears[iSu] > 0){
      wIyears <- rownames(cols$Iyearsage)[which(is.na(cols$Iyearsage[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Iagesample[wIyears,iSu] <- na.omit(.an(res1[[counter]]))
      counter <- counter + 1
    }
  }
  cols$Ipropage <- array(NA, dim = c(nY, nA, nI), 
                         dimnames = list(years = Ys[1]:Ys[2], age = As[1]:As[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumageyears[iSu] > 0){
      wIyears <- rownames(cols$Iyearsage)[which(is.na(cols$Iyearsage[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Ipropage[wIyears,,iSu] <- matrix(na.omit(.an(unlist(res1[counter:(counter + cols$Inumageyears[iSu]-1)]))),
                                            ncol = nA, nrow = cols$Inumageyears[iSu], byrow = TRUE)
      counter <- counter + cols$Inumageyears[iSu]
    }
  }
  
  cols$Ilengthsample <- matrix(NA, ncol = nI, nrow = nY, 
                               dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumlengthyears[iSu] > 0){
      wIyears <- rownames(cols$Iyearslength)[which(is.na(cols$Iyearslength[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Ilengthsample[wIyears, iSu] <- na.omit(.an(res1[[counter]]))
      counter <- counter + 1
    }
  }
  cols$Iproplength <- array(NA, dim = c(nY, nL, nI),
                            dimnames = list(years = Ys[1]:Ys[2], lengths = Ls[1]:Ls[length(Ls)], 
                                            paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumlengthyears[iSu] > 0){
      wIyears <- rownames(cols$Iyearslength)[which(is.na(cols$Iyearslength[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Iproplength[wIyears,,iSu] <- 
        matrix(na.omit(.an(unlist(res1[counter:(counter + cols$Inumlengthyears[iSu] - 1)]))), 
               ncol = nL, nrow = cols$Inumlengthyears[iSu], byrow = TRUE)
      counter <- counter + cols$Inumlengthyears[iSu]
    }
  }
  
  cols$Iwtatage <- array(NA, dim = c(nY, nA, nI),
                         dimnames = list(years = Ys[1]:Ys[2], age = As[1]:As[2], paste("index", 1:nI, sep = "")))
  
  for(iSu in 1:nI){
    cols$Iwtatage[,,iSu] <- matrix(na.omit(.an(unlist(res1[counter:(counter + nY - 1)]))),
                                   ncol = nA, nrow = nY, byrow = TRUE)
    counter <- counter + nY
  }
  
  #-Population data
  cols$Pwtatage <- matrix(NA, ncol = 1, nrow = nA, dimnames = list(age = As[1]:As[2], "weight"))
  cols$Pwtatage[] <- na.omit(.an(res1[[counter]])); counter <- counter + 1
  cols$Pmatatage <- matrix(NA, ncol = 1, nrow = nA, dimnames = list(age = As[1]:As[2], "maturity"))
  cols$Pmatatage[] <- na.omit(.an(res1[[counter]])); counter <- counter + 1
  cols$Pspwn <- numeric()
  cols$Pspwn <- na.omit(.an(res1[[counter]])); counter <- counter + 1
  cols$Pageerr <- matrix(NA, ncol = nA, nrow = nA, dimnames = list(age = As[1]:As[2], age = As[1]:As[2]))
  cols$Pageerr[] <- matrix(na.omit(.an(unlist(res1[counter:(counter + nA - 1)]))),
                           ncol = nA, nrow = nA, byrow = TRUE); counter <- counter + nA
  return(cols)
}

.LikeTable <- function(lstOuts){
  if(class(lstOuts)[1] == 'jjm.output'){
    Name <- lstOuts$output$info$model
    lstOuts <- list(lstOuts$output$output)
  }else {
    Name <- lstOuts$info
    lstOuts <- lstOuts$combined$outputs
  }
  
  names(lstOuts) <- Name
  tab <- cbind(lstOuts[[1]]$Like_Comp_names, do.call(cbind, lapply(lstOuts, function(x){round(x$Like_Comp, 2)})))

  return(tab)
}

.Fut_SSB_SD <- function(lstOuts){
  if(class(lstOuts)[1] == 'jjm.output') {
    Name <- lstOuts$output$info$model
    lstOuts <- list(lstOuts$output$output)
  }else {
    Name <- lstOuts$info
    lstOuts <- lstOuts$combined$output
  }
  
  names(lstOuts) <- Name
  fut <- do.call(rbind, lapply(lstOuts, function(x){do.call(rbind, lapply(x[grep("SSB_fut_", names(x))],
                                                                          function(y){return(y[,1:3])}))}))
  fut <- as.data.frame(fut, stringsAsFactors = FALSE)
  colnames(fut) <- c("year", "SSB", "SD")

  fut$modelscenario <- paste(rep(names(lstOuts),
                                 lapply(lstOuts, function(x) {nrow(x$SSB_fut_1)*length(grep("SSB_fut_", names(x)))})),
                             paste("Scen",
                                   as.vector(do.call(c, lapply(lstOuts, 
                                                               function(x){rep(1:length(grep("SSB_fut_", names(x))),
                                                                               each = nrow(x$SSB_fut_1))}))),
                                   sep = "_"),
                             sep = "_")
  
  return(fut)
}

.SSB_SD <- function(lstOuts){
  if(class(lstOuts)[1] == 'jjm.output') {
    Name <- lstOuts$output$info$model
    lstOuts <- list(lstOuts$output$output)
  }else {
    Name <- lstOuts$info
    lstOuts <- lstOuts$combined$output
  }
  
  names(lstOuts) <- Name
  SSB_SD <- do.call(rbind, lapply(lstOuts, function(x){x$SSB}))
  #  SSB_SD=lstOuts[[1]]$SSB
  SSB_SD <- SSB_SD[,1:3]
  SSB_SD <- as.data.frame(SSB_SD, stringsAsFactors = FALSE)
  colnames(SSB_SD) <- c("year", "SSB", "SD")
  if(length(lstOuts) > 1){
    SSB_SD$model <- rep(names(lstOuts), lapply(lstOuts, function(x){nrow(x$SSB)}))}
  
  return(SSB_SD)
}

.Puntual_SSB_SD <- function(lstOuts,year){
  if(class(lstOuts)[1] == 'jjm.output') {
    Name <- lstOuts$output$info$model
    lstOuts <- list(lstOuts$output$output)
  }else {
    Name <- lstOuts$info
    lstOuts <- lstOuts$combined$output
  }
  
  names(lstOuts) <- Name
  if(year > lstOuts[[1]]$SSB[nrow(lstOuts[[1]]$SSB), 1])
    stop(cat('Year should be lesser than ', lstOuts[[1]]$SSB[nrow(lstOuts[[1]]$SSB), 1]))
  
  ass <- do.call(rbind, lapply(lstOuts, function(x){x$SSB[which(x$SSB[,1] == year), 1:3]}))
  ass <- as.data.frame(ass, stringsAsFactors = FALSE)
  colnames(ass) <- c("year", "SSB", "SD")
  # if(length(lstOuts)>1){ass$modelscenario <- names(lstOuts)}
  return(ass)
}

.integrateModels <- function(...)
{
  modelList <- deparse(substitute(list(...)))
  modelList <- substr(modelList, start = 6, stop = nchar(modelList) - 1)
  modelList <- unlist(strsplit(x = modelList, split = ", "))
  
  # Remove repeated models from modelList 
  modIndex <- NULL
  for(i in modelList)
    modIndex <- c(modIndex, get(i)$info$model)
  
  modelNames <- modIndex[!duplicated(modIndex)]
  modelList <- modelList[!duplicated(modIndex)]
  
  output <- list()
  
  class(output) <- c("jjm.output")
  
  return(output)
}

.compareModels <- function(lstObject, comparisonType = "time", comparisonParams, ...) {
  
  # Prepare model names
  modelNames <- names(lstObject$data)
  
  # Get plots
  if(comparisonType == "time")
  {
    comparisonParams <- .getParameters(pattern = list(SD = FALSE, Sum = NULL, startYear = NULL, legendPos = "topright",
                                                      xlim = NULL, ylim = NULL),
                                       myList = comparisonParams)
    
    .compareTime(lstOuts = lstOuts, Slot = comparisonParams$Slot, SD = comparisonParams$SD,
                Sum = comparisonParams$Sum, startYear = comparisonParams$startYear,
                legendPos = comparisonParams$legendPos, xlim = comparisonParams$xlim,
                ylim = comparisonParams$ylim)
  }else if(comparisonType == "matrix")
  {
    comparisonParams <- .getParameters(pattern = list(SD = FALSE, Sum = NULL, YrInd = NULL, Apply = mean,
                                                      startYear = NULL, legendPos = "topright",
                                                      xlim = NULL, ylim = NULL),
                                       myList = comparisonParams)
    
    .compareMatrix(lstOuts = lstOuts, Slot = comparisonParams$Slot, SD = comparisonParams$SD,
                  Sum = comparisonParams$Sum, YrInd = comparisonParams$YrInd,
                  Apply = comparisonParams$Apply, startYear = comparisonParams$startYear,
                  legendPos = comparisonParams$legendPos, xlim = comparisonParams$xlim,
                  ylim = comparisonParams$ylim)
  }else if(comparisonType == "times")
  {
    comparisonParams <- .getParameters(pattern = list(SD = FALSE, Sum = NULL, YrInd = NULL, Apply = mean,
                                                      startYear = NULL, legendPos = "topright",
                                                      xlim = NULL, ylim = NULL),
                                       myList = comparisonParams)
    
    .compareTimes(lstOuts = lstOuts, Slots = comparisonParams$Slot, SD = comparisonParams$SD,
                 Sum = comparisonParams$Sum, YrInd = comparisonParams$YrInd,
                 Apply = comparisonParams$Apply, startYear = comparisonParams$startYear,
                 legendPos = comparisonParams$legendPos, xlim = comparisonParams$xlim,
                 ylim = comparisonParams$ylim)
  }else stop("Incorrect value for 'comparisonType'.")
  
  return(invisible())
}

.combineStocks <- function(... , model){
  
  modelList <- deparse(substitute(list(...)))
  modelList <- substr(modelList, start = 6, stop = nchar(modelList) - 1)
  modelList <- unlist(strsplit(x = modelList, split = ", "))
  
  # Models in a list called 'list.model'
  list.model = list()
  for(i in 1:length(modelList)){
    list.model[[i]] = get(modelList[i])$output$output
  }
  
  # Analysis to Slots1 ----------------------------------
  
  nModels         = length(list.model)
  
  # Slots 1
  Slots1 = c("SSB","R","TotBiom")
  
  # Correction of SSB, R, TotBiom matrices:
  for(j in 1:length(Slots1)){
    nFilas = numeric(length(list.model))
    for(i in 1:length(list.model)){
      nFilas[i] = nrow(list.model[[i]][[which(names(list.model[[i]]) == Slots1[j])]])
    }
    MinF = which(nFilas == min(nFilas))[1]
    for(i in 1:length(list.model)){
      FYear = list.model[[MinF]][[which(names(list.model[[i]]) == Slots1[j])]][1,1]
      list.model[[i]][[which(names(list.model[[i]]) == Slots1[j])]] = 
        list.model[[i]][[which(names(list.model[[i]]) == Slots1[j])]][which(list.model[[i]][[which(names(list.model[[i]]) == Slots1[j])]][,1] == FYear):nrow(list.model[[i]][[which(names(list.model[[i]]) == Slots1[j])]]), ]
    }
  }
  
  # Empty matrix
  output1 = list(matrix(0, ncol = 5, nrow = nrow(list.model[[1]]$SSB)),
                 matrix(0, ncol = 5, nrow = nrow(list.model[[1]]$R)),
                 matrix(0, ncol = 5, nrow = nrow(list.model[[1]]$TotBiom)))
  
  # Analysis
  for(j in 1:length(Slots1)){
    output1[[j]][,1] = list.model[[1]][[which(names(list.model[[1]]) == Slots1[j])]][,1]
    for(i in 1:nModels){
      output1[[j]][,2] = rowSums(cbind(output1[[j]][,2], list.model[[i]][[which(names(list.model[[i]]) == Slots1[j])]][,2]))
      output1[[j]][,3] = rowSums(cbind(output1[[j]][,3], (list.model[[i]][[which(names(list.model[[i]]) == Slots1[j])]][,3])^2))
    }
    output1[[j]][,3] = sqrt(output1[[j]][,3])
    for(i in 1:nModels){
      output1[[j]][,4] = output1[[j]][,2] - 1.96*output1[[j]][,3]
      output1[[j]][,5] = output1[[j]][,2] + 1.96*output1[[j]][,3]
    }
  }
  # Name to the list 
  names(output1) = Slots1
  
  
  # Analysis to Slots2 ----------------------------------
  
  # Take in account if all models have the same number of scenarios
  nScenarios = numeric(length(list.model))
  for(i in 1:length(list.model)){
    nScenarios[i] = length(grep("Catch_fut_",names(list.model[[i]])))
  }
  
  # if same number of scenarios so:
  if(length(unique(nScenarios)) == 1){
    # Se crean los Slots2
    Slots2 = c(paste0("Catch_fut_",1:unique(nScenarios)), 
               paste0("SSB_fut_",1:unique(nScenarios)))
    
    
    LastYear = max(output1[[3]][,1])
    NYearP = nrow(list.model[[1]]$Catch_fut_1)
    YearsProy = (LastYear+1):(LastYear+NYearP)
    nYearsProy = length(YearsProy)
    
    # Empty matrix
    output2 = matrix(0, ncol = 2, nrow = nYearsProy)
    output2 = replicate(length(Slots2), output2, simplify=F)
    
    # Analysis (only sum)
    for(j in 1:length(Slots2)){
      output2[[j]][,1] = YearsProy # por el momento se pone de frente
      for(i in 1:nModels){
        output2[[j]][,2] = rowSums(cbind(output2[[j]][,2], list.model[[i]][[which(names(list.model[[i]]) == Slots2[j])]][,2]))
      }
    }
    # name to the list
    names(output2) = Slots2
  }
  
  # if not same number of scenarios so:
  if(length(unique(nScenarios)) != 1){
    
    LastYear = max(output1[[3]][,1])
    NYearP = nrow(list.model[[1]]$Catch_fut_1)
    YearsProy = (LastYear+1):(LastYear+NYearP)
    nYearsProy = length(YearsProy)
    
    # the outcome is a NA's matrix
    output2 = replicate(length(Slots2), NA, simplify=F)
  }
  
  # 'output1' is the outcome of analysis in Slots1
  # 'output2' is the outcome of analysis in Slots2
  
  
  # Total Result ----------------------------------------------------
  # Merge both list (output1 and output2)
  output.merge = c(output1, output2)
  
  # Length of the final list (para escribir el _R.rep)
  nNames = length(names(list.model[[1]]))
  
  # names to the final list
  outcome = replicate(nNames, NA, simplify=F)
  names(outcome) = names(list.model[[1]])
  
  #  Merge final list with output.merge
  for(i in 1:length(names(output.merge))){
    outcome[[which(names(output.merge)[i] == names(outcome))]] = output.merge[[i]]
  }
  
  # Final Result
  if(is.null(model)) 
    writeList(outcome, paste0(reposDir, "arc/Combine_R.rep"), format = "P") else 
      writeList(outcome, paste0(reposDir, "arc/", model,"_R.rep"), format = "P")
  
  output <- list(output = list(info = NULL, output = outcome, YPR = NULL),
                 data = list(info = NULL, data = NULL))
  
  class(output) = c("jjm.output")
  
  return(output)
}