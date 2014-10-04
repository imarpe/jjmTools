###########################################################################
# INTERNAL FUNCTIONS
###########################################################################

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
  tab <- do.call(cbind, lapply(lstOuts, function(x){round(x$Like_Comp, 2)}))
  row.names(tab) <- lstOuts[[1]]$Like_Comp_names

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

.combineStocks <- function(... , model){
  
  modelList <- deparse(substitute(list(...)))
  modelList <- substr(modelList, start = 6, stop = nchar(modelList) - 1)
  modelList <- unlist(strsplit(x = modelList, split = ", "))
  
  # Models in a list called 'allModels'
  allModels <- list()
  for(i in 1:length(modelList)){
    allModels[[i]] <- get(modelList[i])$output$output
  }
  
  ###### Analysis to Slots1
  
  nModels <- length(allModels)
  
  # Slots 1
  Slots1 <- c("SSB","R","TotBiom")
  
  # Correction of SSB, R, TotBiom matrices:
  for(j in seq_along(Slots1)){
    nFilas <- numeric(length(allModels))
    for(i in seq_along(allModels)){
      nFilas[i] <- nrow(allModels[[i]][[Slots1[j]]])
    }
    
    minF <- which.min(nFilas)[1]
    for(i in seq_along(allModels)){
      index <- which(names(allModels[[i]]) == Slots1[j])
      FYear <- allModels[[minF]][[index]][1, 1]
      
      temp <- allModels[[i]]
      temp <- temp[[Slots1[j]]][which(temp[[Slots1[j]]][,1] == FYear):nrow(temp[[Slots1[j]]]),]
      
      allModels[[i]][[Slots1[j]]] <- temp
    }
  }
  
  # Empty matrix
  output1 <- list(matrix(0, ncol = 5, nrow = nrow(allModels[[1]]$SSB)),
                  matrix(0, ncol = 5, nrow = nrow(allModels[[1]]$R)),
                  matrix(0, ncol = 5, nrow = nrow(allModels[[1]]$TotBiom)))
  
  # Analysis
  for(j in 1:length(Slots1)){
    output1[[j]][,1] <- allModels[[1]][[Slots1[j]]][,1]
    for(i in seq(nModels)){
      output1[[j]][,2] <- rowSums(cbind(output1[[j]][,2], allModels[[i]][[Slots1[j]]][,2]))
      output1[[j]][,3] <- rowSums(cbind(output1[[j]][,3], (allModels[[i]][[Slots1[j]]][,3])^2))
    }
    
    output1[[j]][,3] <- sqrt(output1[[j]][,3])
    for(i in seq(nModels)){
      output1[[j]][,4] <- output1[[j]][,2] - 1.96*output1[[j]][,3]
      output1[[j]][,5] <- output1[[j]][,2] + 1.96*output1[[j]][,3]
    }
  }
  # Name to the list 
  names(output1) <- Slots1
  
  
  ###### Analysis to Slots2
  
  # Take in account if all models have the same number of scenarios
  nScenarios <- numeric(length(allModels))
  for(i in seq_along(allModels)){
    nScenarios[i] <- length(grep("Catch_fut_", names(allModels[[i]])))
  }
  
  # if same number of scenarios so:
  if(length(unique(nScenarios)) == 1){
    # Create Slots2
    Slots2 <- c(paste0("Catch_fut_", seq(unique(nScenarios))), 
                paste0("SSB_fut_", seq(unique(nScenarios))))
    
    LastYear    <- max(output1[[3]][,1])
    NYearP      <- nrow(allModels[[1]]$Catch_fut_1)
    YearsProy   <- seq(from = (LastYear + 1), to = (LastYear + NYearP))
    nYearsProy  <- length(YearsProy)
    
    # Empty matrix
    output2 <- matrix(0, ncol = 2, nrow = nYearsProy)
    output2 <- replicate(length(Slots2), output2, simplify = FALSE)
    
    # Analysis (only sum)
    for(j in seq_along(Slots2)){
      output2[[j]][,1] <- YearsProy # por el momento se pone de frente
      
      for(i in seq(nModels)){
        output2[[j]][,2] <- rowSums(cbind(output2[[j]][,2],
                                          allModels[[i]][[Slots2[j]]][,2]))
      }
    }
    
    # name to the list
    names(output2) <- Slots2
  } else {
    LastYear    <- max(output1[[3]][,1])
    NYearP      <- nrow(allModels[[1]]$Catch_fut_1) 
    YearsProy   <- seq(from = (LastYear+1), to = (LastYear+NYearP))
    nYearsProy  <- length(YearsProy)
    
    # the outcome is a NA's matrix
    output2 <- replicate(length(Slots2), NA, simplify = FALSE)
  }
  
  # 'output1' is the outcome of analysis in Slots1
  # 'output2' is the outcome of analysis in Slots2
  
  
  ###### Total Result
  # Merge both list (output1 and output2)
  outputMerge <- c(output1, output2)
  
  # Length of the final list (para escribir el _R.rep)
  nNames <- length(names(allModels[[1]]))
  
  # names to the final list
  outcome <- replicate(nNames, NA, simplify = FALSE)
  names(outcome) <- names(allModels[[1]])
  
  #  Merge final list with output.merge
  for(i in seq_along(names(outputMerge))){
    index <- which(names(outputMerge)[i] == names(outcome))
    outcome[[index]] <- outputMerge[[i]]
  }
  
  #Find the directory
  posLoc <- max(gregexpr("/", get(modelList[1])$data$info$file)[[1]])
  fileLocation <- substr(x = get(modelList[1])$data$info$file, start = 1, stop = posLoc)
  
  # Final Result
  if(is.null(model)) 
    writeList(outcome, file.path(fileLocation, "arc/Combine_R.rep"), format = "P") else 
      writeList(outcome, file.path(fileLocation, "arc", paste0(model,"_R.rep")), format = "P")
  
  infoData <- list(file       = modelList,
                   variables  = sum(!is.na(outcome)),
                   year       = c(outcome$TotBiom[1, 1], outcome$TotBiom[nrow(outcome$TotBiom), 1]),
                   age        = NULL, 
                   length     = NULL)
  
  output <- list(info   = list(model = NULL),
                 output = list(info = NULL, output = outcome, YPR = NULL),
                 data   = list(info = infoData, data = NULL))
  
  class(output) = c("jjm.output")
  
  return(output)
}

.compareTime <-  function(lstOuts, Slot = "TotBiom", SD = FALSE, Sum = NULL, startYear = NULL, legendPos = "topright",
                          ylim = NULL, grid = TRUE, yFactor = 1, main = NA, ylab = Slot,
                          linesCol = NULL, lwd = 1, lty = 1, ...){
  
  dat <- lapply(lstOuts$combined$outputs, function(x){return(x[[Slot]])})
  nms <- names(dat)
  
  if(!is.null(Sum)){
    nms <- c(nms, paste(Sum[1], "+", Sum[2], sep = ""))}
  
  nD <- length(dat)
  if(is.null(startYear)){
    xrange <- range(unlist(lapply(dat, function(x){x[,1]})), na.rm = TRUE)
  }else {
    xrange <- c(startYear, range(unlist(lapply(dat, function(x){x[,1]})), na.rm = TRUE)[2])
  }
  
  dat <- lapply(dat, function(x){idx <- which(x[,1] %in% xrange[1]:xrange[2]); return(x[idx,])})
  
  if(is.null(ylim)) 
    ylim <- range(pretty(range(unlist(lapply(dat, function(x){x[,4:5]})), na.rm = TRUE)))
  
  if(is.null(linesCol))
    linesCol <- rainbow(nD) else
      linesCol <- rep(linesCol, length.out = nD)
  
  if(is.na(main))
    mar <- c(2, 4, 0.1, 0.1) else
      mar <- c(2, 4, 2, 0.1)
  
  par(mar = mar, xaxs = "i")
  
  plot(x = dat[[1]][,1], y = dat[[1]][,2]*yFactor, col = linesCol[1], type = "l", main = main,
       ylim = ylim, xlim = xrange, axes = FALSE, lwd = lwd, lty = lty, ylab = ylab, ...)
  
  if(grid) grid()
  axis(1)
  axis(2, las=2)
  
  for(i in 2:nD)
    lines(x = dat[[i]][,1], y = dat[[i]][,2]*yFactor, col = linesCol[i], lwd = lwd, lty = lty)
  
  if(!is.null(Sum)){
    idx1    <- which(nms == Sum[1])
    idx2    <- which(nms == Sum[2])
    datsum  <- colSums(rbind(dat[[idx1]][,2], dat[[idx2]][,2]))
    
    lines(x = dat[[idx1]][,1], y = datsum*yFactor, col = nD + 1, lwd = lwd, lty = lty)
  }
  
  if(SD){
    for(i in 1:nD){
      polygon(x = c(dat[[i]][,1], rev(dat[[i]][,1])),
              y = c(dat[[i]][,4], rev(dat[[i]][,5]))*yFactor,
              col = adjustcolor(linesCol[i], alpha.f = 0.1), border = 0)
    }
  }
  
  legend(legendPos, legend = nms, col = linesCol, lwd = lwd, lty = lty, box.col = NA)
  box()
  
  return(invisible())
}

.compareMatrix <- function(lstOuts, Slot = 'TotF', Sum = NULL, YrInd = FALSE, Apply = "mean", startYear = NULL,
                           legendPos = "topright", ...){
  
  lst     <- list(...)
  
  dat     <- lapply(lstOuts$combined$outputs,function(x){return(x[[Slot]])})
  nms     <- names(dat); if(!is.null(Sum)){nms <- c(nms,paste(Sum[1],"+",Sum[2],sep=""))}
  
  nD      <- length(dat)
  if(!YrInd){
    for(i in 1:nD){
      dat[[i]] = cbind(lstOuts$combined$outputs[[i]]$Yr,dat[[i]])
    }
  }
  
  for(i in 1:nD) dat[[i]] <- cbind(dat[[i]][,1],apply(dat[[i]][,-1],1, get(Apply)))
  
  if(is.null(startYear)){xrange <- range(unlist(lapply(dat,function(x){x[,1]})),na.rm=T)
  } else { xrange <- c(startYear,range(unlist(lapply(dat,function(x){x[,1]})),na.rm=T)[2])}
  
  dat     <- lapply(dat,function(x){idx <- which(x[,1] %in% xrange[1]:xrange[2]); return(x[idx,])})
  
  yrange=range(pretty(range(unlist(lapply(dat,function(x){x[,2]})),na.rm=T)))
  if(is.null(lst$ylim)==F) yrange <- lst$ylim
  if(is.null(lst$xlim)==F) xrange <- lst$xlim
  if(!is.null(Sum)){
    idx1  <- which(nms==Sum[1])
    idx2  <- which(nms==Sum[2])
    datsum<- colSums(rbind(dat[[idx1]][,2],dat[[idx2]][,2]))
    yrange=range(pretty(range(c(unlist(lapply(dat,function(x){x[,2]})),datsum))))
  }
  plot(x=dat[[1]][,1],y=dat[[1]][,2],type="l",lwd=2,xlab="Years",ylab=Slot,
       xlim=xrange,ylim=yrange,axes=T)
  
  grid(); box()
  for(i in 1:nD)
    lines(x=dat[[i]][,1],y=dat[[i]][,2],col=i,lwd=2)
  if(!is.null(Sum)){
    lines(x=dat[[idx1]][,1],y=datsum,col=nD+1,lwd=2)}
  
  legend(legendPos,legend=c(nms),col=1:length(nms),lwd=2,lty=1,box.lty=0,bty="n")
  box()
  
  return(invisible())
}

.getParameters <- function(patternList, myList) {
  
  list3 <- NULL
  for(i in seq_along(patternList))
    if(names(patternList)[i] %in% names(myList))
      list3[[i]] <- myList[[i]] else
        list3[[i]] <- patternList[[i]]
  
  return(list3)
}

.getResume <- function(typePlot, object) {
  formulaVector <- NULL
  for(i in names(object[[typePlot]]))
  {
    if(class(object[[typePlot]][[i]]) == "list")
    {
      result <- c(name = i, type = "List of plots")
    }else
    {
      result <- c(name = i, type = "Single plot")
    }
    
    formulaVector <- rbind(formulaVector, result)
  }
  
  return(formulaVector)
}

.getPath <- function(path)
{
  firstChar <- substr(path, 1, 1)
  firstSecondChar <- substr(path, 1, 2)
  if(firstSecondChar != "..")
  {
    if(firstChar == "/" | firstChar == "" | firstChar == ".")
      path <- file.path(getwd(), path)
  }
  else
  {
    firstDir <- unlist(strsplit(getwd(), split = .Platform$file.sep)[[1]])
    secondDir <- unlist(strsplit(path, split = .Platform$file.sep)[[1]])
    m <- gregexpr(pattern = paste("..",.Platform$file.sep,sep=""), text = path, fixed = TRUE)
    n <- length(unlist(regmatches(path, m)[[1]]))
    firstDir <- rev(rev(firstDir)[-(1:n)])
    secondDir <- regmatches(path, m, invert = TRUE)[[1]][-(1:n)]
    path <- paste(firstDir, sep=.Platform$file.sep, collapse = '/')
    path <- file.path(path, secondDir)
  }
  
  return(path)
}

.getPath2 <- function(path, pattern, target)
{
  output <- list.files(path = path, recursive = TRUE, pattern = pattern)
  output <- output[grep(x = output, pattern = target)]
  
  return(output)
}