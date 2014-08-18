# Libraries
library(lattice)
require(PBSadmb)
library(RColorBrewer)
library(doBy)

# Set paths ---------------------------------------------------------------

reposDir    <- "/home/ird/ERICK/PACKAGER/ADMB/jack_mackerel-master"
#codePath    <- file.path(reposDir,"Code/R/")
inputPath   <- file.path(reposDir,"Code/admb/")
outputPath  <- file.path(reposDir,"Code/admb/arc/")
#resultPath  <- file.path(reposDir,"Results/Assessment/")
#setwd(codePath)

# Model names
modelName <- "mod3.2"
dataName <- "mod3.2.dat"
compareList <- paste0("mod3.", 1:2)
###
source('./R/jjm.output-class.R')
source('./R/jjm.data-class.R')
source('./R/jjm.lstOuts-class.R')
source('./R/jjmTools-auxiliar.R')
source('./R/jjmTools-main.R')
source('./R/jjmTools-internal.R')
########################################
# READING DATA
########################################
# READING OUTPUT DATA
model = readJJM(outputPath, modelName)
# READING INPUT DATA
data  = readJJM(inputPath, modelName, 'data')
# READING A LIST OF OUTPUT MODELS
lstModel=readJJM(outputPath, compareList)
# METADATA OF MODEL
class(model)
summary(model)
summary(data)
# PLOTTING DATA
plot(model,data)

#####################################
# END DEMO
#####################################

# 
# 
# mod1 = readJJM("jjm")
# mod2 = readJJM("jjm2")
# datos = readJJM("jjm", type="data")
# 
# diag1 = diagnostics(model1)
# summary(model1, ...) # write all the summaries and tables
# summary(diag1)
# plot(model1) # make all the plots
# plot(model1, what="index") # plot the fit of the indices
# plot(model1, what="SSB") # plot the SSB
# models = combineModels(model1, model2, model3, ...) # make an object to compare models
# plot(models) # make the plots for comparisons
# 
# # clases
# jjm.output # output: print, summary, plot
#   summary.jjm.output # class for summary: print
# jjm.data # data: print, summary, plot
#   summary.jjm.data # class for summary: print 
# jjm.diag # diagnostics: print, summary?, plot      
#   summary.jjm.diag # class for summary: print  
# jjm.multi # combined models: print, summary, plot
#   summary.jjm.multi # class for summary: print  
# 
