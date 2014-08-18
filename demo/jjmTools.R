# Libraries
# library(lattice)
# require(PBSadmb)
# library(RColorBrewer)
# library(doBy)

# Set paths ---------------------------------------------------------------

reposDir    <- "F:/NvTomus InDemonic/test1/"

# Model names
modelName <- "mod4.2"
compareList <- paste0("mod4.", 1:2)

# source('./R/jjm.output-class.R')
# source('./R/jjm.data-class.R')
# source('./R/jjm.lstOuts-class.R')
# source('./R/jjmTools-auxiliar.R')
# source('./R/jjmTools-main.R')
# source('./R/jjmTools-internal.R')


# Reading Data ------------------------------------------------------------

# OUTPUT DATA
model <- readJJM(path = reposDir, modelName = modelName, type = "output")

# INPUT DATA
data  = readJJM(path = reposDir, modelName = modelName, type = 'data')

# LIST OF OUTPUT MODELS
lstModel=readJJM(path = reposDir, modelName = compareList, type = "lstOuts")


# Print data --------------------------------------------------------------

# OUTPUT DATA
print(model)

# INPUT DATA
print(data)

# LIST OF OUTPUT MODELS
print(lstModel)


# Get and print summaries -------------------------------------------------

# OUTPUT DATA (not working, to be tested)
# sumModel <- summary(model)
# sumModel

# INPUT DATA
sumData <- summary(data)
sumData

# LIST OF OUTPUT MODELS
sumList <- summary(lstModel)
sumList


# Plots -------------------------------------------------------------------

# OUTPUT DATA
# diagnostics function

# INPUT DATA
# diagnostics function

# LIST OF OUTPUT MODELS
# compareModels function

# Diagnostics -------------------------------------------------------------

diagnostics(inputObject = data, outputObject = model,
            what = c("input", "fit", "projections", "ypr"))
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
