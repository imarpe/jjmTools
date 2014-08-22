
###########################################################################
# DEMO SCRIPT
###########################################################################

# Set parameters ----------------------------------------------------------

# Path of JJM repository
reposDir    <- "F:/NvTomus InDemonic/test1/"

# Name of a model
modelName <- "mod4.2"

# Names of models
compareList <- paste0("mod4.", 1:4)


# Reading -----------------------------------------------------------------

# OUTPUT Object
model <- readJJM(path = reposDir, modelName = modelName, type = "output")

# LIST OF OUTPUT Object
lstModel <- readJJM(path = reposDir, modelName = compareList, type = "lstOuts")

# DIAG object
diagPlots <- diagnostics(outputObject = model)



# Print -------------------------------------------------------------------

# OUTPUT object
print(model)

# LIST OF OUTPUT object
print(lstModel)

# DIAG object
print(diagPlots)


# Get and print summaries -------------------------------------------------

# OUTPUT object
sumModel <- summary(model)
sumModel

# LIST OF OUTPUT object
sumList <- summary(lstModel)
sumList

# DIAG object
sumPlots <- summary(diagPlots)
sumPlots


# Get and print plots -----------------------------------------------------

# OUTPUT object
# diagnostic function

# LIST OF OUTPUT object
# filename <- file.path(reposDir, "testPlot.pdf")
# pdf(file = filename)
# 
# plot(lstModel, outputFilename, plotType = "pdf", comparisonType = "time",
#      comparisonParams = list(Slot = "TotBiom", SD = TRUE),
#      height = 29.7/2.54, width = 21/2.54, pointsize = 16, bg = "white")
# 
# dev.off()
# shell.exec(file = filename)

# DIAG object (single plot)
filename <- file.path(reposDir, "testPlot.pdf")
pdf(file = filename)

plot(diagPlots, what = c("input", "fit", "projections", "ypr"))

dev.off()
shell.exec(file = filename)