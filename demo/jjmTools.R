###########################################################################
# DEMO SCRIPT
###########################################################################

# Set parameters ----------------------------------------------------------

# Path of JJM repository
reposDir    <- "../JJM/Code/admb/"

# Name of a model
modelName <- "mod2.0"

# Names of models
compareList <- paste0("mod2.", 1:3)


# Run models --------------------------------------------------------------

# Run single model
runJJM(modelName = modelName, path = reposDir)

# Run a list of models
runJJM(modelName = compareList, path = reposDir)


# Reading -----------------------------------------------------------------

# OUTPUT Object
model <- readJJM(modelName = modelName, path = reposDir)

# LIST OF OUTPUT Object
for(i in seq_along(compareList[1:3]))
  assign(paste0("mod", i), readJJM(modelName = compareList[i], path = reposDir))

mod4 <- mod2

# DIAG object
diagPlots <- diagnostics(outputObject = model)


# Combine models ----------------------------------------------------------

mod1234 <- combineModels(mod1, mod2, mod3, mod4)


# Integrating models ------------------------------------------------------

mod12 <- combineStocks(mod1, mod2, model = "giancarlo")


# Print -------------------------------------------------------------------

# OUTPUT object
print(model)

# LIST OF OUTPUT object
print(mod1234)

# DIAG object
print(diagPlots)


# Get and print summaries -------------------------------------------------

# OUTPUT object
sumModel <- summary(model)
sumModel

# LIST OF OUTPUT object
sumList <- summary(mod1234)
sumList

# DIAG object
sumPlots <- summary(diagPlots)
sumPlots


# Get and print plots -----------------------------------------------------

plot(diagPlots$fit$summarySheet2)
