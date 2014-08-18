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
