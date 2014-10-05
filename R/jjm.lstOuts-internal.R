.detailedIndLikelihood <- function(object){
  # Survey lik
  surfLik <- do.call(cbind, lapply(object$combined$outputs,
                                   function(x){do.call(rbind, x[grep("Survey_Index_", names(x))])}))
  colnames(surfLik) <- object$info
  
  # Age survey lik
  ageSurvLik <- do.call(cbind, lapply(object$combined$outputs,
                                      function(x){do.call(rbind, x[grep("Age_Survey_", names(x))])}))
  colnames(ageSurvLik) <- object$info
  
  return(rbind(surfLik, ageSurvLik))
}


