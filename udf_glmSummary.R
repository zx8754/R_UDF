#################### Model Summary functions
udf_glmSummary <- function(model, Description=NA, Summary=FALSE) 
{ 
  #Get LCI, OR, UCI (lower Confidence, odds ratio, upper confidence)
  
  #get summary
  coeffs <- as.data.frame(coef(summary(model)))
  #c("Estimate","Std. Error","z value","Pr(>|z|)")
  #c("Estimate" ,"Std. Error","t value","Pr(>|t|)")
  
  coeffs$OR   <- round(exp(coeffs$Estimate),2) #Odds Ratio
  coeffs$Lo.CI<- round(exp(coeffs$Estimate - 1.96 * coeffs$"Std. Error"),2) #Lower Confidence Interval
  coeffs$Up.CI<- round(exp(coeffs$Estimate + 1.96 * coeffs$"Std. Error"),2) #Upper Confidence Interval
  
  coefColNames <- colnames(coeffs)
  colnames(coeffs) <- c("Est","SE",
                        toupper(substr(coefColNames[grepl("value",coefColNames)],1,1)),
                        "P","OR","Lo.CI","Up.CI")
  
  coeffs[,c(1,2,3)] <- round(coeffs[,c(1,2,3)],2)
  coeffs$P <- format(coeffs$P,scientific=T)
  
  #output as data.frame
  if(Summary)
  {
    result <- as.data.frame(
      cbind(Description, coeffs$P, paste0(coeffs$OR," (", coeffs$Lo.CI,"-", coeffs$Up.CI,")")),
      stringsAsFactors=F)
    #exclude intercept
    result <- result[-1,]
    colnames(result) <- c("Description","P","OR (CI.95)")
    row.names(result)<-NULL
  } else 
  {
    result <- as.data.frame(
      cbind(Description, coeffs),
      stringsAsFactors=F)
  }
  return(result)
}
