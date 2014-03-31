#################### Model Summary functions
udf_glmSummary <- function(model, Description=NA, Summary=FALSE) 
{ 
  #Get LCI, OR, UCI (lower Confidence, odds ratio, upper confidence)
  
  #get summary
  coeffs <- coef(summary(model)) 
  #c("Estimate","Std. Error","z value","Pr(>|z|)")
  
  #extract
  Est  <- round(coeffs[ ,"Estimate"],2) #Estimate
  SE   <- round(coeffs[ ,"Std. Error"],2) #Std Error
  Z    <- round(coeffs[ ,"z value"],2) #Z Value
  P    <- format(coeffs[ ,"Pr(>|z|)"],scientific=T) #P Value
  OR   <- round(exp(Est),2) #Odds Ratio
  Lo.CI<- round(exp(Est - 1.96 * SE),2) #Lower Confidence Interval
  Up.CI<- round(exp(Est + 1.96 * SE),2) #Upper Confidence Interval
  
  #output as data.frame
  if(Summary)
  {
    result <- as.data.frame(
      cbind(Description, P, paste0(OR," (", Lo.CI,"-", Up.CI,")")),
      stringsAsFactors=F)
    result <- result[!grepl("Intercept",rownames(result)),]
    colnames(result) <- c("Description","P","OR (CI.95)")
    row.names(result)<-NULL
  } else 
  {
    result <- as.data.frame(
      cbind(Description, Est, SE, Z, P, OR, Lo.CI, Up.CI),
      stringsAsFactors=F)
  }
  return(result)
}
