#################### Model Summary functions
udf_glmSummary <- function(model, Description=NA, Summary=FALSE) 
{ 
  #convert glm to LCI, OR, UCI (lower Confidence, odds ratio, upper confidence)
  
  #get summary
  coeffs <- coef(summary(model)) 
  
  #extract
  Est  <- round(coeffs[ ,1],2) #Estimate
  SE   <- round(coeffs[ ,2],2) #Std Error
  Z    <- round(coeffs[ ,3],2) #Z Value
  P    <- format(coeffs[ ,4],scientific=T) #P Value
  OR   <- round(exp(coeffs[ ,1]),2) #Odds Ratio
  Lo.CI<- round(exp(coeffs[ ,1] - 1.96 * coeffs[ ,2]),2) #Lower Confidence Interval
  Up.CI<- round(exp(coeffs[ ,1] + 1.96 * coeffs[ ,2]),2) #Upper Confidence Interval
  
  #output as data.frame
  if(Summary)
    {
    result <- as.data.frame(
      cbind(Description, P, paste0(OR," (", Lo.CI,"-", Up.CI,")")),
      stringsAsFactors=F)
    result <- result[!grepl("Intercept",rownames(result)),]
    colnames(result) <- c("Description","P","OR-CI.95")
    row.names(result)<-NULL
    } else 
      {
        result <- as.data.frame(
          cbind(Description, Est, SE, Z, P, OR, Lo.CI, Up.CI),
          stringsAsFactors=F)
        }
  return(result)
  }
