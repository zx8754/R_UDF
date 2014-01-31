#################### Model Summary functions
udf_fisherSummary <- function(model, Description=NA, Summary=FALSE)
{
  #extract
  P    <- format(model$p.value,scientific=T) #P Value
  OR   <- round(model$estimate,2) #Odds Ratio
  Lo.CI<- round(model$conf.int[1],2) #Lower Confidence Interval
  Up.CI<- round(model$conf.int[2],2) #Upper Confidence Interval
  
  #output as data.frame
  if(Summary)
  {
    result <- 
      as.data.frame(
        cbind(Description, P, paste0(OR," (", Lo.CI,"-", Up.CI,")")),
        stringsAsFactors=F)
    colnames(result) <- c("Description","P","OR-CI.95")
    row.names(result)<-NULL
  } else 
  {
    result <- as.data.frame(
      cbind(Description, P, OR, Lo.CI, Up.CI),
      stringsAsFactors=F)
    row.names(result)<-NULL
  }
  return(result) 
}
