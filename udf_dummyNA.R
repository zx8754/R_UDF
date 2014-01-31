udf_dummyNA <- function(missing_ratio=0.2)
{
  #set missing ratio
  #return 1 for non missing 
  #return -1 for missing
  sample(c(rep(1,100*(1-missing_ratio)),
           rep(-1,100*missing_ratio)),1)}
