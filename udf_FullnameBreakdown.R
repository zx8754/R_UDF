#################### RMH cleaning functions
udf_FullnameBreakdown <- function(x){
  #input: "RADFORD,MR JOHN ALFRED"
  #output: c("Mr","John Alfred","Radford")
  #depends on udf_simpleCap function
  Fullname<-strsplit(x,",")[[1]]
  Surname<-Fullname[1]
  Forename<-substr(Fullname[2],regexpr(" ",Fullname[2])[1]+1,nchar(Fullname[2]))
  Title<-substr(Fullname[2],1,regexpr(" ",Fullname[2])[1]-1)
  c(udf_simpleCap(Title),
    udf_simpleCap(Forename),
    udf_simpleCap(Surname))}
