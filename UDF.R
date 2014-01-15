#################### RMH cleaning functions
udf_simpleCap <- function(x) {
  #input: "abs add too"
  #output: "Abs Add Too"
  s <- strsplit(tolower(x), " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")}

udf_FullnameBreakdown <- function(x){
  #input: "RADFORD,MR JOHN ALFRED"
  #output: c("Mr","John Alfred","Radford")
  Fullname<-strsplit(x,",")[[1]]
  Surname<-Fullname[1]
  Forename<-substr(Fullname[2],regexpr(" ",Fullname[2])[1]+1,nchar(Fullname[2]))
  Title<-substr(Fullname[2],1,regexpr(" ",Fullname[2])[1]-1)
  c(simpleCap(Title),
    simpleCap(Forename),
    simpleCap(Surname))}

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

#################### Common Odds Ratio
udf_CommonOddsRatio <- function(d) 
{ 
  # Armitage's trend test - common odds ratio
  # http://ihg.gsf.de/cgi-bin/hw/hwa2.pl
  # 
  # Peter D. Sasieni - "From Genotypes to Genes: Doubling the Sample Size"
  # (Biometrics 1997)
  # 
  # Example data - class=table
  # d <- structure(c(5220L, 810L, 26L, 30L, 0L, 1L),
  #                .Dim = 2:3,
  #                .Dimnames = structure(
  #                  list(
  #                    caco = c("0", "1"), 
  #                    snp1 = c("CC", "CT", "TT")),
  #                  .Names = c("caco","snp1")),
  #                class = "table")
  # snp1
  # caco   CC   CT   TT
  # 0 5220   26    0
  # 1  810   30    1
 
  #Assign Variables
  Control_11 <- d[1,1]
  Control_12 <- d[1,2]
  Control_22 <- d[1,3]
  Case_11 <- d[2,1]
  Case_12 <- d[2,2]
  Case_22 <- d[2,3]
 
  N01 <- Case_11 + Control_11 + Case_12 + Control_12
  N02 <- Case_11 + Control_11 + Case_22 + Control_22
  N12 <- Case_12 + Control_12 + Case_22 + Control_22
 
  #Return OR
  return(
    (Case_12*Control_11/N01
     + Case_22*Control_12/N12
     + 4*(Case_22*Control_11/N02))/
      (Case_11*Control_12/N01
       + Case_12*Control_22/N12
       + 4*(Case_22*Control_11*Case_11*Control_22)**0.5/N02)
  )
}
