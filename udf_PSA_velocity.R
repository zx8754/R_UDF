udf_PSA_velocity <- function(dat) {
  # About -------------------------------------------------------------------
  # Date: 30/05/2014
  # Calculate PSA Velocity 3 methods:
  #     1. Arithmetic equation of change in PSA over time.
  #     2. Linear regression (LR) analysis, using all PSA values and the
  #        equation.
  #     3. Rate of PSA change using first and last values only.
  #
  # Ref paper:
  # http://www.ncbi.nlm.nih.gov/pubmed/17197071
  # Methods of calculating prostate-specific antigen velocity.
  # Connolly D1, Black A, Murray LJ, Napolitano G, Gavin A, Keane PF.
  
  # Input data.frame format:
  # SampleName   PSA_Date PSA
  # Sample_1 18/07/2011 4.2
  # Sample_2 18/04/2008 2.2
  # Sample_2 08/05/2009 2.8
  # Sample_2 07/04/2010 3.3
  
  # Workspace ---------------------------------------------------------------
  #setwd("C:/Users/tdadaev/Desktop/Projects/Impact/PSAvelocity/R")
  date_prefix <- substr(Sys.time(),1,10)
  
  # Data prep ---------------------------------------------------------------
  #exclude samples with no PSA
  dat <- dat[ !is.na(dat$PSA),]
  #split by sample - list
  dat <- split(dat,dat$SampleName)
  
  # Calculate PSA Velocity - 3 methods --------------------------------------
  PSAV <-
    do.call(rbind,
            #loop through each patient
            lapply(dat,function(ind){
              #ind=as.data.frame(dat[[3]])
              d <- data.frame(SampleName=ind[,1],
                              PSADate=as.Date(ind[,2],"%d/%m/%Y"),
                              PSA=ind[,3])
              #sort by date
              d <- d[order(d$PSADate),]
              
              #if PSA count 1 then no velocity
              if(nrow(d)>1){
                # Velocity-a --------------------------------------------------------------
                # Arithmetic equation
                PSAV_a=
                  1/(nrow(d)-1) * 
                  sum(diff(d$PSA) / 
                        as.numeric(diff.Date(d$PSADate)))
                
                # Velocity-b --------------------------------------------------------------
                # Linear regresssion
                #convert dates to duration
                d$duration <- as.numeric(d$PSADate) - as.numeric(d$PSADate)[1]
                
                #linear model PSA=Slope*Time+Intercept
                xFit <- lm(PSA~duration,data=d)
                #velocity=slope
                PSAV_b=as.numeric(xFit$coefficients[2])
                
                # Velocity-c --------------------------------------------------------------
                # first and last only
                PSAV_c=
                  (d[nrow(d),"PSA"]-d[1,"PSA"]) / 
                  as.numeric(diff.Date(c(d[1,"PSADate"],d[nrow(d),"PSADate"])))
                
              }else{#if there is only one PSA then velocity=NA
                PSAV_a=NA
                PSAV_b=NA
                PSAV_c=NA}
              
              #return results
              data.frame(SampleName=unique(d$SampleName),
                         PSAcnt=nrow(d),
                         #output as per YEAR
                         PSAV_a=PSAV_a*365.242,
                         PSAV_b=PSAV_b*365.242,
                         PSAV_c=PSAV_c*365.242)
            }))
  
  # Return PSA Velocity -----------------------------------------------------
  return(PSAV)
  
}
