udf_impute2mach <- function(inFile="temp.txt",
                            outFolder="R_output/")
{
  # Workspace ---------------------------------------------------------------
  
  outFileMAP=paste0(outFolder,basename(inFile),".map")
  outFileDOSE=paste0(outFolder,basename(inFile),".dose")
  
  #read Impute2 output
  dat <- readLines(inFile)
  
  # Make MAP file -----------------------------------------------------------
  MAP <- 
    as.data.frame(do.call(
      rbind,
      lapply(dat,
             function(x){
               unlist(strsplit(x," "))[1:5]})))
  #output map file
  write.table(MAP,outFileMAP,quote=FALSE,row.names=FALSE,col.names=FALSE)
  
  # Make DOSE file ----------------------------------------------------------
  #write in chunks - append to file every chunk
  chunk=1000
  for(i in seq(1,length(dat),chunk)){
    colStart <- i
    colEnd <- ifelse(colStart+chunk-1 > length(dat),
                     length(dat),
                     colStart+chunk-1)
    d_chunk <- dat[colStart:colEnd]
    
    #make dosage and write with append
    DOSE <-
      do.call(rbind,
              lapply(d_chunk,
                     function(x){
                       #x<-dat[1]
                       d <- unlist(strsplit(x," "))
                       d <- as.numeric(d[6:length(d)])
                       sapply(seq(1,length(d),3),function(i)
                         d[i]*0 + d[i+1]*1 + d[i+2]*2)
                     }))
    
    #output dosage file
    write.table(DOSE,outFileDOSE,quote=FALSE,
                row.names=FALSE,col.names=FALSE,append=TRUE)
    
    #loop status
    flush.console()
    print(i)
  }
}