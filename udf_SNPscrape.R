udf_SNPscrape <- function(fileName="SNP_Tables.xlsx",
                          excelSheetN=1) {
  # About -------------------------------------------------------------------
  # Date: 13/05/2014
  # Scrape SNP names from:
  # - Excel file: SNP_Tables.xlsx [Status: Done]
  # - Text file:  SNP_Tables.txt (copy paste from ".doc" file) [Status: Done]
  
  # Future improvements:
  # ?? - Word file:  SNP_Tables.doc [Status: to be added] - requires AntiWord
  # ?? - CSV file: ...
  # ?? - flexible striingsplit, or better regex...
  
  # Workspace ---------------------------------------------------------------
  require("xlsx")
  
  # Data prep ---------------------------------------------------------------
  #get file type
  pos <- regexpr("\\.([[:alnum:]]+)$", fileName)
  fileType <- ifelse(pos > -1L, substring(fileName, pos + 1L), "")
  
  #check file extension
  if(!(fileType %in% c("xlsx","txt")))stop("Supported input files: .xlsx, .txt")
  
  #read file
  dat <- 
    switch(fileType,
           #Excel file
           xlsx = {
             #convert to 1 col matrix
             matrix(as.matrix(
               read.xlsx(file=fileName,
                         sheetIndex=excelSheetN,
                         stringsAsFactors=FALSE)),
               ncol=1)},
           #Text file
           txt = {readLines(con=fileName,
                            n=-1,
                            warn=FALSE)
           })
  
  # Scrape SNPs -------------------------------------------------------------
  # split by "/|,|\\s|\\*", this might need updating depending on ugliness of
  # input SNP tables.
  dat <- unique(unlist(strsplit(dat,"/|,|\\s|\\*|\\(|\\)")))
  #scrape using regex: rs123, chr1:123:I, chr1:123:D
  dat <- dat[grepl("rs[0-9]{1,}$|chr[0-9]{1,2}:[0-9]*:[I|D]$",dat)]
  output <- sort(unique(dat))
  # Return output -----------------------------------------------------------
  return(output)
}
