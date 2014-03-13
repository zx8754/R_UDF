udf_BEDTools_bmerge <- 
  function(bed1,
           bmerge,
           opt.string="-s"){
    #opt.string
    #-s   Force strandedness. That is, only merge features that
    #     are the same strand. By default, this is disabled.
    #-n   Report the number of BED entries that were merged.
    #     1 is reported if no merging occurred.
    #-d   Maximum distance between features allowed for features
    #     to be merged. Default is 0. That is, overlapping and/or book-ended
    #     features are merged.
    #-nms Report the names of the merged features separated by semicolons.
    
    #create temp files
    bedfile=tempfile(tmpdir=getwd())
    out=tempfile(tmpdir=getwd())
    options(scipen=99) # not to use scientific notation when writing out
    
    #write bed formatted dataframes to tempfile
    write.table(bed1,bedfile,
                quote=FALSE,sep="\t",
                col.names=FALSE,row.names=FALSE)
    
    # create the command string and call the command using system()
    command=paste(bmerge,"-i",opt.string, bedfile,">",out,sep=" ")
    cat(command,"\n")
    try(system(command))
    
    # read output file and return
    res=read.table(out,header=F)
    unlink(a.file);unlink(b.file);unlink(out)
    return(res)
  }
