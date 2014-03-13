udf_BEDTools_bmerge <- 
  function(bed1,
           bmerge="/bedtools2-2.19.1/bin/mergeBed",
           opt.string="-s"){
    #bed1 - data.frame sorted by chrom and start.
    #       If opt.string="-s", then data.frame must be in BED6 format:
    #             chrom, start, end, name, score, and strand.
    #       Else: chrom, start, end
    #
    #bmerge - BEDTools bmerge file with a path:
    #     e.g.: /software/bedtools2-2.19.1/bin/mergeBed
    #
    #opt.string - 
    #-s   Force strandedness. That is, only merge features that
    #     are the same strand. By default, this is disabled.
    #-n   Report the number of BED entries that were merged.
    #     1 is reported if no merging occurred.
    #-d   Maximum distance between features allowed for features
    #     to be merged. Default is 0. That is, overlapping and/or book-ended
    #     features are merged.
    #-nms Report the names of the merged features separated by semicolons.
    
    #create temp files
    bedfile=tempfile(pattern="temp_in_",tmpdir=getwd(),fileext=".txt")
    out=tempfile(pattern="temp_out_",tmpdir=getwd(),fileext=".txt")
    options(scipen=99) # not to use scientific notation when writing out
    
    #write bed formatted dataframes to tempfile
    write.table(bed1,bedfile,
                quote=FALSE,sep="\t",
                col.names=FALSE,row.names=FALSE)
    
    # create the command string and call the command using system()
    command=paste(bmerge,"-i",bedfile,opt.string,">",out,sep=" ")
    cat(command,"\n")
    try(system(command))
    
    # read output file and return
    res=read.table(out,header=F)
    unlink(bedfile);unlink(out)
    return(res)
  }
