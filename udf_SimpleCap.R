#################### RMH cleaning functions
udf_simpleCap <- function(x) {
  #input: "abs add too"
  #output: "Abs Add Too"
  s <- strsplit(tolower(x), " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")}
