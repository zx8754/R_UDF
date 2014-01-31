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
