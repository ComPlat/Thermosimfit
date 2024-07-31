library(tsf)
df <- read.csv("dba_dye_const.txt",
  sep = "\t",
  dec = ".",
  header = FALSE
)

res <- opti(
  case = "dba_dye_const",
  lowerBounds = c(
    kHD = 0,
    I0 = 0,
    IHD = 0,
    ID = 0
  ),
  upperBounds = c(
    kHD = 10^8,
    I0 = 10^8,
    IHD = 10^8,
    ID = 10^8
  ),
  df,
  additionalParameters = c(
    dye = 0.000151
  ),
  npop = 40,
  ngen = 1000,
  Topology = "random",
  errorThreshold = 0.7
)

res
