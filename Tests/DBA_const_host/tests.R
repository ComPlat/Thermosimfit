library(tsf)
df <- read.csv("dba_dye_const.txt",
  sep = "\t",
  dec = ".",
  header = FALSE
)

res <- opti(
  case = "dba_host_const",
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
  seed = 1234,
  additionalParameters = c(
    host = 0.000151
  ),
  npop = 40,
  ngen = 1000,
  Topology = "random",
  errorThreshold = 1.6
)

res

tsf::sensitivity(
  case = "dba_host_const", parameter = res[[2]],
  path = df,
  additionalParameters = c(host = 0.000151),
  percentage = 15
)
