library(rootSolve)
library(ggplot2)
library(patchwork)
files <- list.files("./tsf/R", full.names = TRUE)
trash <- lapply(files, source)

df <- read.csv("./Tests/IDA/forKonrad-conc-vs-signal.csv",
  sep = ";",
  dec = ".",
  header = TRUE
)
opti(
  case = "ida",
  lowerBounds = c(
    kG = 1000,
    I0 = 0,
    IHD = 0,
    ID = 0
  ),
  upperBounds = c(
    kG = 10^8,
    I0 = 100, # started at 10^7 but it ended always at 0...
    IHD = 10^7,
    ID = 10^7
  ),
  df,
  additionalParameters = c(
    host = 1.00E-06,
    dye = 1.00E-06,
    kHD = 3.00E+06
  ),
  npop = 40,
  ngen = 1000,
  Topology = "random",
  errorThreshold = 0.7
)

.traceback()
