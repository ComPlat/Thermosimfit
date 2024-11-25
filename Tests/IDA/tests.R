# Quantity	Value
# conc(H)	1.00E-06
# conc(D)	1.00E-06
# conc(G)	1/200000
# Kequ(HD)	3.00E+06
# Kequ(HG)	2.00E+07
# Signal-0	0
# Signal-HD	1.00E+06
# Signal-Dye	2.00E+05
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA")
library(tsf)

# Test several opti calls in parallel
path <- "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/idaBatch.csv"
list_df <- tsf:::importDataBatch(path)

seeds <- 1:length(list_df)
messages <- paste0(1:length(list_df))
lowerBounds <- c(
  kG = 1000,
  I0 = 0,
  IHD = 0,
  ID = 0
)
upperBounds <- c(
  kG = 10^8,
  I0 = 100, # started at 10^7 but it ended always at 0...
  IHD = 10^7,
  ID = 10^7
)
additionalParameters <- c(
  host = 1e-6,
  dye = 1e-6,
  kHD = 3e6
)

res <- opti(
  case = "ida",
  lowerBounds = lowerBounds,
  upperBounds = upperBounds,
  path = list_df[[1]],
  seed = 1234,
  additionalParameters = additionalParameters,
  npop = 40,
  ngen = 100,
  Topology = "random",
  errorThreshold = 0.3
)

res[[3]]
