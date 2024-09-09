setwd("/home/konrad/Documents/Thermosimfit/Tests/IDA")
library(tsf)

# Test several opti calls in parallel
path <- "idaBatch.csv"
lowerBounds <- c(
  kG = 1000,
  I0 = 0,
  IHD = 0,
  ID = 0
)
upperBounds <- c(
  kG = 10^8,
  I0 = 100,
  IHD = 10^7,
  ID = 10^7
)
additionalParameters <- c(
  host = 1e-6,
  dye = 1e-6,
  kHD = 3e6
)

tsf::batch(
  "ida",
  lowerBounds, upperBounds,
  path, additionalParameters,
  ngen = 20
)
