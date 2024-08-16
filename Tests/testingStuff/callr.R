setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA")
library(tsf)
library(callr)
df <- read.csv("forKonrad-conc-vs-signal.csv",
  sep = ";",
  dec = ".",
  header = FALSE
)

case <- "ida"
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
npop <- 40
ngen <- 20
Topology <- "random"
errorThreshold <- 0.7
seed <- 1234

process <- callr::r_bg(
  function(case, lb, ub,
           df, ap, seed, npop, ngen, Topology, errorThreshold) {
    res <- tsf::opti(
      case, lb, ub,
      df, ap, seed, npop, ngen, Topology, errorThreshold
    )
    return(res)
  },
  args = list(
    case, lowerBounds, upperBounds,
    df, additionalParameters, seed, npop, ngen, Topology, errorThreshold
  )
)
process$is_alive()
process$get_status()
process$read_output()
process$print()
process$read_output_lines()
process$signal()
process$get_result()
