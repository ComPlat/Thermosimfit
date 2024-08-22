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
  host =  1e-6,
  dye =  1e-6,
  kHD =  3e6
)

additionalParameters <- c( # NOTE: wrong input by purpose
  host =  0,
  dye =  0,
  kHD =  0
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
while (process$is_alive()) {
  process$get_status() |> print()
  process$read_output() |> cat()
  process$read_error() |> cat()
  Sys.sleep(0.01)
}
print("End loop")
process$is_alive() |> print()
e <- process$read_all_error() |> print()
# process$get_result() |> print()
stop()






process <- tsf:::call_sensi_in_bg(
  "ida", res[[2]],
  df, additionalParameters, 15
)

while (process$is_alive()) {
  process$get_status() |> print()
  process$read_output() |> cat()
  Sys.sleep(1)
}
print("test")
process$is_alive() |> print()
res <- process$get_result()
res
