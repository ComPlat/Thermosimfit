# Host: β-cyclodextrin
library(tsf)

path <- "./CrossValidation/supramolecular/"
df1 <- read.csv( paste0(path, "FirstDataset.csv"), sep = ",") # µM
df2 <- read.csv( paste0(path, "SecondDataSet.csv"), sep = ",") # µM
df3 <- read.csv( paste0(path, "ThirdDataSet.csv"), sep = ",") # µM

run_sims <- function(df) {
  dye <- 151 # TNS [µM]
  lower <- c(khd = 0.00000001, I0 = 0, IHD = 10, ID = 3)
  upper <- c(khd = 10^1, I0 = 10^2, IHD = 10^3, ID = 10^2)
  seeds <- sample(1:10^6, 10)
  lapply(seeds, function(seed) {
    opti(
      "dba_dye_const",
      lower, upper,
      df,
      dye,
      seed = seed,
      npop = 40, ngen = 1000,
      Topology = "random",
      errorThreshold = 0.3
    )
  })
}

res1 <- run_sims(df1[, c(2, 3)]) # Supramolecular requires the host as own column
res2 <- run_sims(df2[, c(2, 3)])
res3 <- run_sims(df3[, c(2, 3)])

res <- c(res1, res2, res3)
save(
  res,
  file = paste0(path, "DBA_30Sims.RData")
)
