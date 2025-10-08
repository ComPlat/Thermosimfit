library(rootSolve)
library(ggplot2)
library(patchwork)
library(nnls)

files <- list.files("./tsf/R", full.names = TRUE)
trash <- lapply(files, source)

data <- readLines("./Tests/GlobalAnalysis/Full_Spectrum_all_Replicas.csv")
data <- data[1:18] # First repetition
data <- lapply(data, function(line) {
  strsplit(line, ",")[[1]]
})
names <- data[[1]]
data <- do.call(rbind, data[2:length(data)]) |> as.data.frame()
names(data) <- names
data <- data[, c(-7, -8)] # Removed these as data times instead of numbers were found
df <- apply(data, 2, as.numeric) |> as.data.frame()
df[[1]] <- df[[1]] * 10^6 # M to µM

# Test 10 wavelengths == semi global analysis
cols <- sapply(c(450, 487, 512, 524, 527, 577, 602, 651, 677, 700), \(x) {
  which(x == names(df))
})
cols <- sapply(c(450, 487, 524), \(x) {
  which(x == names(df))
})
df <- df[, c(1, cols)]
n_sigs <- ncol(df) - 1L
lowerBounds <- c(Ka = 0, rep(c(I0 = 0, IHD = 0, ID = 0), n_sigs))
upperBounds <- c(Ka = 10^5, rep(c(I0 = 10^5, IHD = 10^5, ID = 10^5), n_sigs))
additionalParameters <- c(
  host = 4.0,
  dye = 6,
  kHD = 1.7e01
)
res <- opti(
  case = "ida",
  lowerBounds, upperBounds,
  path = df,
  additionalParameters,
  npop = 40,
  ngen = 5000,
  Topology = "random",
  seed = 1234,
  errorThreshold = 0.3
)

save(res, file = "./Tests/GlobalAnalysis/res_GA.RData")
load("./Tests/GlobalAnalysis/res_GA.RData")
res$parameter
res$metrices
res$signal_plots[[1]]
res$signal_plots[[2]]
res$signal_plots[[3]]
res$signal_plots[[4]]
res$signal_plots[[5]]
res$signal_plots[[6]]
res$signal_plots[[7]]
res$signal_plots[[8]]
res$signal_plots[[9]]
res$signal_plots[[10]]

res_sensi <- sensitivity(
  "ida", res$parameter, df, additionalParameters, percentage = 15
)

res_sensi
class(res_sensi)
names(res_sensi)
res_sensi[[9]]
summary(res_sensi)
res_sensi$V

path <- "./Tests/GlobalAnalysis/batch_test.csv"
lowerBounds <- c(Ka = 0, rep(c(I0 = 0, IHD = 0, ID = 0), 10L))
upperBounds <- c(Ka = 10^5, rep(c(I0 = 10^5, IHD = 10^5, ID = 10^5), 10L))
res_batch <- batch("ida",lowerBounds, upperBounds,
      path, additionalParameters, num_rep = 3L, num_cores = 5L)
names(res_batch)
res_batch[[2]]
res_batch$params
res_batch$metrices



ps <- plotDAndHDBatch(res_batch, 3)
length(ps)
names(ps)
ps[[1]]
ps[[2]]
length(ps[[2]])
ps[[2]][[2]]
ps[[1]][[10]]
