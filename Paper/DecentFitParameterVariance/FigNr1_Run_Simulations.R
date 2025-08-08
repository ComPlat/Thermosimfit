library(tsf)
library(parallel)
num_cores <- detectCores() - 1

# Figure Nr. 1
# one measured curve & 10 curves (created by vastly different parameter sets)
# ============================================================

# Creating the 10 different simulations
# ============================================================

# GDA new data set
# ============================================================
gda_new <- function() {
  lowerBounds <- c(
    kG = 100,
    I0 = 0,
    IHD = 0,
    ID = 100
  )
  # NOTE: upper bounds are set due to results of preliminary runs
  upperBounds <- c(
    kG = 10^6,
    I0 = 10^3,
    IHD = 10^10,
    ID = 10^6
  )
  additionalParameters <- c(
    host = 50 * 10^-6,
    guest = 292 * 10^-6,
    kHD = 33000
  )

  # NOTE: data var column [M]
  data <- readLines("../GDA.txt")
  var <- lapply(data, function(x) {
    strsplit(x, split = "\t")[[1]][1]
  }) |> unlist()
  signal <- lapply(data, function(x) {
    strsplit(x, split = "\t")[[1]][2]
  }) |> unlist()
  # NOTE: Only the first dataset is used
  df <- data.frame(var = var[2:25], signal = signal[2:25])
  df$var <- as.numeric(df$var)
  df$signal <- as.numeric(df$signal)
  set.seed(1234)
  seeds <- sample(1:1e6, 20)
  result <- mclapply(seeds, function(seed) {
    opti(
      case = "gda",
      lowerBounds = lowerBounds,
      upperBounds = upperBounds,
      path = df,
      seed = seed,
      ngen = 1000,
      errorThreshold = 0.6,
      additionalParameters = additionalParameters,
      add_info = as.character(seed)
    )
  }, mc.cores = num_cores)
  # Filter the 10 best ones
  m <- lapply(result, function(x) x$metrices)
  m <- Reduce(rbind, m)
  result <- result[order(m[, 1])[1:10]]
  save(result, file = "GDA_10_different_seeds.RData")
}
gda()

# DBA
# ============================================================
dba <- function() {
  lowerBounds <- c(
    kHD = 0,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kHD = 10^7,
    I0 = 10^5,
    IHD = 10^8,
    ID = 10^7
  )
  additionalParameters <- c(
    dye = 151 * 10^-6 # TODO: isnt this rather dye?
  )

  # NOTE: data var column [M]
  data <- readLines("../DBA.txt")
  var <- lapply(data, function(x) {
    strsplit(x, split = "\t")[[1]][1]
  }) |> unlist()
  signal <- lapply(data, function(x) {
    strsplit(x, split = "\t")[[1]][2]
  }) |> unlist()
  # NOTE: Only the first dataset is used
  df <- data.frame(var = var[2:28], signal = signal[2:28])
  df$var <- as.numeric(df$var)
  df$signal <- as.numeric(df$signal)
  # seeds <- c(46967, 123, 98765, 91234, 920988)
  seeds <- sample(1:1e6, 10)
  result <- lapply(seeds, function(seed) {
    opti(
      case = "dba_dye_const",
      lowerBounds = lowerBounds,
      upperBounds = upperBounds,
      path = df,
      seed = seed,
      ngen = 5000,
      npop = 100,
      errorThreshold = 0.4,
      additionalParameters = additionalParameters,
      add_info = as.character(seed)
    )
  })
  save(result, file = "DBA_10_different_seeds.RData")
}
# IDA
# ============================================================
ida <- function() {
  lowerBounds <- c(
    kG = 0,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kG = 10^10,
    I0 = 10^2,
    IHD = 10^10,
    ID = 10^10
  )
  additionalParameters <- c(
    host = 4.3 * 10^-6,
    dye = 6 * 10^-6,
    kHD = 1.7E07
  )

  # NOTE: data var column [M]
  data <- readLines("../IDA.txt")
  var <- lapply(data, function(x) {
    strsplit(x, split = "\t")[[1]][1]
  }) |> unlist()
  signal <- lapply(data, function(x) {
    strsplit(x, split = "\t")[[1]][2]
  }) |> unlist()
  # NOTE: Only the first dataset is used
  df <- data.frame(var = var[2:22], signal = signal[2:22])
  df$var <- as.numeric(df$var)
  df$signal <- as.numeric(df$signal)
  # seeds <- c(46967, 1234, 9999, 941285, 920988)
  seeds <- sample(1:1e6, 10)
  result <- lapply(seeds, function(seed) {
    opti(
      case = "ida",
      lowerBounds = lowerBounds,
      upperBounds = upperBounds,
      path = df,
      seed = seed,
      ngen = 5000,
      npop = 100,
      errorThreshold = 0.4,
      additionalParameters = additionalParameters,
      add_info = as.character(seed)
    )
  })
  save(result, file = "IDA_10_different_seeds.RData")
}
ida()
