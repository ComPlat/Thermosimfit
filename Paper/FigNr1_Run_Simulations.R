library(tsf)

# Figure Nr. 1
# one measured curve & 5 simulated curves (created by vastly different parameter sets)
# ============================================================

# Creating the 5 different simulations
# ============================================================

# GDA
# ============================================================
gda <- function() {
  lowerBounds <- c(
    kG = 10,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kG = 10^9,
    I0 = 700,
    IHD = 10^9,
    ID = 10^9
  )
  additionalParameters <- c(
    host = 103 * 10^-6,
    guest = 1050 * 10^-6,
    kHD = 2431.14
  )

  # NOTE: data var column [M]
  data <- readLines("GDA.txt")
  var <- lapply(data, function(x) {
    strsplit(x, split = "\t")[[1]][1]
  }) |> unlist()
  signal <- lapply(data, function(x) {
    strsplit(x, split = "\t")[[1]][2]
  }) |> unlist()
  # NOTE: Only the first dataset is used
  df <- data.frame(var = var[2:19], signal = signal[2:19])
  df$var <- as.numeric(df$var)
  df$signal <- as.numeric(df$signal)
  seeds <- sample(1:1e6, 10)
  result <- lapply(seeds, function(seed) {
    opti(
      case = "gda",
      lowerBounds = lowerBounds,
      upperBounds = upperBounds,
      path = df,
      seed = seed,
      ngen = 10000,
      npop = 100,
      errorThreshold = 0.2,
      additionalParameters = additionalParameters,
      add_info = as.character(seed)
    )
  })
  save(seeds, result, file = "GDA_10_different_seeds.RData")
}
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
  data <- readLines("DBA.txt")
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
  data <- readLines("IDA.txt")
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
