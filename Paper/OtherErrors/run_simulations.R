library(tsf)
library(parallel)

num_cores <- detectCores() - 1
seeds <- c(17287, 31370, 2301, 14163, 40882, 30777,  3513,  8248, 25557, 28574)

# Error functions
# ============================================================
rel_err   <- function(yhat, y) {
  eps <- 1e-12
  sum(abs(y - yhat) / pmax(abs(y), eps))
}
rmse <- function(yhat, y) {
  r <- y - yhat
  sqrt(mean(r^2))
}
sse <- function(yhat, y) {
  r <- y - yhat
  sum(r^2)
}
huber <- function(yhat, y) {
  delta <- 0.5
  r <- abs(y - yhat)
  mean(ifelse(r <= delta, 0.5*r^2, delta*(r - 0.5*delta)))
}

# IDA
# ============================================================
ida <- function(ecf, seeds) {
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
  data <- readLines("./Paper/IDA.txt")
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
  mclapply(seeds, function(seed) {
    opti(
      case = "ida",
      lowerBounds = lowerBounds,
      upperBounds = upperBounds,
      path = df,
      seed = seed,
      ngen = 1000,
      npop = 40,
      errorThreshold = -Inf,
      additionalParameters = additionalParameters,
      add_info = as.character(seed),
      error_calc_fct = ecf
    )
  }, mc.cores = num_cores, mc.silent = TRUE)
}

res_mane <- ida(NULL, seeds)
res_rel_err <- ida(rel_err, seeds)
res_rmse <- ida(rmse, seeds)
res_sse <- ida(sse, seeds)
res_huber <- ida(huber, seeds)

ida <- list(
  MANE = res_mane, RelErr = res_rel_err,
  RMSE = res_rmse, SSE = res_sse, HUBER = res_huber
)
save(ida, file = "./Paper/OtherErrors/IDA_10Simulations_5DifferentErrorFcts.RData")

# GDA
# ============================================================
gda <- function(ecf, seeds) {
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
  data <- readLines("./Paper/GDA.txt")
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
  mclapply(seeds, function(seed) {
    opti(
      case = "gda",
      lowerBounds = lowerBounds,
      upperBounds = upperBounds,
      path = df,
      seed = seed,
      ngen = 1000,
      errorThreshold = 0.6,
      additionalParameters = additionalParameters,
      add_info = as.character(seed),
      error_calc_fct = ecf
    )
  }, mc.cores = num_cores, mc.silent = TRUE)
}

res_mane <- gda(NULL, seeds)
res_rel_err <- gda(rel_err, seeds)
res_rmse <- gda(rmse, seeds)
res_sse <- gda(sse, seeds)
res_huber <- gda(huber, seeds)

gda <- list(
  MANE = res_mane, RelErr = res_rel_err,
  RMSE = res_rmse, SSE = res_sse, HUBER = res_huber
)
save(gda, file = "./Paper/OtherErrors/GDA_10Simulations_5DifferentErrorFcts.RData")
