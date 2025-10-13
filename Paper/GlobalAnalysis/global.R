library(tsf)
library(parallel)

num_cores <- detectCores() - 3
seeds <- c(376033, 945211, 478950, 429370, 78735, 268225, 324905, 359876, 868834, 584371)

pp <- function(res, wls) {
  ps <- vapply(res$parameter, function(x) {
    formatC(x[[1]], format = "e", digits = 2)
  }, character(1))
  n <- names(ps)[1]
  cat(paste0(n, " ", ps[[1]]), "\n")
  temp <- as.data.frame(matrix(ps[-1], nrow = 3))
  names(temp) <- wls
  row.names(temp) <- c("I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  print(temp)
  invisible(NULL)
}

df <- read.csv("./Paper/GlobalAnalysis/Full.csv", header = TRUE)
wls <- paste("X", c(470, 486, 512, 524, 532, 550), sep = "")
df <- df[, c("var", wls)]

ida <- function(df, seeds) {
  additionalParameters <- c(
    host = 4.0*10^-6,
    dye = 6.0*10^-6,
    kHD = 1.7*10^7
  )
  lowerBounds <- c(Ka = 1000,
    c(I0 = 0, IHD = 10^6, ID = 10^5), # 470
    c(I0 = 0, IHD = 10^6, ID = 10^5), # 486
    c(I0 = 0, IHD = 10^6, ID = 10^5), # 512
    c(I0 = 0, IHD = 10^6, ID = 10^5), # 524
    c(I0 = 0, IHD = 10^6, ID = 10^5), # 532
    c(I0 = 0, IHD = 10^6, ID = 10^5)  # 550
  )
  upperBounds <- c(Ka = 10^10,
    c(I0 = 10^2, IHD = 10^10, ID = 10^9), # 470
    c(I0 = 10^2, IHD = 10^10, ID = 10^9), # 486
    c(I0 = 10^2, IHD = 10^10, ID = 10^9), # 512
    c(I0 = 10^2, IHD = 10^10, ID = 10^9), # 524
    c(I0 = 10^2, IHD = 10^10, ID = 10^9), # 532
    c(I0 = 10^2, IHD = 10^10, ID = 10^9)  # 550
  )
  mclapply(seeds, function(seed) {
    opti(
      case = "ida",
      lowerBounds = lowerBounds,
      upperBounds = upperBounds,
      path = df,
      seed = seed,
      ngen = 5000,
      npop = 40,
      errorThreshold = -Inf,
      additionalParameters = additionalParameters,
      add_info = as.character(seed)
    )
  }, mc.cores = num_cores, mc.silent = TRUE, mc.preschedule = FALSE)
}

res <- ida(df, seeds)
save(res, file = "./Paper/GlobalAnalysis/10Runs.RData")
