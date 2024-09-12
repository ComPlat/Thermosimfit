library(tsf)
df <- read.csv("IDA.csv",
  sep = ";",
  dec = ".",
  header = TRUE
)

lowerBounds <- c(
  kG = 10^3,
  I0 = 0,
  IHD = 0,
  ID = 0
)

upperBounds <- c(
  kG = 10^8,
  I0 = 1000,
  IHD = 10^7,
  ID = 10^6
)

# grid across the parameter space
grid <- expand.grid(
  kG = seq(lowerBounds[["kG"]],
    upperBounds[["kG"]],
    length.out = 10
  ),
  I0 = seq(lowerBounds[["I0"]],
    upperBounds[["I0"]],
    length.out = 10
  ),
  IHD = seq(lowerBounds[["IHD"]],
    upperBounds[["IHD"]],
    length.out = 10
  ),
  ID = seq(lowerBounds[["ID"]],
    upperBounds[["ID"]],
    length.out = 10
  )
)

env <- new.env()
env$h0 <- 1e-6
env$kd <- 3e6
env$d0 <- 1e-6
env$ga <- df[, 1]
env$signal <- df[, 2]
grid$errors <- apply(grid, 1, function(parameter) {
  idx <- parent.frame()$i[]
  if ((idx %% 100) == 0) {
    print((100 / nrow(grid)) * idx)
  }
  tsf:::lossFctIDA(parameter, env, FALSE)
})

saveRDS(grid, file = "ParameterError.rds")
