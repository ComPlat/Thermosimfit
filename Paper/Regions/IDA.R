library(tsf)
library(plotly)
# Conduct one optimization
# ===============================
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

data <- readLines("../IDA.txt")
var <- lapply(data, function(x) {
  strsplit(x, split = "\t")[[1]][1]
}) |> unlist()
signal <- lapply(data, function(x) {
  strsplit(x, split = "\t")[[1]][2]
}) |> unlist()
df <- data.frame(var = var[2:22], signal = signal[2:22])
df$var <- as.numeric(df$var)
df$signal <- as.numeric(df$signal)
seed <- 46967
result <- opti(
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

# create new parameters based on optimization result
# ===============================
best_params <- result[[2]]
lb <- best_params * 0.8
ub <- best_params * 1.2

new_params <- lapply(1:4, function(x) {
  r <- runif(5000)
  lb[[x]] + (ub[[x]] - lb[[x]]) * r
})
new_params <- Reduce(cbind, new_params)
new_params <- as.data.frame(new_params)
names(new_params) <- c("kG", "I0", "ID", "IHD")

# Evaluate the new parameters
# ===============================
env <- new.env()
env$h0 <- 4.3e-6
env$kd <- 1.7e07
env$d0 <- 6e-6
env$ga <- df[, 1]
env$signal <- df[, 2]
new_params$errors <- apply(new_params, 1, function(parameter) {
  idx <- parent.frame()$i[]
  if ((idx %% 100) == 0) {
    print((100 / nrow(new_params)) * idx)
  }
  tsf:::lossFctIDA(parameter, env, FALSE)
})

# Visualize the results
# ===============================
df <- new_params[new_params$errors < 0.75, ]

dim(df)
head(df)
