# Conduct one optimization
# ===============================
library(tsf)
lowerBounds <- c(
  kG = 1000,
  I0 = 0,
  IHD = 0,
  ID = 0
)
upperBounds <- c(
  kG = 10^8,
  I0 = 100,
  IHD = 10^7,
  ID = 10^7
)
additionalParameters <- c(
  host = 1e-6,
  dye = 1e-6,
  kHD = 3e6
)

result <- opti(
  case = "ida",
  lowerBounds = lowerBounds,
  upperBounds = upperBounds,
  path = "IDA.csv",
  additionalParameters = additionalParameters
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
df <- read.csv("IDA.csv",
  sep = ";",
  dec = ".",
  header = TRUE
)

env <- new.env()
env$h0 <- 1e-6
env$kd <- 3e6
env$d0 <- 1e-6
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
library(plotly)
plot_ly(
  data = df,
  x = ~kG,
  y = ~ID,
  z = ~errors,
  size = 1,
  type = "scatter3d"
)

# Visualize the parameter variance
# ===============================
boxplot(df$kG)
boxplot(df$I0)
boxplot(df$ID)
boxplot(df$IHD)
