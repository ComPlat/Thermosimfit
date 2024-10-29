library(tsf)
library(plot3D)

# DBA
# Load best parameters
# ===============================
load("../DecentFitParameterVariance/DBA_10_different_seeds.RData")
parameter <- lapply(result, function(x) x[[2]])
parameter <- Reduce(rbind, parameter)
parameter <- apply(parameter, 2, mean)

df <- result[[1]]$data
df <- df[, c(1, 2)]

# create new parameters based on optimization result
# ===============================
best_params <- as.numeric(parameter)

lb_KaHD <- best_params[1] * 0.5
ub_KaHD <- best_params[1] * 2
KaHD <- seq(lb_KaHD, ub_KaHD, length.out = 100)

lb_IHD <- best_params[3] * 0.1
ub_IHD <- best_params[3] * 5
IHD <- seq(lb_IHD, ub_IHD, length.out = 100)

grid <- mesh(KaHD, IHD)

# Evaluate the new parameters
# ===============================
env <- new.env()
env$d0 <- 50 * 10^-6
env$host <- df[, 1]
env$signal <- df[, 2]

new_params <- data.frame(
  KaHD = as.vector(grid$x),
  IHD = as.vector(grid$y)
)

new_params$errors <- apply(new_params, 1, function(params) {
  idx <- parent.frame()$i[]
  if ((idx %% 100) == 0) {
    print((100 / nrow(new_params)) * idx)
  }
  parameter <- c(params[1], best_params[2], params[2], best_params[4])
  tsf:::lossFctDBA(parameter, env, FALSE)
})

save(KaHD, IHD, grid, new_params, file = "DBA_3D.RData")
stop()


# GDA
# Load best parameters
# ===============================
load("../DecentFitParameterVariance/GDA_10_different_seeds.RData")
parameter <- lapply(result, function(x) x[[2]])
parameter <- Reduce(rbind, parameter)
parameter <- apply(parameter, 2, mean)

df <- result[[1]]$data
df <- df[, c(1, 2)]

# create new parameters based on optimization result
# ===============================
best_params <- as.numeric(parameter)

lb_KaHG <- best_params[1] * 0.8
ub_KaHG <- best_params[1] * 1.2
KaHG <- seq(lb_KaHG, ub_KaHG, length.out = 100)

lb_IHD <- best_params[3] * 0.8
ub_IHD <- best_params[3] * 1.2
IHD <- seq(lb_IHD, ub_IHD, length.out = 100)

grid <- mesh(KaHG, IHD)

# Evaluate the new parameters
# ===============================
env <- new.env()
env$h0 <- 50 * 10^-6
env$ga0 <- 292 * 10^-6
env$kd <- 33000
env$dye <- df[, 1]
env$signal <- df[, 2]

new_params <- data.frame(
  KaHG = as.vector(grid$x),
  IHD = as.vector(grid$y)
)

new_params$errors <- apply(new_params, 1, function(params) {
  idx <- parent.frame()$i[]
  if ((idx %% 100) == 0) {
    print((100 / nrow(new_params)) * idx)
  }
  parameter <- c(params[1], best_params[2], params[2], best_params[4])
  tsf:::lossFctGDA(parameter, env, FALSE)
})

save(KaHG, IHD, grid, new_params, file = "GDA_3D.RData")



# IDA
# Load best parameters
# ===============================
load("../DecentFitParameterVariance/IDA_10_different_seeds.RData")
parameter <- lapply(result, function(x) x[[2]])
parameter <- Reduce(rbind, parameter)
parameter <- apply(parameter, 2, mean)

df <- result[[1]]$data
df <- df[, c(1, 2)]

# create new parameters based on optimization result
# ===============================
best_params <- as.numeric(parameter)

lb_KaHG <- best_params[1] * 0.8
ub_KaHG <- best_params[1] * 1.2
KaHG <- seq(lb_KaHG, ub_KaHG, length.out = 100)

lb_IHD <- best_params[3] * 0.8
ub_IHD <- best_params[3] * 1.2
IHD <- seq(lb_IHD, ub_IHD, length.out = 100)

grid <- mesh(KaHG, IHD)

# Evaluate the new parameters
# ===============================
env <- new.env()
env$h0 <- 4.3 * 10^-6
env$d0 <- 6 * 10^-6
env$kd <- 1.7E07
env$ga <- df[, 1]
env$signal <- df[, 2]

new_params <- data.frame(
  KaHG = as.vector(grid$x),
  IHD = as.vector(grid$y)
)

new_params$errors <- apply(new_params, 1, function(params) {
  idx <- parent.frame()$i[]
  if ((idx %% 100) == 0) {
    print((100 / nrow(new_params)) * idx)
  }
  parameter <- c(params[1], best_params[2], params[2], best_params[4])
  tsf:::lossFctIDA(parameter, env, FALSE)
})

save(KaHG, IHD, grid, new_params, file = "IDA_3D.RData")
