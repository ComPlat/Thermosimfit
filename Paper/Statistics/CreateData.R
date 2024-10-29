library(parallel)

get_data <- function(path) {
  load(path)
  parameter <- lapply(seq_len(length(result)), function(idx) {
    res <- result[[idx]][[2]]
    return(res)
  })
  parameter <- Reduce(rbind, parameter)
  parameter <- apply(parameter, 2, mean)
  parameter <- as.numeric(t(parameter))
  df <- result[[1]][[1]]
  ap <- result[[1]]$additionalParameters
  return(
    list(
      parameter = parameter,
      data = df,
      additionalParameters = ap,
      lb = result[[1]]$lowerBounds |> as.numeric(),
      ub = result[[1]]$upperBounds |> as.numeric()
    )
  )
}

create_df <- function(case, params, env) {
  lossFct <- tryCatch(
    expr = {
      if (case == "dba_host_const") {
        tsf:::lossFctHG
      } else if (case == "dba_dye_const") {
        tsf:::lossFctDBA
      } else if (case == "ida") {
        tsf:::lossFctIDA
      } else if (case == "gda") {
        tsf:::lossFctGDA
      }
    },
    error = function(e) {
      return(NULL)
    },
    interrupt = function(e) {
      return(NULL)
    }
  )

  names <- c("Ka(HD) [1/M]", "I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  if (case == "ida" || case == "gda") {
    names <- c("Ka(HG) [1/M]", "I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  }

  lb <- params * 0.1 # 0.5
  ub <- params * 3 # 2
  parameter <- lapply(1:4, function(x) {
    seq(lb[x], ub[x], length.out = 20) # 15
  })
  grid <- expand.grid(parameter)
  names(grid) <- names
  grid$errors <- unlist(mclapply(1:nrow(grid), function(i) {
    row <- grid[i, ]
    lossFct(as.numeric(row), env, FALSE)
  }, mc.cores = detectCores() - 1))
  return(grid)
}

df_dba <- function(path) {
  data <- get_data(path)
  df <- data$data
  parameter <- data$parameter
  ap <- data$additionalParameters
  env <- new.env()
  env$host <- df[, 1]
  env$signal <- df[, 2]
  env$d0 <- ap[1]
  create_df(
    "dba_dye_const",
    parameter, env
  )
}

df_ida <- function(path) {
  data <- get_data(path)
  df <- data$data
  parameter <- data$parameter
  ap <- data$additionalParameters
  env <- new.env()
  env$ga <- df[, 1]
  env$signal <- df[, 2]
  env$h0 <- ap[1]
  env$d0 <- ap[2]
  env$kd <- ap[3]
  create_df(
    "ida",
    parameter, env
  )
}

df_gda <- function(path) {
  data <- get_data(path)
  df <- data$data
  parameter <- data$parameter
  ap <- data$additionalParameters
  env <- new.env()
  env$dye <- df[, 1]
  env$signal <- df[, 2]
  env$h0 <- ap[1]
  env$ga0 <- ap[2]
  env$kd <- ap[3]
  create_df(
    "gda",
    parameter, env
  )
}

dba <- df_dba("../DecentFitParameterVariance/DBA_10_different_seeds.RData")
names(dba) <- c("KaHD", "I0", "IHD", "ID", "errors")
print("dba done")
ida <- df_ida("../DecentFitParameterVariance/IDA_10_different_seeds.RData")
names(ida) <- c("KaHG", "I0", "IHD", "ID", "errors")
print("ida done")
gda <- df_gda("../DecentFitParameterVariance/GDA_10_different_seeds.RData")
names(gda) <- c("KaHG", "I0", "IHD", "ID", "errors")
print("gda done")

save(dba, ida, gda, file = "DenseGrid.RData")
