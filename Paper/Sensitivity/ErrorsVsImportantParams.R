library(ggplot2)
library(cowplot)

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

create_single_param_plots <- function(case, params, env, lb, ub) {
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

  plots <- lapply(1:4, function(idx) {
    disturbed_param <- seq(
      params[idx] * 0.5,
      params[idx] * 2,
      length.out = 100
    )
    errors <- sapply(disturbed_param, function(dp) {
      parameter <- params
      parameter[idx] <- dp
      lossFct(parameter, env, FALSE)
    })
    df <- data.frame(disturbed_param = disturbed_param, errors = errors)
    ggplot(data = df, aes(x = disturbed_param, y = errors)) +
      geom_point() +
      labs(x = names[idx], y = "MANE")
  })
  plot_grid(plotlist = plots)
}

plot_dba <- function(path) {
  data <- get_data(path)
  df <- data$data
  parameter <- data$parameter
  ap <- data$additionalParameters
  env <- new.env()
  env$host <- df[, 1]
  env$signal <- df[, 2]
  env$d0 <- ap[1]
  create_single_param_plots(
    "dba_dye_const",
    parameter, env,
    data$lb, data$ub
  )
}

plot_ida <- function(path) {
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
  create_single_param_plots(
    "ida",
    parameter, env,
    data$lb, data$ub
  )
}

plot_gda <- function(path) {
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
  create_single_param_plots(
    "gda",
    parameter, env,
    data$lb, data$ub
  )
}

p_dba_ip <- plot_dba("../DecentFitParameterVariance/DBA_10_different_seeds.RData")
p_ida_ip <- plot_ida("../DecentFitParameterVariance/IDA_10_different_seeds.RData")
p_gda_ip <- plot_gda("../DecentFitParameterVariance/GDA_10_different_seeds.RData")
save(p_dba_ip, p_ida_ip, p_gda_ip, file = "ErrorsVsImportantParams.RData")
