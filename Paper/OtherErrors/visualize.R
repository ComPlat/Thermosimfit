library(ggplot2)
library(cowplot)

# SSE / RMSE
# Square residuals → large errors dominate.
# Optimizer focuses on fitting big peaks / high-magnitude regions.
# Parameters shift to minimize a few large deviations, sometimes at the cost of small ones.
# RelErr
# Linear residuals (or relative) → all errors weighted more equally (or scaled by signal size).
# Optimizer fits low-magnitude regions better, less dominated by peaks.
# Parameters can shift upward/downward to capture small values more precisely.
# Huber
# Quadratic for small residuals, linear for large ones.
# Behaves like RMSE where fit is already good, but ignores big outliers.
# Leads to robust, tighter parameter estimates, less sensitive to noise spikes.

p <- function(case) {
  combine <- function(result, error_fct) {
    seeds <- lapply(result, function(x) x$seed)

    df <- lapply(seq_len(length(result)), function(idx) {
      res <- result[[idx]][[1]]
      res$seed <- seeds[[idx]]
      res$error_fct <- error_fct
      return(res)
    })
    df <- Reduce(rbind, df)

    parameter <- lapply(seq_len(length(result)), function(idx) {
      res <- result[[idx]][[2]]
      res$seed <- seeds[[idx]]
      res$error_fct <- error_fct
      return(res)
    })
    parameter <- Reduce(rbind, parameter)

    metrices <- lapply(seq_len(length(result)), function(idx) {
      res <- result[[idx]][[4]]
      res$seed <- seeds[[idx]]
      res$error_fct <- error_fct
      return(res)
    })
    metrices <- Reduce(rbind, metrices)

    list(data = df, parameter = parameter, metrices = metrices)
  }

  data <- get(case)
  res <- lapply(names(data), function(name) {
    combine(data[[name]], name)
  })

  plot_data <- function(res) {
    df <- lapply(res, function(sim) {
      sim$data
    })
    df <- Reduce(rbind, df)
    name_col_1 <- names(df)[1]
    ggplot(data = df) +
      geom_boxplot(
        data = df, aes(x = .data[[name_col_1]],
          y = .data[["Signal simulated"]],
          group = .data[[name_col_1]]
        )
      ) +
      geom_point(
        data = df, aes(x = .data[[name_col_1]],
          y = .data[["Signal measured"]]
        ), colour = "darkred"
      ) +
      facet_wrap(~ error_fct)
  }
  p_data <- plot_data(res)

  plot_parameter <- function(res) {
    df <- lapply(res, function(sim) {
      sim$parameter
    })
    df <- Reduce(rbind, df)
    df <- data.frame(
      stack(df[, 1:4]),
      seed = rep(df$seed, 4),
      error_fct = rep(df$error_fct, 4)
    )
    ggplot(data = df, aes(x = error_fct, y = values, group = error_fct)) +
      geom_boxplot() +
      facet_wrap(~ ind, scales = "free")
  }
  p_parameter <- plot_parameter(res)

  plot_grid(p_data, p_parameter)
}

# IDA
# ============================================================
load("./Paper/OtherErrors/IDA_10Simulations_5DifferentErrorFcts.RData")
pl <- p("ida")
pl
ggsave("./Paper/OtherErrors/ErrorFctIDA.png", pl)

load("./Paper/OtherErrors/GDA_10Simulations_5DifferentErrorFcts.RData")
pl <- p("gda")
pl
ggsave("./Paper/OtherErrors/ErrorFctGDA.png", pl)
