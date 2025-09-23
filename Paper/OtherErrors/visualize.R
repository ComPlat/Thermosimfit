library(ggplot2)

p <- function(case) {
  combine <- function(result, error_fct) {
    seeds <- lapply(result, function(x) x$seed)

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

    list(parameter = parameter, metrices = metrices)
  }

  data <- get(case)
  res <- lapply(names(data), function(name) {
    combine(data[[name]], name)
  })

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
  plot_parameter(res)
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
