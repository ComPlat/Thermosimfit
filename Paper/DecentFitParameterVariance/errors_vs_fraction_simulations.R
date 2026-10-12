library(ggplot2)
library(cowplot)

calc_errors_and_chose_sims <- function(path, thresholds) {
  load(path)
  l <- lapply(thresholds, function(t) {
    metrices <- res[[1]]$metrices
    metrices <- Reduce(rbind, metrices)
    upper_bound <- quantile(metrices$RootMeanSquareError, t)
    indices <- which(metrices$RootMeanSquareError < upper_bound)
    metrices <- metrices[indices, ]
    metrices$ind <- interaction(metrices$dataset, metrices$repetition)
    metrices$quantile <- t

    states <- res[[1]]$states
    states <- Reduce(rbind, states)
    states$ind <- interaction(states$dataset, states$repetition)
    inds_states <- sapply(metrices$ind, function(i) {
      which(i == states$ind)
    })
    states <- states[inds_states, ]
    states$quantile <- t

    states
  })
  states <- Reduce(rbind, l)

  states_new <- lapply(unique(states$quantile), function(q) {
    sub <- states[states$quantile == q, ]
    l <- lapply(unique(sub$ind), function(i) {
      subsub <- sub[sub$ind == i, ]
      e <- tsf:::rel_err(subsub[["Signal simulated"]], subsub[["Signal measured"]])
      subsub$rel_error <- e
      subsub
    })
    Reduce(rbind, l)
  })
  states <- Reduce(rbind, states_new)
  states
}

configs <- list(
  dba = list(path = "./Paper/MeasurementVariance/dba_100Runs.RData",
    x = "total Host measured [M]"),
  ida = list(path = "./Paper/MeasurementVariance/ida_100.RData",
    x = "total Guest measured [M]"),
  gda = list(path = "./Paper/MeasurementVariance/gda_100.RData",
    x = "total Dye measured [M]")
)
dba <- configs[["dba"]]
ida <- configs[["ida"]]
gda <- configs[["gda"]]
thresholds <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)

res_dba <- calc_errors_and_chose_sims(dba$path, thresholds)
res_ida <- calc_errors_and_chose_sims(ida$path, thresholds)
res_gda <- calc_errors_and_chose_sims(gda$path, thresholds)

cp <- function(df) {
  df$quantile <- df$quantile * 100
  ggplot(data = df, aes(x = quantile, group = quantile, y = rel_error)) +
    geom_boxplot() +
    labs(x = "% best simulations", y = "Rel. Error")
}

p <- cp(res_dba)
ggsave("./Paper/DecentFitParameterVariance/DBA_Rel_Error_vs_Fractions_Sim.png", p)
p <- cp(res_ida)
ggsave("./Paper/DecentFitParameterVariance/IDA_Rel_Error_vs_Fractions_Sim.png", p)
p <- cp(res_gda)
ggsave("./Paper/DecentFitParameterVariance/GDA_Rel_Error_vs_Fractions_Sim.png", p)
p
