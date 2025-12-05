# dye conc for DBA 151 * 10^-6
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

    parameter <- res[[1]]$params
    parameter <- Reduce(rbind, parameter)
    parameter$ind <- interaction(parameter$dataset, parameter$repetition)
    parameter <- parameter[match(metrices$ind, parameter$ind), ]
    parameter$error <- metrices[match(parameter$ind, metrices$ind), "RootMeanSquareError"]
    parameter$quantile <- t

    states <- res[[1]]$states
    states <- Reduce(rbind, states)
    states$ind <- interaction(states$dataset, states$repetition)
    inds_states <- sapply(metrices$ind, function(i) {
      which(i == states$ind)
    })
    states <- states[inds_states, ]
    states$quantile <- t

    list(
      states = states,
      parameter = parameter,
      metrices = metrices
    )
  })

  state_list <- lapply(l, \(elem) elem$states)
  states <- Reduce(rbind, state_list)
  parameter_list <- lapply(l, \(elem) elem$parameter)
  parameter <- Reduce(rbind, parameter_list)
  metrices_list <- lapply(l, \(elem) elem$metrices)
  metrices <- Reduce(rbind, metrices_list)
  list(
    states = states,
    parameter = parameter,
    metrices = metrices
  )
}

configs <- list(
  dba = list(path = "./Paper/MeasurementVariance/dba_100Runs.RData",
    x = "total Host measured [M]", y = "Ka(HD) [1/M]"),
  ida = list(path = "./Paper/MeasurementVariance/ida_100.RData",
    x = "total Guest measured [M]", y = "Ka(HG) [1/M]"),
  gda = list(path = "./Paper/MeasurementVariance/gda_100.RData",
    x = "total Dye measured [M]", y = "Ka(HG) [1/M]")
)
dba <- configs[["dba"]]
ida <- configs[["ida"]]
gda <- configs[["gda"]]
thresholds <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)

res <- calc_errors_and_chose_sims(dba$path, thresholds)
dba_params <- res$parameter[res$parameter$quantile == 0.05, ]
write.csv(dba_params, "./Paper/DecentFitParameterVariance/dba_params.csv", quote = FALSE, row.names = FALSE)
# 0.000151
# 0; 0.000852459
res <- calc_errors_and_chose_sims(ida$path, thresholds)
ida_params <- res$parameter[res$parameter$quantile == 0.05, ]
write.csv(ida_params, "./Paper/DecentFitParameterVariance/ida_params.csv", quote = FALSE, row.names = FALSE)

res <- calc_errors_and_chose_sims(gda$path, thresholds)
gda_params <- res$parameter[res$parameter$quantile == 0.05, ]
write.csv(gda_params, "./Paper/DecentFitParameterVariance/gda_params.csv", quote = FALSE, row.names = FALSE)

cp <- function(config, thresholds) {
  res <- calc_errors_and_chose_sims(config$path, thresholds)
  df <- res$states
  dfs <- lapply(unique(df$dataset), \(x) {
    sub <- df[df$dataset == x, ]
    rep1 <- unique(sub$repetition)[[1]]
    sub <- sub[sub$repetition == rep1, ]
    q1 <- unique(sub$quantile)[[1]]
    sub[sub$quantile == q1, ]
  })
  df <- Reduce(rbind, dfs)
  res$states[[config$x]] <- factor(res$states[[config$x]])
  df[[config$x]] <- factor(df[[config$x]])
  p1 <- ggplot() +
    geom_point(data = res$states, aes(x = .data[[config$x]],
      y = .data[["Signal simulated"]], colour = quantile),
      position = position_dodge2(width = 0.5)) +
    geom_point(data = df, aes(x = .data[[config$x]],
      y = .data[["Signal measured"]], shape = factor(dataset)),
      position = position_dodge2(width = 0.5), size = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  res$parameter$quantile <- res$parameter$quantile * 100
  p2 <- ggplot(data = res$parameter, aes(x = quantile, group = quantile, y = .data[[config$y]])) +
    geom_boxplot() +
    labs(x = "% best simulations")
  p3 <- ggplot(data = res$parameter, aes(x = quantile, group = quantile, y = .data[["I(0)"]])) +
    geom_boxplot() +
    labs(x = "% best simulations")
  p4 <- ggplot(data = res$parameter, aes(x = quantile, group = quantile, y = .data[["I(HD) [1/M]"]])) +
    geom_boxplot() +
    labs(x = "% best simulations")
  p5 <- ggplot(data = res$parameter, aes(x = quantile, group = quantile, y = .data[["I(D) [1/M]"]])) +
    geom_boxplot() +
    labs(x = "% best simulations")
  pp <- plot_grid(plotlist = list(p2, p3, p4, p5))
  plot_grid(plotlist = list(p1, pp), nrow = 2L)
}

p <- cp(dba, thresholds)
ggsave("./Paper/DecentFitParameterVariance/DBA.png", p)
p <- cp(ida, thresholds)
ggsave("./Paper/DecentFitParameterVariance/IDA.png", p)
p <- cp(gda, thresholds)
ggsave("./Paper/DecentFitParameterVariance/GDA.png", p)
