library(ggplot2)
library(cowplot)
# Secondly, across the parameter landscape, the authors select values with a MANE<1 as satisfactory.

# Error: sum(abs(signal - InsilicoSignal) / signal)
# Sum of absoulte normalised error
# If the error is 1 and we measured 21 data points we know that we have 4.76% diviation per data point (averaged)
#
# While relative errors appear large at the very beginning of the titration curves,
# this corresponds to very low signal intensities where small absolute
# deviations translate into inflated normalized errors.
# Across the relevant concentration range, the error remains below 5%,
# indicating robust agreement between experiment and simulation.

error_exploration <- function(case, path) {
  load(path)
  df_l <- lapply(seq_len(length(result)), function(idx) {
    res <- result[[idx]][[1]]
    signal <- res[, 2]
    signalInsilico <- res[, 3]
    res$errors <- abs(signal - signalInsilico) / signal
    res$resids <- signal - signalInsilico
    res
  })
  df <- Reduce(rbind, df_l)
  x <- list(
    DBA = "total Host measured [M]",
    GDA = "total Dye measured [M]",
    IDA = "total Guest measured [M]"
  )[[case]]
  p1 <- ggplot(data = df, aes(x = .data[[x]], y = errors, group = .data[[x]])) +
    geom_boxplot() +
    labs(y = "Rel. Error", title = case)
  p2 <- ggplot(data = df, aes(x = .data[[x]], y = resids)) +
    geom_point() +
    labs(y = "Residuals", x = NULL)
  temp <- data.frame(
    x = rep(df[, x], 2),
    Signal = c(df[, "Signal simulated"], df[, "Signal measured"]),
    group = c(rep("Simulated", nrow(df)), rep("Measured", nrow(df)))
  )
  p3 <- ggplot() +
    geom_point(data = temp, aes(x = x, y = Signal, colour = group)) +
    labs(x = x) +
    theme(legend.position="bottom", legend.title = element_blank())
  plot_grid(p1, p2, p3, nrow = 3)
}

p_ida <- error_exploration("IDA", "./Paper/DecentFitParameterVariance/IDA_10_different_seeds.RData")
p_gda <- error_exploration("GDA", "./Paper/DecentFitParameterVariance/GDA_10_different_seeds.RData")
p_dba <- error_exploration("DBA", "./Paper/DecentFitParameterVariance/DBA_10_different_seeds.RData")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot10_IDA.png", p_ida, bg = "white")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot10_GDA.png", p_gda, bg = "white")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot10_DBA.png", p_dba, bg = "white")

calc_errors_and_chose_sims <- function(res) {
  states <- res$states
  states <- Reduce(rbind, states)
  lapply(unique(states$dataset), function(x) {
    states_subset <- states[states$dataset == x, ]
    dfs <- split(states_subset, states_subset$repetition)
    temp <- lapply(dfs, function(t) {
      signal <- t[, 2]
      signalInsilico <- t[, 3]
      rel_errors <- abs(signal - signalInsilico) / signal
      sum <- sum(rel_errors)
      t$errors <- rel_errors
      t$resids <- signal - signalInsilico
      list(t, sum)
    })
    errors <- lapply(temp, function(x) x[[2]]) |> unlist()
    upper_bound <- quantile(errors, 0.9)
    indices <- which(errors < upper_bound)
    dfs <- lapply(temp, function(x) x[[1]])
    Reduce(rbind, dfs[indices])
  })
}
error_exploration_100 <- function(case, path) {
  load(path)
  states_l <- calc_errors_and_chose_sims(res[[1]])
  df <- Reduce(rbind, states_l)
  pl <- function(df, case, dataset_idx) {
    x <- list(
      dba = "total Host measured [M]",
      gda = "total Dye measured [M]",
      ida = "total Guest measured [M]")[[case]]
    p1 <- ggplot(data = df, aes(x = .data[[x]], y = errors, group = .data[[x]])) +
      geom_boxplot() +
      labs(title = paste0("Dataset: ", dataset_idx), y = "Rel. Error", x = NULL)
    p2 <- ggplot(data = df, aes(x = .data[[x]], y = resids)) +
      geom_point() +
      labs(y = "Residuals", x = NULL)
    temp <- data.frame(
      x = rep(df[, x], 2),
      Signal = c(df[, "Signal simulated"], df[, "Signal measured"]),
      group = c(rep("Simulated", nrow(df)), rep("Measured", nrow(df)))
    )
    p3 <- ggplot() +
      geom_point(data = temp, aes(x = x, y = Signal, colour = group)) +
      labs(x = x) +
      theme(legend.position="bottom", legend.title = element_blank())
    plot_grid(p1, p2, p3, nrow = 3)
  }
  ps <- lapply(unique(df$dataset), function(i) {
    sub_df <- df[df$dataset == i, ]
    p <- pl(sub_df, case, i)
  })
  plot_grid(plotlist = ps)
}
p_ida <- error_exploration_100("ida", "./Paper/MeasurementVariance/ida_100.RData")
p_gda <- error_exploration_100("gda", "./Paper/MeasurementVariance/gda_100.RData")
p_dba <- error_exploration_100("dba", "./Paper/MeasurementVariance/dba_100Runs.RData")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot100_IDA.png", p_ida, bg = "white")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot100_GDA.png", p_gda, bg = "white")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot100_DBA.png", p_dba, bg = "white")
