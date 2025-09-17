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
# Across the relevant concentration range, the median MANE remains below 5%,
# indicating robust agreement between experiment and simulation.

calc_errors <- function(df_l) {
  lapply(df_l, function(x) {
    signal <- x[, 2]
    signalInsilico <- x[, 3]
    return(abs(signal - signalInsilico) / signal)
  }) |> unlist()
}

error_exploration <- function(case, path) {
  load(path)
  seeds <- lapply(result, function(x) x$seed)
  df_l <- lapply(seq_len(length(result)), function(idx) {
    res <- result[[idx]][[1]]
    res$seed <- seeds[[idx]]
    return(res)
  })
  errors <- calc_errors(df_l)
  df <- Reduce(rbind, df_l)
  df$errors <- errors
  x <- list(dba = "total Host measured [M]", gda = "total Dye measured [M]", ida = "total Guest measured [M]")[[case]]
  ggplot(data = df, aes(x = .data[[x]], y = errors, group = .data[[x]])) +
    geom_boxplot() +
    labs(y = "MANE", title = case)
}

p_ida <- error_exploration("ida", "./Paper/DecentFitParameterVariance/IDA_10_different_seeds.RData")
p_gda <- error_exploration("gda", "./Paper/DecentFitParameterVariance/GDA_10_different_seeds.RData")
p_dba <- error_exploration("dba", "./Paper/DecentFitParameterVariance/DBA_10_different_seeds.RData")
p <- cowplot::plot_grid(p_ida, p_gda, p_dba)
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot1.png", p, bg = "white")

remove_really_bad_runs <- function(res) {
  states <- res$states
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  states <- Reduce(rbind, states)
  states <- lapply(unique(errors$dataset), function(x) {
    states_subset <- states[states$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    upper_bound <- quantile(errors_subset$MeanSquareError, 0.9)
    indices <- which(errors_subset$MeanSquareError < upper_bound)
    errors_subset <- errors_subset[indices, ]
    states_subset[states_subset$repetition %in% errors_subset$repetition, ]
  })
  states
}
error_exploration_100 <- function(case, path) {
  load(path)
  states_l <- remove_really_bad_runs(res[[1]])
  for (i in 1:3) {
    temp <- split(states_l[[i]], states_l[[i]]$repetition)
    states_l[[i]]$errors <- calc_errors(temp)
  }
  df <- Reduce(rbind, states_l)
  temp <- split(df, df$dataset)
  print(head(temp[[1]]))
  print(head(temp[[2]]))
  print(head(temp[[3]]))
  x <- list(dba = "total Host measured [M]", gda = "total Dye measured [M]", ida = "total Guest measured [M]")[[case]]
  ggplot(data = df, aes(x = .data[[x]], y = errors, group = .data[[x]])) +
    geom_boxplot() +
    facet_wrap(~ dataset, scales = "free") + labs(y = "MANE")
}
p_ida <- error_exploration_100("ida", "./Paper/MeasurementVariance/ida_100.RData")
p_gda <- error_exploration_100("gda", "./Paper/MeasurementVariance/gda_100.RData")
p_dba <- error_exploration_100("dba", "./Paper/MeasurementVariance/dba_100Runs.RData")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot100_IDA.png", p_ida, bg = "white")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot100_GDA.png", p_gda, bg = "white")
ggsave("./Paper/DecentFitParameterVariance/ErrorPlot100_DBA.png", p_dba, bg = "white")
