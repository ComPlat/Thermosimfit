library(ggplot2)
library(ks)
library(cowplot)

# Load the results
# ========================================
extract_best_runs <- function(res) {
  states <- res$states
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  states <- Reduce(rbind, states)
  params <- Reduce(rbind, params)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50, ]
    res <- params_subset[params_subset$repetition %in% errors_subset$repetition, ]
    res <- res[, 1:4]
    res$error <- errors_subset$MeanSquareError
    return(res)
  })
  params <- Reduce(rbind, params)
  return(params)
}

load_params <- function(path) {
  load(path)
  extract_best_runs(res[[1]])
}

p_dba <- load_params("../MeasurementVariance/dba_100Runs.RData")
p_ida <- load_params("../MeasurementVariance/ida_100.RData")
p_gda <- load_params("../MeasurementVariance/gda_100.RData")
