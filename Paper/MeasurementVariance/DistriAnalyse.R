setwd("./Paper/MeasurementVariance/")
library(ggplot2)

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
p_ida <- load_params("ida_100.RData")

# Kernel density estimation
# ========================================
hdi_interval <- function(x, prob = 0.95) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n < 2) return(c(lower = NA_real_, upper = NA_real_))
  m <- max(1L, floor(prob * n))
  widths <- x[(m+1):n] - x[1:(n-m)]
  j <- which.min(widths)
  c(lower = x[j], upper = x[j + m])
}

kde_fit_and_draws <- function(X, n_samp = 2000) {
  H   <- ks::Hpi(as.matrix(X)) # determines the bandwidth later used in kde
  fit <- ks::kde(as.matrix(X), H = H) # calculates: f_hat(X*) where f_hat is the joint kernel density
  # Result: The estimated joint kernel density (fit$estimate) has the same dimensions as X
  draws <- as.data.frame(ks::rkde(n = n_samp, fhat = fit)) # Derived quantities from kernel density estimates
  # draws N_samp time from the previously calculated joint kernel density
  # Result: data.frame with n_samp rows and the ncol(X)
  marginals <- lapply(seq_len(ncol(draws)), function(j) {
    d <- density(draws[[j]], n = 512) # calculates a smoothed marginal PDF (kernel density estimate)
    data.frame(x = d$x, y = d$y)
  })
  names(marginals) <- colnames(draws)
  list(fit = fit, draws = draws, marginals = marginals)
}

print_progress_bootstrap <- function(c, ncol, b, boot_iter) {
  already_done <- (c - 1) / ncol
  from_current_done <- (b / boot_iter) / ncol
  print(paste0(100 * (already_done + from_current_done), "%"))
}

bootstrap_mode_ci <- function(df, Xs, density_n, boot_iter, prob) {
  qs <- matrix(NA_real_, nrow = ncol(df), ncol = 2,
    dimnames = list(colnames(df), c("lower","upper")))
  n <- nrow(df)
  for (j in seq_len(ncol(df))) {
    d0 <- density(Xs[[j]], n = density_n)
    mb <- numeric(boot_iter)
    for (b in seq_len(boot_iter)) {
      bdf <- df[sample.int(n, n, replace = TRUE), , drop = FALSE]
      kb  <- kde_fit_and_draws(bdf, n_samp = nrow(Xs))
      db  <- density(kb$draws[[j]], n = density_n)
      mb[b] <- db$x[ which.max(db$y) ]
      print_progress_bootstrap(j, ncol(df), b, boot_iter)
    }
    qs[j, ] <- stats::quantile(mb, c((1-prob)/2, 1-(1-prob)/2), names = FALSE)
  }
  qs
}

joint_jde_mode_and_intervals <- function(df, prob = 0.95, n_samp = 20000,
                                 spread_estimation = "mode_boot",
                                 boot_iter = 1000, density_n = 512) {
  spread_estimators <- c("hdi", "quantile", "mode_boot")
  stopifnot("Unknown spread estimator" = any(spread_estimation == spread_estimators))

  ki   <- kde_fit_and_draws(df, n_samp = n_samp)
  fit  <- ki$fit
  Xs   <- ki$draws
  grid <- expand.grid(fit$eval.points)
  dens <- as.vector(fit$estimate)
  mode <- as.numeric(grid[which.max(dens), ])
  names(mode) <- colnames(df)
  if (spread_estimation == "quantile") {
    alpha <- (1 - prob) / 2
    qs <- t(apply(Xs, 2, quantile, probs = c(alpha, 1 - alpha), na.rm = TRUE))
    colnames(qs) <- c("lower", "upper")
  } else if (spread_estimation == "hdi") {
    qs <- t(apply(Xs, 2, function(col) hdi_interval(col, prob)))
    colnames(qs) <- c("lower", "upper")
  } else {
    qs <- bootstrap_mode_ci(df, Xs, density_n, boot_iter, prob)
  }
  list(
    mode     = mode,
    lower_ci = qs[, "lower"],
    upper_ci = qs[, "upper"],
    df       = ki$marginals
  )
}

res_bootstrapped <- joint_jde_mode_and_intervals(p_ida[, 1:4], boot_iter = 100)

res <- res_bootstraped
df_lists <- res$df
idx <- 1L
df <- df_lists[[idx]]
bins <- hist(df[, 2L], plot = FALSE)$breaks
location_error <- data.frame(
  y = 2.5e-9, x = res$mode[[1L]],
  xmin = res$lower_ci[[1L]], xmax = res$upper_ci[[1L]]
)

p <- ggplot() +
  geom_point(data = df, aes(x = x, y = y)) +
  geom_errorbarh(
    data = location_error,
    aes(xmin = xmin, xmax = xmax, y = y),
    height = 1e-9, size = 0.8
  ) +
  labs(y = "Joint kernel density", x = "Ka(HG) [1/M]")
p
