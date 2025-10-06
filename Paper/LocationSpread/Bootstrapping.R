# Median IQR
# ========================================
median_iqr <- function(x) {
  c(median(x), quantile(x, 0.25), quantile(x, 0.75))
}

# Bootstrap method for calculating CI for the median
# ========================================
calc_location_ci_bootstrap <- function(
    location_fct,
    data, n_iter = 10000,
    conf_level = 0.95) {
  locations <- numeric(n_iter)
  for (i in 1:n_iter) {
    sample_data <- sample(
      data,
      size = length(data),
      replace = TRUE
    )
    locations[i] <- location_fct(sample_data)
  }
  lower_ci <- quantile(locations, (1 - conf_level) / 2)
  upper_ci <- quantile(locations, 1 - (1 - conf_level) / 2)
  # Confidence that true location is lying there
  return(
    list(
      location = location_fct(data),
      lower_ci = lower_ci, upper_ci = upper_ci
    )
  )
}

# Kernel density estimation
# ========================================
# Draws 10000 times of HDR to estimate CIs
modus_ci_hdr <- function(data) {
  res <- density(data)
  mode <- res$x[which.max(res$y)]

  n_iter <- 10000
  modes <- numeric(n_iter)
  for (i in 1:n_iter) {
    sample_data <- sample(
      data,
      size = length(data),
      replace = TRUE
    )
    res <- density(sample_data)
    modes[i] <- res$x[which.max(res$y)]
  }
  lower_ci <- quantile(modes, 0.025)
  upper_ci <- quantile(modes, 0.975)
  res <- data.frame(x = res$x, y = res$y)
  return(
    list(
      kd = res,
      mode = mode,
      lower_ci = lower_ci, upper_ci = upper_ci
    )
  )
}
