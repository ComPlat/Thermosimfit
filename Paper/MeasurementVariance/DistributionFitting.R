setwd("/home/konrad/Documents/Thermosimfit/Paper/MeasurementVariance/")
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

p_dba <- load_params("dba_100Runs.RData")
p_ida <- load_params("ida_100.RData")
p_gda <- load_params("gda_100.RData")

# Transform the data
# ========================================
transform <- function(data, distri) {
  data <- (data - min(data)) / (max(data) - min(data))
  data <- ifelse(data < 1e-6, 1e-6, data) # Avoid log(0)
  if (distri != "norm" && distri != "exp") {
    if (min(data) < 1e-2) {
      data <- data + 0.01
    }
  }
  return(data)
}

# Calculate start values
# ========================================
start_params_beta <- function(data) {
  mu <- mean(data)
  sigma_sq <- var(data)
  alpha <- mu * ((mu * (1 - mu)) / sigma_sq - 1)
  beta <- (1 - mu) * ((mu * (1 - mu)) / sigma_sq - 1)
  alpha <- max(alpha, 1e-6)
  beta <- max(beta, 1e-6)
  return(c(alpha, beta))
}

# Distribution fitting
# ========================================
dfn <- function(data, params) {
  dnorm(
    data,
    mean = params[1],
    sd = params[2],
    log = TRUE
  )
}
dfln <- function(data, params) {
  res <- dlnorm(
    data,
    meanlog = params[1],
    sdlog = params[2],
    log = TRUE
  )
  return(res)
}
dfexp <- function(data, params) {
  dexp(
    data,
    rate = params[1],
    log = TRUE
  )
}
dfgamma <- function(data, params) {
  dgamma(
    data,
    shape = params[1],
    scale = params[2],
    log = TRUE
  )
}
dfweibull <- function(data, params) {
  dweibull(
    data,
    shape = params[1],
    scale = params[2],
    log = TRUE
  )
}
dfbeta <- function(data, params) {
  dbeta(
    data,
    shape1 = params[1],
    shape2 = params[2],
    log = TRUE
  )
}
pretty_params <- function(parameter, errors, distri) {
  format_with_error <- function(param, error) {
    param <- round(param, 2)
    error <- round(error, 2)
    paste0(param, " ± ", error)
  }
  switch(distri,
    "exp" = paste0(
      "Exp(λ = ", format_with_error(parameter[1], errors[1]),
      ")"
    ),
    "norm" = paste0(
      "N(µ = ", format_with_error(parameter[1], errors[1]),
      ",\n  σ = ", format_with_error(parameter[2], errors[2]),
      ")"
    ),
    "lognorm" = paste0(
      "LogN(µ = ", format_with_error(parameter[1], errors[1]),
      ", σ = ", format_with_error(parameter[2], errors[2]),
      ")"
    ),
    "gamma" = paste0(
      "Gamma(α = ", format_with_error(parameter[1], errors[1]),
      ", β = ", format_with_error(parameter[2], errors[2]),
      ")"
    ),
    "weibull" = paste0(
      "Weibull(k = ", format_with_error(parameter[1], errors[1]),
      ", λ = ", format_with_error(parameter[2], errors[2]),
      ")"
    ),
    "beta" = paste0(
      "Beta(α = ", format_with_error(parameter[1], errors[1]),
      ", β = ", format_with_error(parameter[2], errors[2]),
      ")"
    ),
    paste0("Unsupported distribution: ", distri)
  )
}
fit_distri <- function(data, distri) {
  df <- switch(distri,
    "exp" = dfexp,
    "norm" = dfn,
    "lognorm" = dfln,
    "gamma" = dfgamma,
    "weibull" = dfweibull,
    "beta" = dfbeta
  )
  lf <- function(params) {
    res <- sum(
      df(data, params)
    )
    return(res)
  }
  start_params <- switch(distri,
    "norm" = c(mean(data), sd(data)),
    "lognorm" = c(mean(log(data)), sd(log(data))),
    "exp" = c(1),
    "gamma" = c(mean(data)^2 / var(data), var(data) / mean(data)),
    "weibull" = c(1, mean(data)),
    "beta" = start_params_beta(data),
    stop("Unsupported distribution")
  )
  op <- optim(
    par = start_params,
    fn = lf,
    control = list(fnscale = -1),
    hessian = TRUE
  )
  x_vals <- seq(min(data), max(data), length.out = 100)
  y_vals <- exp(df(x_vals, op$par))
  return(
    list(
      df = data.frame(x = x_vals, y = y_vals),
      params = pretty_params(op$par, sqrt(diag(solve(-op$hessian))), distri)
    )
  )
}

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
kde <- function(data) {
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
kde4d_intern <- function(df) {
  mins <- apply(df, 2, min)
  maxs <- apply(df, 2, max)
  res <- ks::kde(df, xmin = mins, xmax = maxs)
  grid_points <- expand.grid(res$eval.points)
  joint_densities <- as.vector(res$estimate)
  density_data <- cbind(grid_points, joint_density = joint_densities)
  return(density_data)
}
kde4d_bootstrap <- function(df, idx, n_iter = 10) {
  temp <- kde4d_intern(df)
  temp <- temp[!duplicated(temp[, idx]), ]
  mode <- temp[which.max(temp[, 5]), idx]
  modes <- numeric(n_iter)
  res <- NULL
  for (i in 1:n_iter) {
    cat("Bootstrap iteration: ", i, "\n")
    bootstrap_sample <- df[sample(1:nrow(df), replace = TRUE), ]
    temp <- kde4d_intern(bootstrap_sample)
    temp <- temp[!duplicated(temp[, idx]), ]
    temp[, 5] <- transform(temp[, 5], "norm")
    res <- temp
    modes[i] <- temp[which.max(temp[, 5]), idx]
  }
  lower_ci <- quantile(modes, 0.025)
  upper_ci <- quantile(modes, 0.975)

  res <- kde4d_intern(df)
  # res <- res[!duplicated(res[, idx]), ]
  res[, 5] <- transform(res[, 5], "norm")

  return(list(
    res = res,
    mode = mode,
    lower_ci = lower_ci, upper_ci = upper_ci
  ))
}
kde4d <- function(df, distris) {
  df <- df[, 1:4]
  for (i in 1:4) {
    df[, i] <- transform(df[, i], distris[i])
  }
  res <- lapply(1:4, function(x) {
    kde4d_bootstrap(df, x)
  })
  return(
    list(
      mode = lapply(res, function(x) x$mode),
      lower_ci = lapply(res, function(x) x$lower_ci),
      upper_ci = lapply(res, function(x) x$upper_ci),
      df = lapply(res, function(x) {
        i <- parent.frame()$i[]
        data.frame(x = x$res[, i], y = x$res[, 5])
      })
    )
  )
}
# NOTE: Using Contour Levels for Significant Model Regions
# Example call: result <- kde4d_with_smr(df, rep("norm", 4))
# Significant Model Regions (SMR)
#
# Goal:
# Identify regions of the density that contain a specified proportion
# of the total probability mass (e.g., 95% for a confidence region).
#
# Steps:
#  Calculate density estimates at all grid points.
#  the density threshold (res$cont[level])
#  for the desired cumulative probability (e.g., 0.95).
#  Include only those grid points where the density is above the threshold.
#
# SMR highlights the most probable regions,
# ignoring the "tails" of the distribution.
kde4d_with_smr <- function(df, distris, prob = 0.95) {
  df <- df[, 1:4]
  df <- mapply(transform, df, distris, SIMPLIFY = FALSE)
  df <- as.data.frame(df)
  res <- ks::kde(df)
  level <- paste0(prob * 100, "%")
  density_threshold <- res$cont[level]
  grid_points <- expand.grid(res$eval.points)
  densities <- as.vector(res$estimate)
  significant_points <- grid_points[densities >= density_threshold, ]
  mode_index <- which.max(densities)
  mode <- grid_points[mode_index, ]
  mode <- ifelse(mode < 0, 0, mode) |> as.numeric()
  CIs <- apply(significant_points, 2, range)
  lc <- CIs[1, ]
  lc <- ifelse(lc < 0, 0, lc)
  uc <- CIs[2, ]
  uc <- ifelse(uc < 0, 0, uc)
  res <- kde4d_intern(df)
  res[, 5] <- transform(res[, 5], "norm")
  df <- lapply(1:4, function(x) {
    i <- parent.frame()$i[]
    data.frame(x = res[, i], y = res[, 5])
  })
  return(list(
    mode = mode,
    lower_ci = lc,
    upper_ci = uc,
    df = df
  ))
}

# Plotting
# ========================================
plotting <- function(data, idx, distri, density_data, kd4_m_ci) {
  data[, idx] <- transform(data[, idx], distri)
  bins <- hist(data[, idx], plot = FALSE)$breaks
  median_ci <- calc_location_ci_bootstrap(median, data[, idx])
  median_iqr <- median_iqr(data[, idx])
  mean_ci <- calc_location_ci_bootstrap(mean, data[, idx])
  kde_ci <- kde(data[, idx])
  fd <- fit_distri(data[, idx], distri)
  df <- fd$df
  df$linetype <- "Fitted distribution"
  kde_ci$kd$linetype <- "Kernel density"
  df <- rbind(df, kde_ci$kd)
  x <- c(0.75, 0.4, 1, 0.7)[idx] # TODO: change if it is integrated in tsf
  y <- c(3, 3, 1.4, 2.1)[idx]
  location_error <- data.frame(
    x = c(
      mean_ci$location, median_ci$location, kde_ci$mode, kd4_m_ci[1], median_iqr[1]
    ),
    xmin = c(
      mean_ci$lower_ci, median_ci$lower_ci, kde_ci$lower_ci, kd4_m_ci[2], median_iqr[2]
    ),
    xmax = c(
      mean_ci$upper_ci, median_ci$upper_ci, kde_ci$upper_ci, kd4_m_ci[3], median_iqr[3]
    ),
    type = c(
      "Mean", "Median", "Mode (kernel density)", "Mode (joint kernel density)", "Median IQR"
    ),
    y = c(0.5, 0.6, 0.7, 0.8, 0.9)
  )

  p <- ggplot() +
    geom_histogram(
      data = data.frame(x = data[, idx]),
      binwidth = bins[2] - bins[1],
      aes(x = x, y = ..density..)
    ) +
    geom_rug(
      data = data.frame(x = data[, idx]),
      aes(x = data[, idx])
    ) +
    geom_line(
      data = df,
      aes(
        x = x, y = y,
        linetype = linetype
      )
    ) +
    geom_point(
      data = density_data,
      aes(x = x, y = y, shape = "joint kernel density")
    ) +
    geom_label(
      aes(
        x = x,
        y = y,
        label = fd$params
      ), size = 6
    ) +
    labs(x = names(data)[idx], y = "Density")

  colors <- RColorBrewer::brewer.pal(5, "Dark2")
  p <- p +
    geom_errorbarh(
      data = location_error,
      aes(xmin = xmin, xmax = xmax, y = y, color = type),
      height = 0.01, size = 0.8
    ) +
    geom_point(
      data = location_error,
      aes(x = x, y = y, color = type),
      size = 1
    ) +
    scale_color_manual(
      name = "Location ± 95% CI",
      values = c(
        "Mean" = colors[1],
        "Median" = colors[2],
        "Mode (kernel density)" = colors[3],
        "Mode (joint kernel density)" = colors[4],
        "Median IQR" = colors[5]
      )
    ) +
    scale_linetype_manual(
      name = NULL,
      values = setNames(
        c("solid", "dashed", "dotted"),
        c("Fitted distribution", "Kernel density", "Joint kernel density")
      )
    ) +
    scale_shape_manual(
      name = NULL,
      values = c("Joint kernel density" = 1)
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 18),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18)
    ) +
    guides(
      linetype = guide_legend(order = 1, keywidth = 2, keyheight = 1),
      color    = guide_legend(order = 2, keywidth = 2, keyheight = 1)
    )
  return(p)
}

distris <- c("lognorm", "exp", "norm", "weibull")
# kdjoint <- kde4d(p_ida, distris)
kdjoint_smr <- kde4d_with_smr(p_ida, distris)
plots <- lapply(1:4, function(x) {
  df <- kdjoint_smr$df[[x]]
  m <- kdjoint_smr$mode[[x]]
  l <- kdjoint_smr$lower_ci[[x]]
  u <- kdjoint_smr$upper_ci[[x]]
  plotting(p_ida, x, distris[x], df, c(m, l, u))
})
legend <- get_legend(plots[[1]])
plots <- lapply(plots, function(x) {
  x + theme(legend.position = "none")
})
plot_grid <- plot_grid(
  plotlist = plots, nrow = 4,
  labels = c("A", "B", "C", "D"), label_size = 18
)
final_plot <- plot_grid(
  plot_grid, legend,
  ncol = 2,
  rel_widths = c(1, 0.2)
)

ggsave(final_plot,
  bg = "white",
  file = "LocationEstimation.png",
  width = 24,
  height = 16
)

# Calculate results
distis <- rep("norm", 4)
back_transform <- function(transformed_data, distri,
                           original_min, original_max) {
  original_data <- transformed_data *
    (original_max - original_min) + original_min
  return(original_data)
}
calc_values <- function(df, distris) {
  res <- kde4d_with_smr(df, distris)
  res$mode
  res$lower_ci
  res$upper_ci
  res <- lapply(1:4, function(idx) {
    max <- max(df[, idx])
    min <- min(df[, idx])
    mode <- back_transform(res$mode[idx], distris[idx], min, max)
    l <- back_transform(res$lower_ci[idx], distris[idx], min, max)
    u <- back_transform(res$upper_ci[idx], distris[idx], min, max)
    df_temp <- data.frame(
      values = sprintf("%.3e", c(mode, l, u)),
      type = c("mode", "lower", "upper")
    )
    names(df_temp)[1] <- names(df)[idx]
    return(df_temp)
  })
  res <- lapply(res, function(x) {
    x[, 1]
  })
  res <- Reduce(rbind, res) |> as.data.frame()
  res <- cbind(names(df)[1:4], res)
  names(res) <- c("Parameter", "mode", "lower", "upper")
  row.names(res) <- NULL
  return(res)
}
dba_res <- calc_values(p_dba, distris)
dba_res
ida_res <- calc_values(p_ida, distris)
ida_res
calc_values(p_gda, distris)


# Calc IQR
calc_iqr <- function(df) {
  df <- df[, 1:4]
  lapply(1:4, function(x) {
    median_iqr(df[, x])
  })
}
calc_iqr(p_dba)
