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
cis <- function(data) {
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
kde4d_intern <- function(df, n_samp = 2000) {
  H   <- ks::Hpi(as.matrix(df))
  fit <- ks::kde(as.matrix(df), H = H)
  draws <- as.data.frame(ks::rkde(n = n_samp, fhat = fit))
  colnames(draws) <- colnames(df)
  marginals <- lapply(seq_len(ncol(draws)), function(j) {
    d <- density(draws[[j]], n = 512)
    data.frame(x = d$x, y = d$y)
  })
  names(marginals) <- colnames(draws)
  list(fit = fit, draws = draws, marginals = marginals)
}
hdi_interval <- function(x, prob = 0.95) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n < 2) return(c(lower = NA_real_, upper = NA_real_))
  m <- max(1L, floor(prob * n))
  widths <- x[(m+1):n] - x[1:(n-m)]
  j <- which.min(widths)
  c(lower = x[j], upper = x[j + m])
}
kde4d_with_smr <- function(df, distris, prob = 0.95, n_samp = 20000,
                           ci_kind = c("hdi", "quantile", "mode_boot"),
                           boot_iter = 1000, density_n = 512) {
  ci_kind <- match.arg(ci_kind)
  df <- df[, 1:4, drop = FALSE]
  df <- as.data.frame(mapply(transform, df, distris, SIMPLIFY = FALSE))
  ki   <- kde4d_intern(df, n_samp = n_samp)
  fit  <- ki$fit
  Xs   <- ki$draws
  grid <- expand.grid(fit$eval.points)
  dens <- as.vector(fit$estimate)
  mode <- as.numeric(grid[which.max(dens), ])
  names(mode) <- colnames(df)
  if (ci_kind == "quantile") {
    alpha <- (1 - prob) / 2
    qs <- t(apply(Xs, 2, quantile, probs = c(alpha, 1 - alpha), na.rm = TRUE))
    colnames(qs) <- c("lower", "upper")
  } else if (ci_kind == "hdi") {
    qs <- t(apply(Xs, 2, function(col) hdi_interval(col, prob)))
    colnames(qs) <- c("lower", "upper")
  } else {
    qs <- matrix(NA_real_, nrow = ncol(df), ncol = 2,
                 dimnames = list(colnames(df), c("lower","upper")))
    n <- nrow(df)
    for (j in seq_len(ncol(df))) {
      d0 <- density(Xs[[j]], n = density_n)
      mb <- numeric(boot_iter)
      for (b in seq_len(boot_iter)) {
        bdf <- df[sample.int(n, n, replace = TRUE), , drop = FALSE]
        kb  <- kde4d_intern(bdf, n_samp = nrow(Xs))
        db  <- density(kb$draws[[j]], n = density_n)
        mb[b] <- db$x[ which.max(db$y) ]
      }
      qs[j, ] <- stats::quantile(mb, c((1-prob)/2, 1-(1-prob)/2), names = FALSE)
    }
  }
  list(
    mode     = mode,
    lower_ci = qs[, "lower"],
    upper_ci = qs[, "upper"],
    df       = ki$marginals
  )
}
# Plotting
# ========================================
plotting <- function(data, idx, distri, density_data, kd4_m_ci) {
  data[, idx] <- transform(data[, idx], distri)
  bins <- hist(data[, idx], plot = FALSE)$breaks
  median_ci <- calc_location_ci_bootstrap(median, data[, idx])
  median_iqr <- median_iqr(data[, idx])
  mean_ci <- calc_location_ci_bootstrap(mean, data[, idx])
  kde_ci <- cis(data[, idx])
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
final_plot

# Either update plot and table or change wording from CI to
# per-parameter ranges inside the 95% joint KDE level set (an axis-aligned “HDR box”).
