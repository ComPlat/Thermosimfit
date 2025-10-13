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
  c <- max(data)
  data <- data / c

  # Log-likelihood definitions
  ll <- switch(distri,
    "norm"    = function(th) {
      mu <- th[1]; sigma <- exp(th[2])
      sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
    },
    "lognorm" = function(th) {
      mlog <- th[1]; slog <- exp(th[2])
      sum(dlnorm(data, meanlog = mlog, sdlog = slog, log = TRUE))
    },
    "exp"     = function(th) {
      rate <- exp(th[1])
      sum(dexp(data, rate = rate, log = TRUE))
    },
    "gamma"   = function(th) {
      shape <- exp(th[1]); scale <- exp(th[2])
      sum(dgamma(data, shape = shape, scale = scale, log = TRUE))
    },
    "weibull" = function(th) {
      shape <- exp(th[1]); scale <- exp(th[2])
      sum(dweibull(data, shape = shape, scale = scale, log = TRUE))
    },
    "beta"    = function(th) {
      a <- exp(th[1]); b <- exp(th[2])
      sum(dbeta(data, shape1 = a, shape2 = b, log = TRUE))
    },
    stop("Unsupported distribution")
  )

  # Starting values
  sp <- switch(distri,
    "norm"    = c(mean(data), log(sd(data) + 1e-8)),
    "lognorm" = c(mean(log(pmax(data, 1e-12))), log(sd(log(pmax(data, 1e-12))) + 1e-8)),
    "exp"     = c(log(1 / (mean(data) + 1e-8))),
    "gamma"   = { m <- mean(data); v <- var(data); 
                  shape <- max((m*m)/(v + 1e-12), 1e-8); scale <- max(v/(m + 1e-12), 1e-8);
                  c(log(shape), log(scale)) },
    "weibull" = c(log(1), log(mean(data) + 1e-8)),
    "beta"    = { ab <- start_params_beta(data); c(log(ab[1]), log(ab[2])) }
  )

  op <- optim(sp, fn = function(th) -ll(th), method = "BFGS", hessian = TRUE)

  # Back-transform parameters
  par_map_scaled <- switch(distri,
    "norm"    = c(op$par[1], exp(op$par[2])),
    "lognorm" = c(op$par[1], exp(op$par[2])),
    "exp"     = c(exp(op$par[1])),
    "gamma"   = c(exp(op$par[1]), exp(op$par[2])),
    "weibull" = c(exp(op$par[1]), exp(op$par[2])),
    "beta"    = c(exp(op$par[1]), exp(op$par[2]))
  )

  # Apply scaling rules
  par_map <- switch(distri,
    "norm"    = c(par_map_scaled[1] * c, par_map_scaled[2] * c),
    "lognorm" = c(par_map_scaled[1] + log(c), par_map_scaled[2]),
    "exp"     = c(par_map_scaled[1] / c),
    "gamma"   = c(par_map_scaled[1], par_map_scaled[2] * c),
    "weibull" = c(par_map_scaled[1], par_map_scaled[2] * c),
    "beta"    = c(par_map_scaled[1], par_map_scaled[2])  # α,β unchanged
  )

  # Covariance and SE back-transform
  cov_theta <- tryCatch(solve(op$hessian), error = function(e) NULL)
  J <- switch(distri,
    "norm"    = rbind(c(1, 0), c(0, exp(op$par[2]))),
    "lognorm" = rbind(c(1, 0), c(0, exp(op$par[2]))),
    "exp"     = matrix(exp(op$par[1]), nrow = 1, ncol = 1),
    "gamma"   = diag(exp(op$par[1:2])),
    "weibull" = diag(exp(op$par[1:2])),
    "beta"    = diag(exp(op$par[1:2]))
  )
  se_map_scaled <- if (!is.null(cov_theta)) {
    sqrt(diag(J %*% cov_theta %*% t(J)))
  } else {
    rep(NA_real_, length(par_map_scaled))
  }

  # Scale SEs back to original units (same rules as above)
  se_map <- switch(distri,
    "norm"    = se_map_scaled * c,
    "lognorm" = se_map_scaled,              # meanlog shift doesn't affect SE
    "exp"     = se_map_scaled / c,
    "gamma"   = c(se_map_scaled[1], se_map_scaled[2] * c),
    "weibull" = c(se_map_scaled[1], se_map_scaled[2] * c),
    "beta"    = se_map_scaled
  )

  # --------------------------
  # Back-transform x-values for plotting
  # --------------------------
  x_vals <- seq(min(data), max(data), length.out = 100) * c

  dfunc <- switch(distri, exp = dfexp, norm = dfn, lognorm = dfln,
                  gamma = dfgamma, weibull = dfweibull, beta = dfbeta)
  y_vals <- exp(dfunc(x_vals, par_map))

  list(
    df     = data.frame(x = x_vals, y = y_vals),
    params = pretty_params(par_map, se_map, distri),
    optim  = op
  )
}
