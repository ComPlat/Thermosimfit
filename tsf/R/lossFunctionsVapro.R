# VAPRO (variable projection) variants of the loss functions in lossFunctions.R
# The nonlinear binding parameter(s) (Kd / Kga) are optimized by the outer
# optimizer via `parameter`, while the linear intercept/slope parameters for
# each signal are profiled out in closed form using non-negative least
# squares (nnls), instead of being part of `parameter`.

# DBA is case DBA with const dye and increasing host - VAPRO version
lossFctDBAVapro <- function(parameter, env, eval = FALSE) {
  # Non linear part: Kd optimized
  # =======================================================================
  sol <- solve_h_dba(parameter[1], env$host, env$d0)
  if (anyNA(sol$d)) {
    return(.Machine$double.xmax)
  }

  # Linear part: determine the optimal I parameters
  # =======================================================================
  X <- cbind(Intercept = 1, hd = sol$hd, d = sol$d)
  betas <- list()
  fit_one <- function(y) {
    eps <- 1e-12
    w <- 1 / pmax(abs(y), eps)
    Xw <- X * w
    yw <- y * w
    fit <- nnls::nnls(Xw, yw) # Non-Negative Least Squares
    beta <- coef(fit) # all >= 0
    betas <<- c(betas, beta)
    as.vector(X %*% beta)
  }
  if (!is.data.frame(env$signal)) env$signal <- as.data.frame(env$signal)
  insilico_signals <- lapply(1:ncol(env$signal), function(x) {
    fit_one(env$signal[[x]])
  })

  if (eval) {
    insilico_mat <- Reduce(cbind, insilico_signals)
    return(list(
      data.frame(insilico_mat, d = sol$d, hd = sol$hd),
      do.call(c, betas)
    ))
  }
  # Calc mean error across all signals
  # =======================================================================
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}

# HG is DBA with increasing dye - VAPRO version
lossFctHGVapro <- function(parameter, env, eval = FALSE) {
  # Non linear part: Kd optimized
  # =======================================================================
  sol <- solve_h_dba(parameter[1], env$h0, env$dye)
  if (anyNA(sol$d)) {
    return(.Machine$double.xmax)
  }

  # Linear part: determine the optimal I parameters
  # =======================================================================
  X <- cbind(Intercept = 1, hd = sol$hd, d = sol$d)
  betas <- list()
  fit_one <- function(y) {
    eps <- 1e-12
    w <- 1 / pmax(abs(y), eps)
    Xw <- X * w
    yw <- y * w
    fit <- nnls::nnls(Xw, yw) # Non-Negative Least Squares
    beta <- coef(fit) # all >= 0
    betas <<- c(betas, beta)
    as.vector(X %*% beta)
  }
  if (!is.data.frame(env$signal)) env$signal <- as.data.frame(env$signal)
  insilico_signals <- lapply(1:ncol(env$signal), function(x) {
    fit_one(env$signal[[x]])
  })

  if (eval) {
    insilico_mat <- Reduce(cbind, insilico_signals)
    return(list(
      data.frame(insilico_mat, d = sol$d, hd = sol$hd),
      do.call(c, betas)
    ))
  }
  # Calc mean error across all signals
  # =======================================================================
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}

# IDA - VAPRO version
lossFctIDAVapro <- function(parameter, env, eval = FALSE) {
  # Non linear part: Kga optimized (Kd taken from env, fixed by prior DBA fit)
  # =======================================================================
  sol <- solve_h_ida_gda(env$kd, parameter[1], env$h0, env$d0, env$ga)
  if (anyNA(sol$d)) {
    return(.Machine$double.xmax)
  }

  # Linear part: determine the optimal I parameters
  # =======================================================================
  X <- cbind(Intercept = 1, hd = sol$hd, d = sol$d)
  betas <- list()
  fit_one <- function(y) {
    eps <- 1e-12
    w <- 1 / pmax(abs(y), eps)
    Xw <- X * w
    yw <- y * w
    fit <- nnls::nnls(Xw, yw) # Non-Negative Least Squares
    beta <- coef(fit) # all >= 0
    betas <<- c(betas, beta)
    as.vector(X %*% beta)
  }
  if (!is.data.frame(env$signal)) env$signal <- as.data.frame(env$signal)
  insilico_signals <- lapply(1:ncol(env$signal), function(x) {
    fit_one(env$signal[[x]])
  })

  if (eval) {
    insilico_mat <- Reduce(cbind, insilico_signals)
    return(list(
      data.frame(insilico_mat, d = sol$d, hd = sol$hd),
      do.call(c, betas)
    ))
  }
  # Calc mean error across all signals
  # =======================================================================
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}

# GDA - VAPRO version
lossFctGDAVapro <- function(parameter, env, eval = FALSE) {
  # Non linear part: Kga optimized (Kd taken from env, fixed by prior DBA fit)
  # =======================================================================
  sol <- solve_h_ida_gda(env$kd, parameter[1], env$h0, env$dye, env$ga0)
  if (anyNA(sol$d)) {
    return(.Machine$double.xmax)
  }

  # Linear part: determine the optimal I parameters
  # =======================================================================
  X <- cbind(Intercept = 1, hd = sol$hd, d = sol$d)
  betas <- list()
  fit_one <- function(y) {
    eps <- 1e-12
    w <- 1 / pmax(abs(y), eps)
    Xw <- X * w
    yw <- y * w
    fit <- nnls::nnls(Xw, yw) # Non-Negative Least Squares
    beta <- coef(fit) # all >= 0
    betas <<- c(betas, beta)
    as.vector(X %*% beta)
  }
  if (!is.data.frame(env$signal)) env$signal <- as.data.frame(env$signal)
  insilico_signals <- lapply(1:ncol(env$signal), function(x) {
    fit_one(env$signal[[x]])
  })

  if (eval) {
    insilico_mat <- Reduce(cbind, insilico_signals)
    return(list(
      data.frame(insilico_mat, d = sol$d, hd = sol$hd),
      do.call(c, betas)
    ))
  }
  # Calc mean error across all signals
  # =======================================================================
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}
