# GDA case. Entry points: loss_fct_gda_a2a, grid_search_gda_a2a (VAPRO),
# loss_fct_pso_gda_a2a (PSO).
# Same equation as IDA with the roles of dye and guest swapped: d0 is the
# varying vector (dye, from the data column) and g the fixed value (guest,
# from additionalParameters).

equation_h_gda <- function(h, params) {
  argtypes(
    h |> type(double),
    params |> type(EquationParamsGda)
  )
  returntype(double)
  if (h <= 0) {
    return(1.797693e+308)
  }
  denom_Kd <- 1 + params$Kd * h
  h_d <- (params$Kd * h * params$d0) / denom_Kd
  h_g <- 0
  if (params$g > 0) {
    denom_Kg <- 1 + params$Kg * h
    h_g <- (params$Kg * h * params$g) / denom_Kg
  }
  return(h + h_d + h_g - params$h0)
}

solve_h_gda <- function(Kd, Kg, h0, d0, g) {
  argtypes(
    Kd |> type(double) |> const(),
    Kg |> type(double) |> const(),
    h0 |> type(double) |> const(),
    d0 |> type(vec(double)) |> const(),
    g |> type(double) |> const()
  )
  returntype(SolveD_HD_Result)
  n <- length(d0)
  out |> type(SolveD_HD_Result)
  out$d <- numeric(n)
  out$hd <- numeric(n)

  h0_c <- h0
  if (h0_c <= 0) {
    h0_c <- 1e-10
  }

  params |> type(EquationParamsGda)
  params$Kd <- Kd
  params$Kg <- Kg
  params$h0 <- h0_c
  params$g <- g

  for (i in 1L:n) {
    params$d0 <- d0[i]
    ur <- uniroot(equation_h_gda, c(1e-20, h0_c), 1e-14, 200, params)
    if (is.na(ur$root) || ur$root <= 0) {
      out$d[i] <- NA
      out$hd[i] <- NA
    } else {
      d_sol <- d0[i] / (1 + Kd * ur$root)
      if (d_sol < 0) {
        out$d[i] <- NA
        out$hd[i] <- NA
      } else {
        out$d[i] <- d_sol
        out$hd[i] <- Kd * ur$root * d_sol
      }
    }
  }
  return(out)
}

# Kga optimized (Kd fixed, taken from add_params); the linear coefficients
# are profiled out per signal via weighted NNLS. See loss_eval_ida (ida.R)
# for why n/X/eps/total_err are declared before the early return.
loss_eval_gda <- function(Kga, add_params) {
  argtypes(
    Kga |> type(double),
    add_params |> type(AddParamsGda)
  )
  returntype(LossEval)
  eval_out |> type(LossEval)
  eval_out$betas <- numeric(3L * add_params$n_sigs)
  eval_out$fitted_signal <- numeric(add_params$n_sigs * length(add_params$d0))

  sol <- solve_h_gda(add_params$kd, Kga, add_params$h0, add_params$d0, add_params$g)

  n <- length(sol$d)
  X |> type(mat(double))
  X <- matrix(numeric(n * 3L), n, 3L)
  eps <- 1e-12
  total_err <- 0

  if (is.na(sol$d[1L])) {
    eval_out$total_err <- 1.797693e+308
    return(eval_out)
  }

  for (i in 1L:n) {
    X[i, 1L] <- 1
    X[i, 2L] <- sol$hd[i]
    X[i, 3L] <- sol$d[i]
  }

  for (s in 1L:add_params$n_sigs) {
    Xw |> type(mat(double))
    Xw <- matrix(numeric(n * 3L), n, 3L)
    yw <- numeric(n)
    for (i in 1L:n) {
      y_i <- add_params$signal[(s - 1L) * n + i]
      ay_i <- abs(y_i)
      w_i <- 0
      if (ay_i > eps) {
        w_i <- 1 / ay_i
      } else {
        w_i <- 1 / eps
      }
      Xw[i, 1L] <- X[i, 1L] * w_i
      Xw[i, 2L] <- X[i, 2L] * w_i
      Xw[i, 3L] <- X[i, 3L] * w_i
      yw[i] <- y_i * w_i
    }
    beta <- nnls(Xw, yw)
    pred <- c(X %*% beta)

    beta_base <- (s - 1L) * 3L
    eval_out$betas[beta_base + 1L] <- beta[1L]
    eval_out$betas[beta_base + 2L] <- beta[2L]
    eval_out$betas[beta_base + 3L] <- beta[3L]

    sig_err <- 0
    for (i in 1L:n) {
      obs <- add_params$signal[(s - 1L) * n + i]
      eval_out$fitted_signal[(s - 1L) * n + i] <- pred[i]
      aobs <- abs(obs)
      denom <- eps
      if (aobs > eps) {
        denom <- aobs
      }
      sig_err <- sig_err + abs(obs - pred[i]) / denom
    }
    total_err <- total_err + sig_err
  }
  eval_out$total_err <- total_err / add_params$n_sigs
  return(eval_out)
}

# parameter: [Kga] only -- R's stats::optimize() calling convention.
loss_fct_gda_a2a <- function(parameter, add_params) {
  argtypes(
    parameter |> type(vec(double)),
    add_params |> type(AddParamsGda)
  )
  returntype(double)
  Kga <- parameter[1L]
  eval_out <- loss_eval_gda(Kga, add_params)
  return(eval_out$total_err)
}

grid_search_gda_a2a <- function(lowerBounds, upperBounds, nGrid, add_params) {
  argtypes(
    lowerBounds |> type(double),
    upperBounds |> type(double),
    nGrid |> type(int),
    add_params |> type(AddParamsGda)
  )
  returntype(GridResult)
  log_lower <- log(lowerBounds)
  log_upper <- log(upperBounds)
  step <- (log_upper - log_lower) / (nGrid - 1L)

  best_param <- lowerBounds
  best_eval <- loss_eval_gda(lowerBounds, add_params)

  for (i in 1L:nGrid) {
    candidate <- exp(log_lower + (i - 1L) * step)
    cand_eval <- loss_eval_gda(candidate, add_params)
    if (cand_eval$total_err < best_eval$total_err) {
      best_eval$total_err <- cand_eval$total_err
      best_eval$betas <- cand_eval$betas
      best_eval$fitted_signal <- cand_eval$fitted_signal
      best_param <- candidate
    }
  }

  res |> type(GridResult)
  res$param_hat <- best_param
  res$loss_hat <- best_eval$total_err
  res$betas <- best_eval$betas
  res$fitted_signal <- best_eval$fitted_signal
  return(res)
}

# PSO: all parameters searched, parameter = [Kg, (I0, IHD, ID) per signal].
# error_code: 1 rel. error, 2 RMSE, 3 SSE, 4 Huber (see pso_error_code()).
loss_fct_pso_gda_a2a <- function(parameter, add_params, error_code) {
  argtypes(
    parameter |> type(vec(double)),
    add_params |> type(AddParamsGda),
    error_code |> type(int)
  )
  returntype(double)
  n <- length(add_params$d0)
  eps <- 1e-12
  huber_delta <- 0.5

  d |> type(vec(double))
  hd |> type(vec(double))
  d <- numeric(n)
  hd <- numeric(n)

  h0_c <- add_params$h0
  if (h0_c <= 0) {
    h0_c <- 1e-10
  }

  Kg <- parameter[1L]
  params |> type(EquationParamsGda)
  params$Kd <- add_params$kd
  params$h0 <- h0_c
  params$g <- add_params$g
  params$Kg <- Kg
  valid <- TRUE

  for (i in 1L:n) {
    params$d0 <- add_params$d0[i]
    ur <- uniroot(equation_h_gda, c(1e-20, h0_c), 1e-14, 200, params)
    if (is.na(ur$root) || ur$root <= 0) {
      valid <- FALSE
    } else {
      d_sol <- add_params$d0[i] / (1 + add_params$kd * ur$root)
      if (d_sol < 0) {
        valid <- FALSE
      } else {
        d[i] <- d_sol
        hd[i] <- add_params$kd * ur$root * d_sol
      }
    }
  }

  if (!valid) {
    return(1.797693e+308)
  }

  total_err <- 0
  for (s in 1L:add_params$n_sigs) {
    base <- 1L + (s - 1L) * 3L
    I0 <- parameter[base + 1L]
    IHD <- parameter[base + 2L]
    ID <- parameter[base + 3L]

    sig_err <- 0
    pred |> type(double)
    obs |> type(double)
    r |> type(double)
    denom |> type(double)
    sq_sum |> type(double)
    hub_sum |> type(double)
    if (error_code == 1L) {
      for (i in 1L:n) {
        pred <- I0 + IHD * hd[i] + ID * d[i]
        obs <- add_params$signal[(s - 1L) * n + i]
        denom <- eps
        if (abs(obs) > eps) {
          denom <- abs(obs)
        }
        sig_err <- sig_err + abs(obs - pred) / denom
      }
    } else if (error_code == 2L) {
      sq_sum <- 0
      for (i in 1L:n) {
        pred <- I0 + IHD * hd[i] + ID * d[i]
        obs <- add_params$signal[(s - 1L) * n + i]
        r <- obs - pred
        sq_sum <- sq_sum + r * r
      }
      sig_err <- sqrt(sq_sum / n)
    } else if (error_code == 3L) {
      for (i in 1L:n) {
        pred <- I0 + IHD * hd[i] + ID * d[i]
        obs <- add_params$signal[(s - 1L) * n + i]
        r <- obs - pred
        sig_err <- sig_err + r * r
      }
    } else {
      hub_sum <- 0
      for (i in 1L:n) {
        pred <- I0 + IHD * hd[i] + ID * d[i]
        obs <- add_params$signal[(s - 1L) * n + i]
        r <- abs(obs - pred)
        if (r <= huber_delta) {
          hub_sum <- hub_sum + 0.5 * r * r
        } else {
          hub_sum <- hub_sum + huber_delta * (r - 0.5 * huber_delta)
        }
      }
      sig_err <- hub_sum / n
    }
    total_err <- total_err + sig_err
  }
  return(total_err / add_params$n_sigs)
}
