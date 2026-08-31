types_f_ida <- function() {
  new_type(
    SolveD_HD_Result,
    slots(
      d |> type(vec(double)),
      hd |> type(vec(double))
    )
  )
  new_type(
    EquationParamsIda,
    slots(
      Kd |> type(double),
      Kg |> type(double),
      h0 |> type(double),
      d0 |> type(double),
      g |> type(double)
    )
  )
  new_type(
    AddParamsIda,
    slots(
      kd |> type(double),
      h0 |> type(double),
      d0 |> type(double),
      ga |> type(vec(double)),
      n_sigs |> type(int),
      signal |> type(vec(double))
    )
  )
  new_type(
    LossEval,
    slots(
      total_err |> type(double),
      betas |> type(vec(double)),
      fitted_signal |> type(vec(double))
    )
  )
  new_type(
    GridResult,
    slots(
      param_hat |> type(double),
      loss_hat |> type(double),
      betas |> type(vec(double)),
      fitted_signal |> type(vec(double))
    )
  )
}

# parameter: [Kga] only -- linear coeffs are profiled out via nnls() below.
loss_fct_ida_a2a <- function(parameter, add_params) {
  argtypes(
    parameter |> type(vec(double)),
    add_params |> type(AddParamsIda)
  )
  solve_h_ida <- fn(
    argtypes(
      Kd |> type(double) |> const(),
      Kg |> type(double) |> const(),
      h0 |> type(double) |> const(),
      d0 |> type(double) |> const(),
      g |> type(vec(double)) |> const()
    ),
    return(SolveD_HD_Result),
    {

      # equation_h_ida_gda(h, Kd, Kg, h0, d0, g0), specialized to a single
      # unary-in-h root-find target -- Kd/Kg/h0/d0/g are read from `params`,
      # passed explicitly as uniroot()'s 5th argument, not via closure.
      equation_h_ida <- fn(
        argtypes(
          h |> type(double),
          params |> type(EquationParamsIda)
        ),
        return(double),
        {
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
      )

      n <- length(g)
      out |> type(SolveD_HD_Result)
      out$d <- numeric(n)
      out$hd <- numeric(n)

      params |> type(EquationParamsIda)
      params$Kd <- Kd
      params$Kg <- Kg
      params$h0 <- h0
      params$d0 <- d0

      for (i in 1L:n) {
        params$g <- g[i]
        ur <- uniroot(equation_h_ida, c(1e-20, h0), 1e-14, 200, params)
        if (is.na(ur$root) || ur$root <= 0) {
          out$d[i] <- NA
          out$hd[i] <- NA
        } else {
          d_sol <- d0 / (1 + Kd * ur$root)
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
  )

  # # Non linear part: Kga optimized (Kd fixed, taken from add_params)
  # # =========================================================================
  sol <- solve_h_ida(add_params$kd, parameter[1L], add_params$h0, add_params$d0, add_params$ga)
  if (is.na(sol$d[1L])) {
    return(1.797693e+308)
  }

  # # Linear part: for each signal, profile out (intercept, hd-coef, d-coef)
  # # via weighted non-negative least squares -- X is shared across signals,
  # # only the weights (1/max(|y|,eps)) and RHS change per signal.
  # # =========================================================================
  n <- length(sol$d)
  X |> type(mat(double))
  X <- matrix(numeric(n * 3L), n, 3L)
  for (i in 1L:n) {
    X[i, 1L] <- 1
    X[i, 2L] <- sol$hd[i]
    X[i, 3L] <- sol$d[i]
  }

  eps <- 1e-12
  total_err <- 0
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

    sig_err <- 0
    for (i in 1L:n) {
      obs <- add_params$signal[(s - 1L) * n + i]
      aobs <- abs(obs)
      denom <- eps
      if (aobs > eps) {
        denom <- aobs
      }
      sig_err <- sig_err + abs(obs - pred[i]) / denom
    }
    total_err <- total_err + sig_err
  }
  return(total_err / add_params$n_sigs)
}


# The coarse exponential grid search (vapro_grid_search's grid part, not the
# stats::optimize() local refinement -- that stays in R for now, it's cheap
# since it only calls the loss a handful more times, not nGrid times) run
# entirely in C++: every grid-point evaluation is a call to a nested fn(),
# not a round trip back to R. loss_fn's body is the same as
# loss_fct_ida_a2a's, just taking Kga as a scalar directly instead of
# through a length-1 `parameter` vector (that vector only existed for R's
# own stats::optimize() calling convention).
grid_search_ida_a2a <- function(lowerBounds, upperBounds, nGrid, add_params) {
  argtypes(
    lowerBounds |> type(double),
    upperBounds |> type(double),
    nGrid |> type(int),
    add_params |> type(AddParamsIda)
  )
  loss_fn <- fn(
    argtypes(
      Kga |> type(double),
      add_params |> type(AddParamsIda)
    ),
    return(LossEval),
    {
      eval_out |> type(LossEval)
      eval_out$betas <- numeric(3L * add_params$n_sigs)
      eval_out$fitted_signal <- numeric(add_params$n_sigs * length(add_params$ga))
      solve_h_ida <- fn(
        argtypes(
          Kd |> type(double) |> const(),
          Kg |> type(double) |> const(),
          h0 |> type(double) |> const(),
          d0 |> type(double) |> const(),
          g |> type(vec(double)) |> const()
        ),
        return(SolveD_HD_Result),
        {
          equation_h_ida <- fn(
            argtypes(
              h |> type(double),
              params |> type(EquationParamsIda)
            ),
            return(double),
            {
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
          )

          n <- length(g)
          out |> type(SolveD_HD_Result)
          out$d <- numeric(n)
          out$hd <- numeric(n)

          params |> type(EquationParamsIda)
          params$Kd <- Kd
          params$Kg <- Kg
          params$h0 <- h0
          params$d0 <- d0

          for (i in 1L:n) {
            params$g <- g[i]
            ur <- uniroot(equation_h_ida, c(1e-20, h0), 1e-14, 200, params)
            if (is.na(ur$root) || ur$root <= 0) {
              out$d[i] <- NA
              out$hd[i] <- NA
            } else {
              d_sol <- d0 / (1 + Kd * ur$root)
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
      )

      sol <- solve_h_ida(add_params$kd, Kga, add_params$h0, add_params$d0, add_params$ga)

      # n/X/eps/total_err are declared unconditionally, BEFORE the early
      # return below -- ast2ast currently loses a variable's type if its
      # first assignment in a block comes after an earlier return() in that
      # same block (confirmed minimal repro), so nothing here can be
      # first-assigned only in the "success" path after the check.
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
  )

  log_lower <- log(lowerBounds)
  log_upper <- log(upperBounds)
  step <- (log_upper - log_lower) / (nGrid - 1L)

  best_param <- lowerBounds
  best_eval <- loss_fn(lowerBounds, add_params)

  for (i in 1L:nGrid) {
    candidate <- exp(log_lower + (i - 1L) * step)
    cand_eval <- loss_fn(candidate, add_params)
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


# ===========================================================================
# DBA (dba_dye_const / dba_host_const): single binding equilibrium. Both
# cases are forward-solved by the same equation (see solve_h_dba in
# DerivedForwardEquations.R, which the R implementation already funnels
# lossFctDBAVapro and lossFctHGVapro through) -- only which physical
# quantity (host vs dye) is the varying titration axis differs, and that's
# handled by build_add_params_dba_a2a (OptimizeVapro_ast2ast.R) recycling
# whichever one is fixed to the same length before calling in here. h0 is
# clamped away from 0 per point since it's the uniroot upper bracket bound,
# and dba_dye_const's real data sweeps host down to exactly 0.
# ===========================================================================
types_f_dba <- function() {
  new_type(
    SolveD_HD_Result,
    slots(
      d |> type(vec(double)),
      hd |> type(vec(double))
    )
  )
  new_type(
    EquationParamsDba,
    slots(
      Kd |> type(double),
      h0 |> type(double),
      d0 |> type(double)
    )
  )
  new_type(
    AddParamsDba,
    slots(
      h0 |> type(vec(double)),
      d0 |> type(vec(double)),
      n_sigs |> type(int),
      signal |> type(vec(double))
    )
  )
  new_type(
    LossEval,
    slots(
      total_err |> type(double),
      betas |> type(vec(double)),
      fitted_signal |> type(vec(double))
    )
  )
  new_type(
    GridResult,
    slots(
      param_hat |> type(double),
      loss_hat |> type(double),
      betas |> type(vec(double)),
      fitted_signal |> type(vec(double))
    )
  )
}

# parameter: [Kd] only -- linear coeffs are profiled out via nnls() below.
loss_fct_dba_a2a <- function(parameter, add_params) {
  argtypes(
    parameter |> type(vec(double)),
    add_params |> type(AddParamsDba)
  )
  solve_h_dba_a2a <- fn(
    argtypes(
      Kd |> type(double) |> const(),
      h0 |> type(vec(double)) |> const(),
      d0 |> type(vec(double)) |> const()
    ),
    return(SolveD_HD_Result),
    {
      equation_h_dba <- fn(
        argtypes(
          h |> type(double),
          params |> type(EquationParamsDba)
        ),
        return(double),
        {
          if (h <= 0) {
            return(1.797693e+308)
          }
          denom_Kd <- 1 + params$Kd * h
          h_d <- (params$Kd * h * params$d0) / denom_Kd
          return(h + h_d - params$h0)
        }
      )

      n <- length(h0)
      out |> type(SolveD_HD_Result)
      out$d <- numeric(n)
      out$hd <- numeric(n)

      params |> type(EquationParamsDba)
      params$Kd <- Kd

      for (i in 1L:n) {
        h0_i <- h0[i]
        if (h0_i <= 0) {
          h0_i <- 1e-10
        }
        params$h0 <- h0_i
        params$d0 <- d0[i]
        ur <- uniroot(equation_h_dba, c(1e-20, h0_i), 1e-14, 200, params)
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
  )

  sol <- solve_h_dba_a2a(parameter[1L], add_params$h0, add_params$d0)
  if (is.na(sol$d[1L])) {
    return(1.797693e+308)
  }

  n <- length(sol$d)
  X |> type(mat(double))
  X <- matrix(numeric(n * 3L), n, 3L)
  for (i in 1L:n) {
    X[i, 1L] <- 1
    X[i, 2L] <- sol$hd[i]
    X[i, 3L] <- sol$d[i]
  }

  eps <- 1e-12
  total_err <- 0
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

    sig_err <- 0
    for (i in 1L:n) {
      obs <- add_params$signal[(s - 1L) * n + i]
      aobs <- abs(obs)
      denom <- eps
      if (aobs > eps) {
        denom <- aobs
      }
      sig_err <- sig_err + abs(obs - pred[i]) / denom
    }
    total_err <- total_err + sig_err
  }
  return(total_err / add_params$n_sigs)
}


# Same coarse exponential grid search as grid_search_ida_a2a, specialized to
# the single-Kd DBA/HG equation -- see that function's header comment for why
# the whole loop (not just loss_fct's body) is duplicated in C++ here rather
# than calling loss_fct_dba_a2a from R per grid point.
grid_search_dba_a2a <- function(lowerBounds, upperBounds, nGrid, add_params) {
  argtypes(
    lowerBounds |> type(double),
    upperBounds |> type(double),
    nGrid |> type(int),
    add_params |> type(AddParamsDba)
  )
  loss_fn <- fn(
    argtypes(
      Kd |> type(double),
      add_params |> type(AddParamsDba)
    ),
    return(LossEval),
    {
      eval_out |> type(LossEval)
      eval_out$betas <- numeric(3L * add_params$n_sigs)
      eval_out$fitted_signal <- numeric(add_params$n_sigs * length(add_params$h0))

      solve_h_dba_a2a <- fn(
        argtypes(
          Kd |> type(double) |> const(),
          h0 |> type(vec(double)) |> const(),
          d0 |> type(vec(double)) |> const()
        ),
        return(SolveD_HD_Result),
        {
          equation_h_dba <- fn(
            argtypes(
              h |> type(double),
              params |> type(EquationParamsDba)
            ),
            return(double),
            {
              if (h <= 0) {
                return(1.797693e+308)
              }
              denom_Kd <- 1 + params$Kd * h
              h_d <- (params$Kd * h * params$d0) / denom_Kd
              return(h + h_d - params$h0)
            }
          )

          n <- length(h0)
          out |> type(SolveD_HD_Result)
          out$d <- numeric(n)
          out$hd <- numeric(n)

          params |> type(EquationParamsDba)
          params$Kd <- Kd

          for (i in 1L:n) {
            h0_i <- h0[i]
            if (h0_i <= 0) {
              h0_i <- 1e-10
            }
            params$h0 <- h0_i
            params$d0 <- d0[i]
            ur <- uniroot(equation_h_dba, c(1e-20, h0_i), 1e-14, 200, params)
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
      )

      sol <- solve_h_dba_a2a(Kd, add_params$h0, add_params$d0)

      # n/X/eps/total_err declared unconditionally, before the early return
      # below -- see grid_search_ida_a2a's header comment for why.
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
  )

  log_lower <- log(lowerBounds)
  log_upper <- log(upperBounds)
  step <- (log_upper - log_lower) / (nGrid - 1L)

  best_param <- lowerBounds
  best_eval <- loss_fn(lowerBounds, add_params)

  for (i in 1L:nGrid) {
    candidate <- exp(log_lower + (i - 1L) * step)
    cand_eval <- loss_fn(candidate, add_params)
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


# ===========================================================================
# GDA: two coupled equilibria (host-dye and host-guest), same equation as
# IDA (see solve_h_ida_gda in DerivedForwardEquations.R, shared by both
# lossFctIDAVapro and lossFctGDAVapro in R). The two cases differ only in
# which of dye/guest is the fixed additional parameter vs. the varying
# titration axis: IDA sweeps guest at fixed dye, GDA sweeps dye at fixed
# guest. This block is IDA's mirror image with those two roles swapped --
# d0 is the varying vec(double) here (dye, from the data column) and g is
# the fixed double (ga0, from additionalParameters), whereas in IDA it's the
# other way around.
# ===========================================================================
types_f_gda <- function() {
  new_type(
    SolveD_HD_Result,
    slots(
      d |> type(vec(double)),
      hd |> type(vec(double))
    )
  )
  new_type(
    EquationParamsGda,
    slots(
      Kd |> type(double),
      Kg |> type(double),
      h0 |> type(double),
      d0 |> type(double),
      g |> type(double)
    )
  )
  new_type(
    AddParamsGda,
    slots(
      kd |> type(double),
      h0 |> type(double),
      d0 |> type(vec(double)),
      g |> type(double),
      n_sigs |> type(int),
      signal |> type(vec(double))
    )
  )
  new_type(
    LossEval,
    slots(
      total_err |> type(double),
      betas |> type(vec(double)),
      fitted_signal |> type(vec(double))
    )
  )
  new_type(
    GridResult,
    slots(
      param_hat |> type(double),
      loss_hat |> type(double),
      betas |> type(vec(double)),
      fitted_signal |> type(vec(double))
    )
  )
}

# parameter: [Kga] only -- linear coeffs are profiled out via nnls() below.
loss_fct_gda_a2a <- function(parameter, add_params) {
  argtypes(
    parameter |> type(vec(double)),
    add_params |> type(AddParamsGda)
  )
  solve_h_gda <- fn(
    argtypes(
      Kd |> type(double) |> const(),
      Kg |> type(double) |> const(),
      h0 |> type(double) |> const(),
      d0 |> type(vec(double)) |> const(),
      g |> type(double) |> const()
    ),
    return(SolveD_HD_Result),
    {
      equation_h_gda <- fn(
        argtypes(
          h |> type(double),
          params |> type(EquationParamsGda)
        ),
        return(double),
        {
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
      )

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
  )

  sol <- solve_h_gda(add_params$kd, parameter[1L], add_params$h0, add_params$d0, add_params$g)
  if (is.na(sol$d[1L])) {
    return(1.797693e+308)
  }

  n <- length(sol$d)
  X |> type(mat(double))
  X <- matrix(numeric(n * 3L), n, 3L)
  for (i in 1L:n) {
    X[i, 1L] <- 1
    X[i, 2L] <- sol$hd[i]
    X[i, 3L] <- sol$d[i]
  }

  eps <- 1e-12
  total_err <- 0
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

    sig_err <- 0
    for (i in 1L:n) {
      obs <- add_params$signal[(s - 1L) * n + i]
      aobs <- abs(obs)
      denom <- eps
      if (aobs > eps) {
        denom <- aobs
      }
      sig_err <- sig_err + abs(obs - pred[i]) / denom
    }
    total_err <- total_err + sig_err
  }
  return(total_err / add_params$n_sigs)
}


# Same coarse exponential grid search as grid_search_ida_a2a, specialized to
# the GDA equation (d0 varying, g fixed) -- see that function's header
# comment for why the whole loop is duplicated in C++ here.
grid_search_gda_a2a <- function(lowerBounds, upperBounds, nGrid, add_params) {
  argtypes(
    lowerBounds |> type(double),
    upperBounds |> type(double),
    nGrid |> type(int),
    add_params |> type(AddParamsGda)
  )
  loss_fn <- fn(
    argtypes(
      Kga |> type(double),
      add_params |> type(AddParamsGda)
    ),
    return(LossEval),
    {
      eval_out |> type(LossEval)
      eval_out$betas <- numeric(3L * add_params$n_sigs)
      eval_out$fitted_signal <- numeric(add_params$n_sigs * length(add_params$d0))

      solve_h_gda <- fn(
        argtypes(
          Kd |> type(double) |> const(),
          Kg |> type(double) |> const(),
          h0 |> type(double) |> const(),
          d0 |> type(vec(double)) |> const(),
          g |> type(double) |> const()
        ),
        return(SolveD_HD_Result),
        {
          equation_h_gda <- fn(
            argtypes(
              h |> type(double),
              params |> type(EquationParamsGda)
            ),
            return(double),
            {
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
          )

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
      )

      sol <- solve_h_gda(add_params$kd, Kga, add_params$h0, add_params$d0, add_params$g)

      # n/X/eps/total_err declared unconditionally, before the early return
      # below -- see grid_search_ida_a2a's header comment for why.
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
  )

  log_lower <- log(lowerBounds)
  log_upper <- log(upperBounds)
  step <- (log_upper - log_lower) / (nGrid - 1L)

  best_param <- lowerBounds
  best_eval <- loss_fn(lowerBounds, add_params)

  for (i in 1L:nGrid) {
    candidate <- exp(log_lower + (i - 1L) * step)
    cand_eval <- loss_fn(candidate, add_params)
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

