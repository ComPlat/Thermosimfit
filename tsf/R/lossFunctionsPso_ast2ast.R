loss_fct_pso_dba_a2a <- function(parameter, add_params, error_code) {
  equation_h_dba <- fn(
    f_args = function(h, params) {
      h |> type(double)
      params |> type(EquationParamsDba)
    },
    return_value = type(double),
    block = function(h, params) {
      if (h <= 0) {
        return(1.797693e+308)
      }
      denom_Kd <- 1 + params$Kd * h
      h_d <- (params$Kd * h * params$d0) / denom_Kd
      return(h + h_d - params$h0)
    }
  )

  n <- length(add_params$h0)
  eps <- 1e-12
  huber_delta <- 0.5

  d |> type(vec(double))
  hd |> type(vec(double))
  d <- numeric(n)
  hd <- numeric(n)

  Kd <- parameter[1L]
  params |> type(EquationParamsDba)
  params$Kd <- Kd
  valid <- TRUE

  for (i in 1L:n) {
    h0_i <- add_params$h0[i]
    if (h0_i <= 0) {
      h0_i <- 1e-10
    }
    d0_i <- add_params$d0[i]
    params$h0 <- h0_i
    params$d0 <- d0_i
    ur <- uniroot(equation_h_dba, c(1e-22, h0_i), 1e-14, 200, params)
    if (is.na(ur$root) || ur$root <= 0) {
      valid <- FALSE
    } else {
      d_sol <- d0_i / (1 + Kd * ur$root)
      if (d_sol < 0) {
        valid <- FALSE
      } else {
        d[i] <- d_sol
        hd[i] <- Kd * ur$root * d_sol
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
      all_preds <- I0 + IHD * hd + ID * d;
      # print(all_preds)
      # print(hd)
      # print(d)
      # print("==============================")
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

args_f_pso_dba <- function(parameter, add_params, error_code) {
  parameter |> type(vec(double))
  add_params |> type(AddParamsDba)
  error_code |> type(int)
}

loss_fct_pso_ida_a2a <- function(parameter, add_params, error_code) {
  equation_h_ida <- fn(
    f_args = function(h, params) {
      h |> type(double)
      params |> type(EquationParamsIda)
    },
    return_value = type(double),
    block = function(h, params) {
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

  n <- length(add_params$ga)
  eps <- 1e-12
  huber_delta <- 0.5

  d |> type(vec(double))
  hd |> type(vec(double))
  d <- numeric(n)
  hd <- numeric(n)

  Kg <- parameter[1L]
  params |> type(EquationParamsIda)
  params$Kd <- add_params$kd
  params$h0 <- add_params$h0
  params$d0 <- add_params$d0
  params$Kg <- Kg
  valid <- TRUE

  for (i in 1L:n) {
    params$g <- add_params$ga[i]
    ur <- uniroot(equation_h_ida, c(1e-20, add_params$h0), 1e-14, 200, params)
    if (is.na(ur$root) || ur$root <= 0) {
      valid <- FALSE
    } else {
      d_sol <- add_params$d0 / (1 + add_params$kd * ur$root)
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

args_f_pso_ida <- function(parameter, add_params, error_code) {
  parameter |> type(vec(double))
  add_params |> type(AddParamsIda)
  error_code |> type(int)
}

loss_fct_pso_gda_a2a <- function(parameter, add_params, error_code) {
  equation_h_gda <- fn(
    f_args = function(h, params) {
      h |> type(double)
      params |> type(EquationParamsGda)
    },
    return_value = type(double),
    block = function(h, params) {
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

args_f_pso_gda <- function(parameter, add_params, error_code) {
  parameter |> type(vec(double))
  add_params |> type(AddParamsGda)
  error_code |> type(int)
}
