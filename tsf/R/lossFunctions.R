# DBA is case DBA with const dye and increasing host
lossFctDBA <- function(parameter, env, eval = FALSE) {
  sol <- solve_h_dba(parameter[1], env$host, env$d0)
  if (anyNA(sol$d)) {
    return(.Machine$double.xmax)
  }

  if (env$n_sigs > 1) {
    params <- matrix(parameter[-1], nrow = 3L)
    insilico_signals <- lapply(1:ncol(env$signal), function(x) {
      ps <- params[, x]
      ps[[1]] + ps[[2]] * sol$hd + ps[[3]] * sol$d
    })
  } else {
    insilico_signals <- list(
      parameter[[2]] + parameter[[3]] * sol$hd + parameter[[4]] * sol$d
    )
  }

  if (eval) {
    insilico_signals <- Reduce(cbind, insilico_signals)
    return(data.frame(insilico_signals, d = sol$d, hd = sol$hd))
  }
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}

# HG is DBA with increasing dye
lossFctHG <- function(parameter, env, eval = FALSE) {
  sol <- solve_h_dba(parameter[1], env$h0, env$dye)
  if (anyNA(sol$d)) {
    return(.Machine$double.xmax)
  }

  if (env$n_sigs > 1) {
    params <- matrix(parameter[-1], nrow = 3L)
    insilico_signals <- lapply(1:ncol(env$signal), function(x) {
      ps <- params[, x]
      ps[[1]] + ps[[2]] * sol$hd + ps[[3]] * sol$d
    })
  } else {
    insilico_signals <- list(
      parameter[[2]] + parameter[[3]] * sol$hd + parameter[[4]] * sol$d
    )
  }

  if (eval) {
    insilico_signals <- Reduce(cbind, insilico_signals)
    return(data.frame(insilico_signals, d = sol$d, hd = sol$hd))
  }
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}

lossFctIDA <- function(parameter, env, eval = FALSE) {
  sol <- solve_h_ida_gda(env$kd, parameter[1], env$h0, env$d0, env$ga)
  if (anyNA(sol$d)) {
    return(.Machine$double.xmax)
  }

  if (env$n_sigs > 1) {
    params <- matrix(parameter[-1], nrow = 3L)
    insilico_signals <- lapply(1:ncol(env$signal), function(x) {
      ps <- params[, x]
      ps[[1]] + ps[[2]] * sol$hd + ps[[3]] * sol$d
    })
  } else {
    insilico_signals <- list(
      parameter[[2]] + parameter[[3]] * sol$hd + parameter[[4]] * sol$d
    )
  }

  if (eval) {
    insilico_signals <- Reduce(cbind, insilico_signals)
    return(data.frame(insilico_signals, d = sol$d, hd = sol$hd))
  }
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}

lossFctGDA <- function(parameter, env, eval = FALSE) {
  sol <- solve_h_ida_gda(env$kd, parameter[1], env$h0, env$dye, env$ga0)
  if (anyNA(sol$d)) {
    return(.Machine$double.xmax)
  }

  if (env$n_sigs > 1) {
    params <- matrix(parameter[-1], nrow = 3L)
    insilico_signals <- lapply(1:ncol(env$signal), function(x) {
      ps <- params[, x]
      ps[[1]] + ps[[2]] * sol$hd + ps[[3]] * sol$d
    })
  } else {
    insilico_signals <- list(
      parameter[[2]] + parameter[[3]] * sol$hd + parameter[[4]] * sol$d
    )
  }

  if (eval) {
    insilico_signals <- Reduce(cbind, insilico_signals)
    return(data.frame(insilico_signals, d = sol$d, hd = sol$hd))
  }
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}
