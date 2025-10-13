# DBA is case DBA with const dye and increasing host
lossFctDBA <- function(parameter, env, eval = FALSE) {

  calc_hd_and_d <- function(kd, d0, host) {
    hdFct <- function(hd) {
      # from dba.mac
      ((hd^2 + (-h0 - d0) * hd + d0 * h0) * kd - hd)
    }
    dFct <- function(d) {
      # from dba.mac
      ((d * h0 - d * d0 + d^2) * kd - d0 + d)
    }
    host <- ifelse(host == 0, 10^-15, host)
    d <- numeric(length(host))
    hd <- numeric(length(host))
    for (i in seq_along(host)) {
      h0 <- host[i]
      hdRoot <- uniroot.all(hdFct, c(0, h0),
        tol = .Machine$double.eps^15, maxiter = 10000, n = 1000)
      if (length(hdRoot) == 0) {
        return(.Machine$double.xmax)
      }
      if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)]
      if (hdRoot > h0) {
        hdRoot <- h0
      } else if (hdRoot > d0) {
        hdRoot <- d0
      }
      dRoot <- uniroot.all(dFct, c(0, d0),
        tol = .Machine$double.eps^15,
        maxiter = 10000, n = 1000
      )
      if (length(dRoot) == 0) {
        return(.Machine$double.xmax)
      }
      if (length(dRoot) > 1) dRoot <- dRoot[length(dRoot)]
      if (dRoot > d0) dRoot <- d0
      d[i] <- dRoot
      hd[i] <- hdRoot
    }
    return(list(d = d, hd = hd))
  }

  d_hd <- calc_hd_and_d(parameter[1], env$d0, env$host)

  if (env$n_sigs > 1) {
    params <- matrix(parameter[-1], nrow = 3L)
    insilico_signals <- lapply(1:ncol(env$signal), function(x) {
      ps <- params[, x]
      ps[[1]] + ps[[2]] * d_hd$hd + ps[[3]] * d_hd$d
    })
  } else {
    insilico_signals <- list(
      parameter[[2]] + parameter[[3]]*d_hd$hd + parameter[[4]]*d_hd$d
    )
  }

  if (eval) {
    insilico_signals <- Reduce(cbind, insilico_signals)
    return(data.frame(insilico_signals, d = d_hd$d, hd = d_hd$hd))
  }
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))

}

# HG is DBA with increasing dye
lossFctHG <- function(parameter, env, eval = FALSE) {

  calc_hd_and_d <- function(kd, h0, dye) {
    hdFct <- function(hd) {
      # from dba.mac
      ((hd^2 + (-h0 - d0) * hd + d0 * h0) * kd - hd)
    }
    dFct <- function(d) {
      # from dba.mac
      ((d * h0 - d * d0 + d^2) * kd - d0 + d)
    }
    dye <- ifelse(dye == 0, 10^-15, dye)
    d <- numeric(length(dye))
    hd <- numeric(length(dye))
    for (i in seq_along(dye)) {
      d0 <- dye[i]
      max <- d0
      hdRoot <- uniroot.all(hdFct, c(0, d0), tol = .Machine$double.eps^15, maxiter = 10000, n = 1000)
      if (length(hdRoot) == 0) {
        return(.Machine$double.xmax)
      }
      if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)]
      if (hdRoot > h0) {
        hdRoot <- h0
      } else if (hdRoot > d0) {
        hdRoot <- d0
      }
      dRoot <- uniroot.all(dFct, c(0, max),
        tol = .Machine$double.eps^15,
        maxiter = 10000, n = 1000
      )
      if (length(dRoot) == 0) {
        return(.Machine$double.xmax)
      }
      if (length(dRoot) > 1) dRoot <- dRoot[length(dRoot)]
      if (dRoot > d0) dRoot <- d0
      d[i] <- dRoot
      hd[i] <- hdRoot
    }
    return(list(d = d, hd = hd))
  }
  d_hd <- calc_hd_and_d(parameter[1], env$h0, env$dye)

  if (env$n_sigs > 1) {
    params <- matrix(parameter[-1], nrow = 3L)
    insilico_signals <- lapply(1:ncol(env$signal), function(x) {
      ps <- params[, x]
      ps[[1]] + ps[[2]] * d_hd$hd + ps[[3]] * d_hd$d
    })
  } else {
    insilico_signals <- list(
      parameter[[2]] + parameter[[3]]*d_hd$hd + parameter[[4]]*d_hd$d
    )
  }

  if (eval) {
    insilico_signals <- Reduce(cbind, insilico_signals)
    return(data.frame(insilico_signals, d = d_hd$d, hd = d_hd$hd))
  }
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))

}

lossFctIDA <- function(parameter, env, eval = FALSE) {
  calc_hd_and_d <- function(kga, kd, h0, d0, ga) {
    hdFct <- function(hd) {
      # from ida_gda_eliminate_hd.mac
      (-(((
        hd^3 + (-h0 + ga0 - d0) * hd^2 + (d0 * h0 - d0 * ga0) * hd
      ) * kd - hd^2) * kga)
        - (-hd^3 + (h0 + 2 * d0) * hd^2 + (-(2 * d0 * h0) - d0^2) * hd +
          d0^2 * h0) * kd^2 - (hd^2 - d0 * hd) * kd)
    }
    dFct <- function(d) {
      # from ida_gda_eliminate_d.mac
      ((d * kd + 1) * ((((d * d0 - d^2) * h0 + d0 * (2 * d^2 - d * ga0) + d^
        2 * ga0 - d * d0^2 - d^3
      ) * kd
        - d0^2 + 2 * d * d0 - d^2)
        * kga
        + (d^2 * h0 - d^2 * d0 + d^3) * kd^2 + (d^2 - d *
          d0) * kd
      ))
    }
    ga <- ifelse(ga == 0, 10^-15, ga)
    d <- numeric(length(ga))
    hd <- numeric(length(ga))
    for (i in seq_along(ga)) {
      ga0 <- ga[i]
      max <- d0
      hdRoot <- uniroot.all(hdFct, c(0, max),
        tol = .Machine$double.eps^15,
        maxiter = 10000, n = 1000
      )
      # if (length(hdRoot) == 0) return(.Machine$double.xmax)
      if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)]
      if (hdRoot > h0) {
        hdRoot <- h0
      } else if (hdRoot > d0) {
        hdRoot <- d0
      }
      dRoot <- uniroot.all(dFct, c(0, max),
        tol = .Machine$double.eps^15,
        maxiter = 10000, n = 1000
      )
      # if (length(dRoot) == 0) return(.Machine$double.xmax)
      if (length(dRoot) > 1) dRoot <- dRoot[length(dRoot)]
      if (dRoot > d0) dRoot <- d0
      d[i] <- dRoot
      hd[i] <- hdRoot
    }
    return(list(d = d, hd = hd))
  }

  d_hd <- calc_hd_and_d(parameter[1], env$kd, env$h0, env$d0, env$ga)

  if (env$n_sigs > 1) {
    params <- matrix(parameter[-1], nrow = 3L)
    insilico_signals <- lapply(1:ncol(env$signal), function(x) {
      ps <- params[, x]
      ps[[1]] + ps[[2]] * d_hd$hd + ps[[3]] * d_hd$d
    })
  } else {
    insilico_signals <- list(
      parameter[[2]] + parameter[[3]]*d_hd$hd + parameter[[4]]*d_hd$d
    )
  }

  if (eval) {
    insilico_signals <- Reduce(cbind, insilico_signals)
    return(data.frame(insilico_signals, d = d_hd$d, hd = d_hd$hd))
  }
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))

}

# lossFctIDA <- function(parameter, env, eval = FALSE) {
#   # Non linear part: Ka optimized
#   # =======================================================================
#   calc_hd_and_d <- function(kga, kd, h0, d0, ga) {
#     hdFct <- function(hd) {
#       # from ida_gda_eliminate_hd.mac
#       (-(((
#         hd^3 + (-h0 + ga0 - d0) * hd^2 + (d0 * h0 - d0 * ga0) * hd
#       ) * kd - hd^2) * kga)
#         - (-hd^3 + (h0 + 2 * d0) * hd^2 + (-(2 * d0 * h0) - d0^2) * hd +
#           d0^2 * h0) * kd^2 - (hd^2 - d0 * hd) * kd)
#     }
#     dFct <- function(d) {
#       # from ida_gda_eliminate_d.mac
#       ((d * kd + 1) * ((((d * d0 - d^2) * h0 + d0 * (2 * d^2 - d * ga0) + d^
#         2 * ga0 - d * d0^2 - d^3
#       ) * kd
#         - d0^2 + 2 * d * d0 - d^2)
#         * kga
#         + (d^2 * h0 - d^2 * d0 + d^3) * kd^2 + (d^2 - d *
#           d0) * kd
#       ))
#     }
#     ga <- ifelse(ga == 0, 10^-15, ga)
#     d <- numeric(length(ga))
#     hd <- numeric(length(ga))
#     for (i in seq_along(ga)) {
#       ga0 <- ga[i]
#       max <- d0
#       hdRoot <- uniroot.all(hdFct, c(0, max),
#         tol = .Machine$double.eps^15,
#         maxiter = 10000, n = 1000
#       )
#       if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)]
#       if (hdRoot > h0) {
#         hdRoot <- h0
#       } else if (hdRoot > d0) {
#         hdRoot <- d0
#       }
#       dRoot <- uniroot.all(dFct, c(0, max),
#         tol = .Machine$double.eps^15,
#         maxiter = 10000, n = 1000
#       )
#       if (length(dRoot) > 1) dRoot <- dRoot[length(dRoot)]
#       if (dRoot > d0) dRoot <- d0
#       d[i] <- dRoot
#       hd[i] <- hdRoot
#     }
#     list(d = d, hd = hd)
#   }
#   d_hd <- calc_hd_and_d(parameter, env$kd, env$h0, env$d0, env$ga)
#
#   # Linear part: determine the optimal I parameters
#   # =======================================================================
#   X <- cbind(Intercept = 1, hd = d_hd$hd, d = d_hd$d)
#   betas <- list()
#   fit_one <- function(y) {
#     eps <- 1e-12
#     w   <- 1 / pmax(abs(y), eps)
#     Xw <- X * w
#     yw <- y * w
#     fit <- nnls(Xw, yw) # Non-Negative Least Squares
#     beta <- coef(fit) # all >= 0
#     betas <<- c(betas, beta)
#     as.vector(X %*% beta)
#   }
#   if (!is.data.frame(env$signal)) env$signal <- as.data.frame(env$signal)
#   # Run non negative least square for each signal independently to determine Is
#   insilico_signals <- lapply(1:ncol(env$signal), function(x) {
#     fit_one(env$signal[[x]])
#   })
#
#   if (eval) {
#     insilico_mat <- Reduce(cbind, insilico_signals)
#     return(list(
#       data.frame(insilico_mat, d = d_hd$d, hd = d_hd$hd),
#       do.call(c, betas)
#     ))
#   }
#   # Calc mean error across all signals
#   # =======================================================================
#   return(mean(
#     Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
#     na.rm = TRUE
#   ))
# }

lossFctGDA <- function(parameter, env, eval = FALSE) {
  calc_hd_and_d <- function(kga, kd, h0, ga0, dye) {
    hdFct <- function(hd) {
      # from ida_gda_eliminate_hd.mac
      (-(((
        hd^3 + (-h0 + ga0 - d0) * hd^2 + (d0 * h0 - d0 * ga0) * hd
      ) * kd - hd^2) * kga)
        - (-hd^3 + (h0 + 2 * d0) * hd^2 + (-(2 * d0 * h0) - d0^2) * hd +
          d0^2 * h0) * kd^2 - (hd^2 - d0 * hd) * kd)
    }
    dFct <- function(d) {
      # from ida_gda_eliminate_d.mac
      ((d * kd + 1) * ((((d * d0 - d^2) * h0 + d0 * (2 * d^2 - d * ga0) + d^
        2 * ga0 - d * d0^2 - d^3
      ) * kd
        - d0^2 + 2 * d * d0 - d^2)
        * kga
        + (d^2 * h0 - d^2 * d0 + d^3) * kd^2 + (d^2 - d *
          d0) * kd
      ))
    }
    dye <- ifelse(dye == 0, 10^-15, dye)
    d <- numeric(length(dye))
    hd <- numeric(length(dye))
    for (i in seq_along(dye)) {
      d0 <- dye[i]
      max <- d0
      hdRoot <- uniroot.all(hdFct, c(0, max),
        tol = .Machine$double.eps^15,
        maxiter = 10000, n = 1000
      )
      if (length(hdRoot) == 0) {
        return(.Machine$double.xmax)
      }
      if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)]
      if (hdRoot > h0) {
        hdRoot <- h0
      } else if (hdRoot > d0) {
        hdRoot <- d0
      }
      dRoot <- uniroot.all(dFct, c(0, max),
        tol = .Machine$double.eps^15,
        maxiter = 10000, n = 1000
      )
      if (length(dRoot) == 0) {
        return(.Machine$double.xmax)
      }
      if (length(dRoot) > 1) dRoot <- dRoot[length(dRoot)]
      if (dRoot > d0) dRoot <- d0
      d[i] <- dRoot
      hd[i] <- hdRoot
    }
    return(list(d = d, hd = hd))
  }

  d_hd <- calc_hd_and_d(parameter[1], env$kd, env$h0, env$ga0, env$dye)

  if (env$n_sigs > 1) {
    params <- matrix(parameter[-1], nrow = 3L)
    insilico_signals <- lapply(1:ncol(env$signal), function(x) {
      ps <- params[, x]
      ps[[1]] + ps[[2]] * d_hd$hd + ps[[3]] * d_hd$d
    })
  } else {
    insilico_signals <- list(
      parameter[[2]] + parameter[[3]]*d_hd$hd + parameter[[4]]*d_hd$d
    )
  }

  if (eval) {
    insilico_signals <- Reduce(cbind, insilico_signals)
    return(data.frame(insilico_signals, d = d_hd$d, hd = d_hd$hd))
  }
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))

}
