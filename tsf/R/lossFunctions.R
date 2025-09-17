# TODO: replace HG with DBA_CONST_HOST and DBA with DBA_CONST_DYE

lossFctHG2 <- function(parameter, env, eval = FALSE) {
  # TODO: add checks for root
  solve_system <- function(h0,g0,kg,kgg) {
    gFct <- function(g) {
      kgg*kg*g^3 +
        g^2*(kg*(1 + 2*kgg*h0 - kgg*g0)) +
        g*(1 + kg*h0 - kg*g0) -
        g0
    }
    g_candidates <- uniroot.all(gFct, c(0, g0))
    sols <- lapply(g_candidates, function(g){
      D <- 1 + kg*g + kg*kgg*g^2
      h  <- h0 / D
      hg <- kg*h*g
      hgg<- kgg*hg*g
      list(g = g,h = h,hg = hg,hgg = hgg)
    })
    return(sols[[1]])
  }

  kg <- parameter[1]
  kgg <- parameter[2]
  I0 <- parameter[3]
  IH <- parameter[4]
  IG <- parameter[5]
  IHG <- parameter[6]
  IHGG <- parameter[7]

  h0 <- env$h0
  guest <- env$guest
  signal <- env$signal
  signal <- ifelse(signal == 0, 10^-15, signal)
  guest <- ifelse(guest == 0, 10^-15, guest)
  h <- numeric(length(guest))
  g <- numeric(length(guest))
  hg <- numeric(length(guest))
  hgg <- numeric(length(guest))

  for (i in seq_along(guest)) {
    g0 <- guest[i]
    res <- try(solve_system(h0,g0,kg,kgg), silent = TRUE)
    if (inherits(res, "try-error")) {
      next
    }
    h[i] <- res$h
    g[i] <- res$g
    hg[i] <- res$hg
    hgg[i] <- res$hgg
  }
  signalInsilico <- I0 + IH*h + IG*g + IHG*hg + IHGG*hgg
  if (eval) {
    return(data.frame(insilico = signalInsilico, g = g, hg = hg))
  }
  return(sum(abs(signal - signalInsilico) / signal))
}

# TODO: finish. But this could reduce the parameter number drastically
lossFctHG2WithQR <- function(parameter, env, eval = FALSE) {
  # TODO: add checks for root
  solve_system <- function(h0,g0,kg,kgg) {
    gFct <- function(g) {
      kgg*kg*g^3 +
        g^2*(kg*(1 + 2*kgg*h0 - kgg*g0)) +
        g*(1 + kg*h0 - kg*g0) -
        g0
    }
    g_candidates <- uniroot.all(gFct, c(0, g0))
    sols <- lapply(g_candidates, function(g){
      D <- 1 + kg*g + kg*kgg*g^2
      h  <- h0 / D
      hg <- kg*h*g
      hgg<- kgg*hg*g
      list(g = g,h = h,hg = hg,hgg = hgg)
    })
    return(sols[[1]])
  }

  kg <- parameter[1]
  kgg <- parameter[2]
  I0 <- parameter[3]
  IH <- parameter[4]
  IG <- parameter[5]
  IHG <- parameter[6]
  IHGG <- parameter[7]

  h0 <- env$h0
  guest <- env$guest
  signal <- env$signal
  signal <- ifelse(signal == 0, 10^-15, signal)
  guest <- ifelse(guest == 0, 10^-15, guest)
  h <- numeric(length(guest))
  g <- numeric(length(guest))
  hg <- numeric(length(guest))
  hgg <- numeric(length(guest))

  for (i in seq_along(guest)) {
    g0 <- guest[i]
    res <- try(solve_system(h0,g0,kg,kgg), silent = TRUE)
    if (inherits(res, "try-error")) {
      next
    }
    h[i] <- res$h
    g[i] <- res$g
    hg[i] <- res$hg
    hgg[i] <- res$hgg
  }

  X <- cbind(1, h, g, hg, hgg)
  # X*beta = signal
  beta <- qr.solve(X, signal)
  # I0 <- beta[1]
  # IH <- beta[2]
  # IG <- beta[3]
  # IHG <- beta[4]
  # IHGG <- beta[5]
  # signalInsilico <- I0 + IH*h + IG*g + IHG*hg + IHGG*hgg
  signalInsilico <- as.vector(X %*% beta)
  if (eval) {
    return(data.frame(insilico = signalInsilico, g = g, hg = hg))
  }
  return(sum(abs(signal - signalInsilico) / signal))
}


# DBA is case DBA with const dye and increasing host
lossFctDBA <- function(parameter, env, eval = FALSE) {
  hdFct <- function(hd) {
    # from dba.mac
    ((hd^2 + (-h0 - d0) * hd + d0 * h0) * kd - hd)
  }
  dFct <- function(d) {
    # from dba.mac
    ((d * h0 - d * d0 + d^2) * kd - d0 + d)
  }
  kd <- parameter[1]
  I0 <- parameter[2]
  IHD <- parameter[3]
  ID <- parameter[4]

  d0 <- env$d0
  host <- env$host
  signal <- env$signal
  host <- ifelse(host == 0, 10^-15, host)
  d <- numeric(length(host))
  hd <- numeric(length(host))

  for (i in seq_along(host)) {
    h0 <- host[i]
    hdRoot <- uniroot.all(hdFct, c(0, h0), tol = .Machine$double.eps^15, maxiter = 10000, n = 1000)
    if (length(hdRoot) == 0) {
      return(.Machine$double.xmax)
    }
    # TODO: add check that hd + d = h0 or d0 dependent on case
    if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)] # TODO: check if this is correct
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
  signalInsilico <- I0 + IHD * hd + ID * d
  if (eval) {
    return(data.frame(insilico = signalInsilico, d = d, hd = hd))
  }
  return(sum(abs(signal - signalInsilico) / signal))
}



# HG is DBA with increasing dye
lossFctHG <- function(parameter, env, eval = FALSE) {
  hdFct <- function(hd) {
    # from dba.mac
    ((hd^2 + (-h0 - d0) * hd + d0 * h0) * kd - hd)
  }
  dFct <- function(d) {
    # from dba.mac
    ((d * h0 - d * d0 + d^2) * kd - d0 + d)
  }
  kd <- parameter[1]
  I0 <- parameter[2]
  IHD <- parameter[3]
  ID <- parameter[4]

  h0 <- env$h0
  dye <- env$dye
  signal <- env$signal
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
  signalInsilico <- I0 + IHD * hd + ID * d
  if (eval) {
    return(data.frame(insilico = signalInsilico, d = d, hd = hd))
  }
  return(sum(abs(signal - signalInsilico) / signal))
}

lossFctIDA <- function(parameter, env, eval = FALSE) {
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
  kga <- parameter[1]
  I0 <- parameter[2]
  IHD <- parameter[3]
  ID <- parameter[4]
  h0 <- env$h0
  d0 <- env$d0
  ga <- env$ga
  signal <- env$signal
  ga <- ifelse(ga == 0, 10^-15, ga)
  d <- numeric(length(ga))
  hd <- numeric(length(ga))
  kd <- env$kd

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

  signalInsilico <- I0 + IHD * hd + ID * d
  if (eval) {
    return(data.frame(insilico = signalInsilico, d = d, hd = hd))
  }
  return(sum(abs(signal - signalInsilico) / signal))
}

lossFctGDA <- function(parameter, env, eval = FALSE) {
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
  kga <- parameter[1]
  I0 <- parameter[2]
  IHD <- parameter[3]
  ID <- parameter[4]
  h0 <- env$h0
  ga0 <- env$ga0
  dye <- env$dye
  signal <- env$signal
  dye <- ifelse(dye == 0, 10^-15, dye)
  d <- numeric(length(dye))
  hd <- numeric(length(dye))
  kd <- env$kd
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
  signalInsilico <- I0 + IHD * hd + ID * d
  if (eval) {
    return(data.frame(insilico = signalInsilico, d = d, hd = hd))
  }
  return(sum(abs(signal - signalInsilico) / signal))
}
