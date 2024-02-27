lossFctHG <- function(parameter, env, eval = FALSE) {
  hdFct <- function(hd) {
    # from hg_eliminate.mac
    ((hd^2 + (-h0 - d0) * hd + d0 * h0) * kd - hd)
  }
  dFct <- function(d) {
    # from hg_eliminate.mac
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
    dRoot <- uniroot.all(dFct, c(min, max),
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
  return( sum( abs(signal - signalInsilico) / signal ) )
}

lossFctIDA <- function(parameter, env, eval = FALSE) {
  hdFct <- function(hd) {
    # from ida_gda_eliminate_hd.mac
    (-(((
      hd ^ 3 + (-h0 + ga0 - d0) * hd ^ 2 + (d0 * h0 - d0 * ga0) * hd
    ) * kd - hd ^ 2) * kga)
    - (-hd ^ 3 + (h0 + 2 * d0) * hd ^ 2 + (-(2 * d0 * h0) - d0 ^ 2) * hd +
         d0 ^ 2 * h0) * kd ^ 2 - (hd ^ 2 - d0 * hd) * kd)
  }
  dFct <- function(d) {
    # from ida_gda_eliminate_d.mac
    ((d * kd + 1) * ((((d * d0 - d ^ 2) * h0 + d0 * (2 * d ^ 2 - d * ga0) + d ^
                         2 * ga0 - d * d0 ^ 2 - d ^ 3
    ) * kd
    - d0 ^ 2 + 2 * d * d0 - d ^ 2)
    * kga
    + (d ^ 2 * h0 - d ^ 2 * d0 + d ^ 3) * kd ^ 2 + (d ^ 2 - d *
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
    hdRoot <- uniroot.all(hdFct, c(0, max), tol = .Machine$double.eps^15,
                          maxiter = 10000, n = 1000)
    # if (length(hdRoot) == 0) return(.Machine$double.xmax)
    if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)]
    if (hdRoot > h0) {
      hdRoot <- h0
    } else if (hdRoot > d0) {
      hdRoot <- d0
    }
    
    dRoot <- uniroot.all(dFct, c(0, max), tol = .Machine$double.eps^15,
                         maxiter = 10000, n = 1000)
    #if (length(dRoot) == 0) return(.Machine$double.xmax)
    if (length(dRoot) > 1) dRoot <- dRoot[length(dRoot)]
    if (dRoot > d0) dRoot <- d0
    d[i] <- dRoot
    hd[i] <- hdRoot
  }
  
  signalInsilico <- I0 + IHD * hd + ID * d
  if (eval) {
    return(data.frame(insilico = signalInsilico, d = d, hd = hd))
  }
  return( sum( abs(signal - signalInsilico) / signal ) )
}

lossFctGDA <- function(parameter, env, eval = FALSE) {
  hdFct <- function(hd) {
    # from ida_gda_eliminate_hd.mac
    (-(((
      hd ^ 3 + (-h0 + ga0 - d0) * hd ^ 2 + (d0 * h0 - d0 * ga0) * hd
    ) * kd - hd ^ 2) * kga)
    - (-hd ^ 3 + (h0 + 2 * d0) * hd ^ 2 + (-(2 * d0 * h0) - d0 ^ 2) * hd +
         d0 ^ 2 * h0) * kd ^ 2 - (hd ^ 2 - d0 * hd) * kd)
  }
  dFct <- function(d) {
    # from ida_gda_eliminate_d.mac
    ((d * kd + 1) * ((((d * d0 - d ^ 2) * h0 + d0 * (2 * d ^ 2 - d * ga0) + d ^
                         2 * ga0 - d * d0 ^ 2 - d ^ 3
    ) * kd
    - d0 ^ 2 + 2 * d * d0 - d ^ 2)
    * kga
    + (d ^ 2 * h0 - d ^ 2 * d0 + d ^ 3) * kd ^ 2 + (d ^ 2 - d *
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
    hdRoot <- uniroot.all(hdFct, c(0, max), tol = .Machine$double.eps^15,
                          maxiter = 10000, n = 1000)
    if (length(hdRoot) == 0) return(.Machine$double.xmax)
    if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)]
    if (hdRoot > h0) {
      hdRoot <- h0
    } else if (hdRoot > d0) {
      hdRoot <- d0
    }
    dRoot <- uniroot.all(dFct, c(min, max), tol = .Machine$double.eps^15,
                         maxiter = 10000, n = 1000)
    if (length(dRoot) == 0) return(.Machine$double.xmax)
    if (length(dRoot) > 1) dRoot <- dRoot[length(dRoot)]
    if (dRoot > d0) dRoot <- d0
    d[i] <- dRoot
    hd[i] <- hdRoot
  }
  signalInsilico <- I0 + IHD * hd + ID * d
  if (eval) {
    return(data.frame(insilico = signalInsilico, d = d, hd = hd))
  }
  return( sum( abs(signal - signalInsilico) / signal ) )
}