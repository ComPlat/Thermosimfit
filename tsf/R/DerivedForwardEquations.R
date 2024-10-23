# eqns: [h + hd + -h0 = 0,
#        d + hd -d0 = 0,
#        hd / (h*d) -kd = 0],
# Eq Nr.1
# Kd = hd / (h * d)
# hd = Kd * h * d
# Eq Nr.2
# hd = d0 - d
# Eq Nr.3
# hd = h0 - h
# Eq Nr.2 == Eq Nr.3
# d0 - d = h0 - h
# d = d0 -h0 + h
# Substitute d in Eq Nr.2
# hd = Kd * h *(d0 -h0 + h)
# Goal:
# h + h_d - h0
equation_h_dba <- function(h, Kd, h0, d0) {
  if (h <= 0) {
    return(Inf)
  }
  denom_Kd <- 1 + Kd * h
  if (denom_Kd == 0) {
    return(Inf)
  }
  h_d <- (Kd * h * d0) / denom_Kd
  residual <- h + h_d - h0
  return(residual)
}

# eqns: [0 = h + hd + hga -h0,
#        0 = d + hd -d0,
#        0 = ga + hga -ga0,
#        0 = hga / (h*ga) -kga,
#        0 = hd / (h*d) -kd],
# denom Kd
# Eq Nr.1
# Kd = hd / (h * d)
# hd = Kd * h * d
# Eq Nr.2
# d0 = d + hd
# d0 = d + Kd * h * d
# d0 = d * (1 + Kd * h)
# d = d0 / (1 + Kd * h)
# Back substitution in Eq Nr.1
# hd = Kd * h * d
# hd = Kd * h * (d0 / (1 + Kd * h))
# denom Kg
# Eq Nr.1
# Kg = hg / (h * g)
# hg = Kg * h * g
# Eq Nr.2
# g0 = g + hg
# g0 = g + Kg * h * g
# g0 = g * (1 + Kg * h)
# g = g0 / (1 + Kg * h)
# Back substitution in Eq Nr. 1
# hg = Kg * h * (g0 / (1 + Kg * h))
equation_h_ida_gda <- function(h, Kd, Kg, h0, d0, g0) {
  if (h <= 0) {
    return(Inf)
  }
  denom_Kd <- 1 + Kd * h
  if (denom_Kd == 0) {
    return(Inf)
  }
  h_d <- (Kd * h * d0) / denom_Kd
  if (g0 == 0) {
    h_g <- 0
  } else {
    denom_Kg <- 1 + Kg * h
    if (denom_Kg == 0) {
      return(Inf)
    }
    h_g <- (Kg * h * g0) / denom_Kg
  }
  residual <- h + h_d + h_g - h0
  return(residual)
}

