# (1) h + hg + hgg = h0
# (2) g + hg + 2 hgg = g0
# (3) hg = kg * h * g
# (4) hgg = kgg * hg * g = kgg * (kg * h * g) * g = kgg * kg * h * g^2

# 3 and 4 into 1: h0 = h + kg*h* g + kgg*kg*h * g^2
# ==> h0 = h (1 + kg*g + kgg*kg*g^2)

# D = (1 + kg*g + kgg*kg*g^2)
# h = h0 / D
# hg = kg * (h0 / D) * g = (kg*h0*g) / D
# hgg = kgg*kg * (h0 / D) * g^2 = (kgg*kg*h0*g^2) / D
# g0 = g + (kg * h0 * g) / D + 2*(kgg*kg*h0*g^2) / D
# D*g0 = g*D + kg*h0*g + 2*kgg*kg*h0*g^2
# (5) 0 = g*D + kg*h0*g + 2*kgg*kg*h0*g^2 - D*g0
# (5) 0 = g * (1 + kg*g + kgg*kg*g^2) + kg*h0*g + 2*kgg*kg*h0*g^2 - g0*(1 + kg*g + kgg*kg*g^2)
# (5) 0 = g + kg*g^2 + kgg*kg*g^3 + kg*h0*g + 2*kgg*kg*h0*g^2 - g0 - kg*g*g0 - kgg*kg*g^2*g0
# (5) 0 = kgg*kg*g^3 + g^2*(kg + 2*kgg*kg*h0 - kgg*kg*g0) + g*(1 + kg*h0 - kg*g0) - g0
# (5) 0 = kgg*kg*g^3 +
#         g^2*(kg*(1 + 2*kgg*h0 - kgg*g0)) +
#         g*(1 + kg*h0 - kg*g0) -
#         g0
# Find roots of g
# calculate D
# h = h0 / D
# hg = kg * h * g
# hgg = kgg * hg * g

solve_system <- function(h0,g0,kg,kgg) {
  coefs <- c(kgg*kg,
             kg*(1 + 2*kgg*h0 - kgg*g0),
             1 + kg*h0 - kg*g0,
             -g0)
  r <- polyroot(coefs)
  g_candidates <- Re(r[abs(Im(r)) < 1e-10 & Re(r) > 0])
  sols <- lapply(g_candidates, function(g){
    D <- 1 + kg*g + kg*kgg*g^2
    h  <- h0 / D
    hg <- kg*h*g
    hgg<- kgg*hg*g
    list(g = g,h = h,hg = hg,hgg = hgg,
         ok = is.finite(h) && h > 0 && hg > 0 && hgg > 0)
  })
  for (i in seq_len(length(sols))) {
    sol <- sols[[i]]
    if (sol$ok) {
      return(sol[1:4])
    }
  }
  stop("Could not solve the system")
}

solve_system(1, 1, 100, 100)
