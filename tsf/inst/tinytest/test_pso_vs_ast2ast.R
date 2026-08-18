# opti(engine = "ast2ast") replaces only the per-particle loss evaluation
# inside pso()'s existing asynchronous loop (RNG draw order, live neighbor
# lookups all untouched) -- so with the same seed it has to reproduce
# opti(engine = "r") essentially exactly, not just converge to a similar
# fit. This guards that parity across all four cases.
library(tinytest)
library(tsf)

check_engines_match <- function(case, path, additionalParameters, lb, ub, ngen = 100L, seed = 1234L) {
  res_r <- tsf::opti(case,
    lowerBounds = lb, upperBounds = ub, path = path,
    additionalParameters = additionalParameters, ngen = ngen, seed = seed
  )
  res_a2a <- tsf::opti(case,
    lowerBounds = lb, upperBounds = ub, path = path,
    additionalParameters = additionalParameters, ngen = ngen, seed = seed,
    engine = "ast2ast"
  )
  expect_equal(unlist(res_r$parameter), unlist(res_a2a$parameter), tolerance = 1e-6)
  expect_equal(res_r$metrices$R2, res_a2a$metrices$R2, tolerance = 1e-8)
}

test_dba_dye_const_engines_match <- function() {
  Kd <- 5e6; I0 <- 25; Ihd <- 8e5; Id <- 1.5e5
  d0 <- 1e-4; h0 <- seq(1e-15, 1.5e-4, length.out = 30)
  sim <- tsf:::forward_dba_dye_const(Kd, Id, Ihd, d0, h0)
  sim$Signal <- sim$Signal + I0
  check_engines_match("dba_dye_const", sim, d0, c(1, 0, 1e2, 1e2), c(1e8, 1e3, 1e7, 1e7))
}
test_dba_dye_const_engines_match()

test_dba_host_const_engines_match <- function() {
  Kd <- 3e6; I0 <- 15; Ihd <- 6e5; Id <- 1.2e5
  h0 <- 1e-4; d0 <- seq(1e-15, 1.5e-4, length.out = 30)
  sim <- tsf:::forward_dba_host_const(Kd, Id, Ihd, h0, d0)
  sim$Signal <- sim$Signal + I0
  check_engines_match("dba_host_const", sim, h0, c(6e5, 0, 1e4, 1e4), c(8e6, 1e2, 1e7, 8e5))
}
test_dba_host_const_engines_match()

test_ida_engines_match <- function() {
  Kd <- 3e6; Kg <- 2e7; I0 <- 0; Ihd <- 1e6; Id <- 2e5
  h0 <- 1e-6; d0 <- 1e-6; g0 <- seq(1e-15, 5e-6, length.out = 30)
  sim <- tsf:::forward_ida(Kg, Ihd, Id, Kd, h0, d0, g0)
  sim$Signal <- sim$Signal + I0
  check_engines_match("ida", sim, c(h0, d0, Kd), c(1e3, 0, 1e2, 1e-15), c(1e9, 1e4, 1e8, 1e8))
}
test_ida_engines_match()

test_gda_engines_match <- function() {
  Kd <- 1.7e7; Kg <- 1.8e6; I0 <- 0.0408218; Ihd <- 602000; Id <- 0
  h0 <- 1.65e-6; g0 <- 1.32e-6; d0 <- seq(1e-15, 7.5e-6, length.out = 30)
  sim <- tsf:::forward_gda(Kd, Kg, Id, Ihd, h0, g0, d0)
  sim$Signal <- sim$Signal + I0
  check_engines_match("gda", sim, c(h0, g0, Kd), c(10, 0, 1e2, 0), c(1e8, 1, 1e7, 1e7))
}
test_gda_engines_match()
