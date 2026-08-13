# test opti_vapro
library(tsf)
library(tinytest)

# Real experimental data has no known ground truth, so unlike the synthetic
# tests below, we can't check parameter recovery. What we can check: both
# opti_vapro and opti() converge, fit the measured signal reasonably well
# (R2), and agree with each other on the identifiable nonlinear binding
# constant (Kd/Kg) - if the two independent methods disagree a lot on real
# data, that's a sign something is off (bad bounds, a bug, or genuine
# multimodality), even without knowing the "true" value.
check_real_data_fit <- function(case, path, additionalParameters,
                                 vaproLower, vaproUpper,
                                 psoLower, psoUpper,
                                 minR2Vapro = 0.9, minR2Pso = 0.9,
                                 maxRelDisagreement = 0.25,
                                 ngen = 500, seed = 1234) {
  fit <- tsf::opti_vapro(
    case = case,
    lowerBounds = vaproLower,
    upperBounds = vaproUpper,
    path = path,
    additionalParameters = additionalParameters,
    nGrid = 2000
  )
  checks <- fit$metrices$R2 >= minR2Vapro

  res <- tsf::opti(
    case = case,
    lowerBounds = psoLower,
    upperBounds = psoUpper,
    path = path,
    seed = seed,
    additionalParameters = additionalParameters,
    ngen = ngen
  )
  checks <- c(checks, res$metrices$R2 >= minR2Pso)

  kVapro <- fit$parameter[[1]]
  kPso <- res$parameter[[1]]
  checks <- c(checks, abs(kVapro - kPso) / kVapro < maxRelDisagreement)
  expect_true(all(checks))
}

# dba_dye_const: host titrated at fixed dye = 0.000151 M
# source: Tests/DBA_const_dye/dba_dye_const.txt (VAPRO/test_vapro_dba.R)
test_dba_dye_const_real <- function() {
  path <- system.file("extdata", "dba_dye_const_real.txt", package = "tsf")
  check_real_data_fit(
    case = "dba_dye_const",
    path = path,
    additionalParameters = 0.000151,
    vaproLower = 1, vaproUpper = 1e8,
    psoLower = c(1, 0, 1e2, 1e2), psoUpper = c(1e8, 1e4, 1e8, 1e8)
  )
}
test_dba_dye_const_real()

# dba_host_const: dye titrated at fixed host = 0.000151 M
# source: Tests/DBA_const_host/dba_dye_const.txt (VAPRO/test_vapro_hg.R)
test_dba_host_const_real <- function() {
  path <- system.file("extdata", "dba_host_const_real.txt", package = "tsf")
  check_real_data_fit(
    case = "dba_host_const",
    path = path,
    additionalParameters = 0.000151,
    vaproLower = 1, vaproUpper = 1e5,
    psoLower = c(1, 0, 1, 1), psoUpper = c(1e5, 1e8, 1e8, 1e8)
  )
}
test_dba_host_const_real()

# gda: dye titrated at fixed host = 1.65e-6 M, guest = 1.32e-6 M,
# Kd (dye-host) = 1.7e7. source: Tests/GDA/data_GDA-Estradiol-CB7-BE.txt
# (VAPRO/test_vapro_gda.R)
test_gda_real <- function() {
  path <- system.file("extdata", "gda_real.txt", package = "tsf")
  check_real_data_fit(
    case = "gda",
    path = path,
    additionalParameters = c(50e-6, 292e-6, 33000),
    vaproLower = 5e3, vaproUpper = 3e5,
    psoLower = c(5e3, 0, 0, 0), psoUpper = c(3e5, 1e4, 1e10, 1e6),
    ngen = 300, maxRelDisagreement = 0.4
  )
}
test_gda_real()

# ida: guest titrated at fixed host = 4.3e-6 M, dye = 6e-6 M,
# Kd (dye-host) = 1.7e7. source: first replicate block of Paper/IDA.txt
# (see Paper/OtherErrors/run_simulations.R for the known additionalParameters)
test_ida_real <- function() {
  path <- system.file("extdata", "ida_real.txt", package = "tsf")
  check_real_data_fit(
    case = "ida",
    path = path,
    additionalParameters = c(4.3e-6, 6e-6, 1.7e7),
    vaproLower = 1e-15, vaproUpper = 1e10,
    psoLower = c(0, 0, 0, 0), psoUpper = c(1e10, 1e2, 1e10, 1e10)
  )
}
test_ida_real()

test_ida_vapro <- function() {
  Kd <- 3e6
  Kg <- 2e7
  I0 <- 0
  Ihd <- 1e6
  Id <- 2e5
  h0 <- 1e-6
  d0 <- 1e-6
  g0 <- seq(1e-15, 5e-6, length.out = 30)

  simulated <- tsf:::forward_ida(Kg, Ihd, Id, Kd, h0, d0, g0)
  simulated$Signal <- simulated$Signal + I0

  # Signal = I0 + IHD*hd + ID*d, and hd = d0 - d always (mass balance), so
  # only two combinations of (I0, IHD, ID) are identifiable from a single
  # curve at fixed d0: the effective intercept (I0 + IHD*d0) and the
  # effective slope (ID - IHD). Individual I0/IHD/ID are not identifiable.
  effIntercept <- I0 + Ihd * d0
  effSlope <- Id - Ihd

  fit <- tsf::opti_vapro(
    case = "ida",
    lowerBounds = 1e3,
    upperBounds = 1e9,
    path = simulated,
    additionalParameters = c(h0, d0, Kd),
    nGrid = 2000
  )
  expect_true(fit$metrices$R2 >= 0.999)
  expect_true(abs(fit$parameter[[1]] - Kg) / Kg < 0.05)
  fitEffIntercept <- fit$parameter[[2]] + fit$parameter[[3]] * d0
  fitEffSlope <- fit$parameter[[4]] - fit$parameter[[3]]
  expect_true(abs(fitEffIntercept - effIntercept) / abs(effIntercept) < 0.05)
  expect_true(abs(fitEffSlope - effSlope) / abs(effSlope) < 0.05)

  res <- tsf::opti(
    case = "ida",
    lowerBounds = c(1e3, 0, 1e2, 1e-15),
    upperBounds = c(1e9, 1e4, 1e8, 1e8),
    path = simulated,
    additionalParameters = c(h0, d0, Kd),
    ngen = 300, seed = 1234
  )
  expect_true(res$metrices$R2 >= 0.99)
  expect_true(abs(res$parameter[[1]] - Kg) / Kg < 0.25)
  resEffIntercept <- res$parameter[[2]] + res$parameter[[3]] * d0
  resEffSlope <- res$parameter[[4]] - res$parameter[[3]]
  expect_true(abs(resEffIntercept - effIntercept) / abs(effIntercept) < 0.1)
  expect_true(abs(resEffSlope - effSlope) / abs(effSlope) < 0.1)
}
test_ida_vapro()

# dba_dye_const: dye is fixed (d0 constant) and host is swept, so like ida
# hd = d0 - d always, and only the effective intercept/slope are identifiable.
test_dba_dye_const_vapro <- function() {
  Kd <- 5e6
  I0 <- 25
  Ihd <- 8e5
  Id <- 1.5e5
  d0 <- 1e-4
  h0 <- seq(1e-15, 1.5e-4, length.out = 30)

  simulated <- tsf:::forward_dba_dye_const(Kd, Id, Ihd, d0, h0)
  simulated$Signal <- simulated$Signal + I0

  effIntercept <- I0 + Ihd * d0
  effSlope <- Id - Ihd

  fit <- tsf::opti_vapro(
    case = "dba_dye_const",
    lowerBounds = 1,
    upperBounds = 1e8,
    path = simulated,
    additionalParameters = d0,
    nGrid = 2000
  )
  expect_true(fit$metrices$R2 >= 0.999)
  expect_true(abs(fit$parameter[[1]] - Kd) / Kd < 0.05)
  fitEffIntercept <- fit$parameter[[2]] + fit$parameter[[3]] * d0
  fitEffSlope <- fit$parameter[[4]] - fit$parameter[[3]]
  expect_true(abs(fitEffIntercept - effIntercept) / abs(effIntercept) < 0.05)
  expect_true(abs(fitEffSlope - effSlope) / abs(effSlope) < 0.05)

  res <- tsf::opti(
    case = "dba_dye_const",
    lowerBounds = c(1, 0, 1e2, 1e2),
    upperBounds = c(1e8, 1e3, 1e7, 1e7),
    path = simulated,
    additionalParameters = d0,
    ngen = 300, seed = 1234
  )
  expect_true(res$metrices$R2 >= 0.99)
  expect_true(abs(res$parameter[[1]] - Kd) / Kd < 0.25)
  resEffIntercept <- res$parameter[[2]] + res$parameter[[3]] * d0
  resEffSlope <- res$parameter[[4]] - res$parameter[[3]]
  expect_true(abs(resEffIntercept - effIntercept) / abs(effIntercept) < 0.1)
  expect_true(abs(resEffSlope - effSlope) / abs(effSlope) < 0.1)
}
test_dba_dye_const_vapro()

# dba_host_const: dye is swept (d0 varies row to row) and host is fixed, so
# hd = d0(row) - d(row) is not constant and I0/IHD/ID are individually
# identifiable (still ill-conditioned, but not exactly rank-deficient).
test_dba_host_const_vapro <- function() {
  Kd <- 3e6
  I0 <- 15
  Ihd <- 6e5
  Id <- 1.2e5
  h0 <- 1e-4
  d0 <- seq(1e-15, 1.5e-4, length.out = 30)

  simulated <- tsf:::forward_dba_host_const(Kd, Id, Ihd, h0, d0)
  simulated$Signal <- simulated$Signal + I0
  checks <- c()

  fit <- tsf::opti_vapro(
    case = "dba_host_const",
    lowerBounds = 5e5,
    upperBounds = 8e6,
    path = simulated,
    additionalParameters = h0,
    nGrid = 2000
  )
  checks <- fit$metrices$R2 >= 0.999
  checks <- c(checks, abs(fit$parameter[[1]] - Kd) / Kd < 0.05)
  checks <- c(checks, abs(fit$parameter[[2]] - I0) / I0 < 0.1)
  checks <- c(checks, abs(fit$parameter[[3]] - Ihd) / Ihd < 0.1)
  checks <- c(checks, abs(fit$parameter[[4]] - Id) / Id < 0.1)

  res <- tsf::opti(
    case = "dba_host_const",
    #               3e6, 15, 6e5, 1.2e5
    lowerBounds = c(6e5, 0, 1e4, 1e4),
    upperBounds = c(8e6, 1e2, 1e7, 8e5),
    path = simulated,
    seed = 1234,
    additionalParameters = h0,
    ngen = 400
  )
  checks <- c(checks, res$metrices$R2 >= 0.99)
  checks <- c(checks, abs(res$parameter[[1]] - Kd) / Kd < 0.25)
  checks <- c(checks, abs(res$parameter[[2]] - I0) / I0 < 0.5)
  checks <- c(checks, abs(res$parameter[[3]] - Ihd) / Ihd < 0.3)
  checks <- c(checks, abs(res$parameter[[4]] - Id) / Id < 0.3)
  expect_true(all(checks))
}
test_dba_host_const_vapro()

# gda: dye is swept (d0 varies row to row), host and guest fixed. Same
# situation as dba_host_const: not exactly rank-deficient, I0/IHD/ID are
# individually identifiable.
test_gda_vapro <- function() {
  Kd <- 1.7e7
  Kg <- 1.8e6
  I0 <- 0.0408218
  Ihd <- 602000
  Id <- 0
  h0 <- 1.65e-6
  g0 <- 1.32e-6
  d0 <- seq(1e-15, 7.5e-6, length.out = 30)

  simulated <- tsf:::forward_gda(Kd, Kg, Id, Ihd, h0, g0, d0)
  simulated$Signal <- simulated$Signal + I0

  fit <- tsf::opti_vapro(
    case = "gda",
    lowerBounds = 10,
    upperBounds = 1e8,
    path = simulated,
    additionalParameters = c(h0, g0, Kd),
    nGrid = 2000
  )
  expect_true(fit$metrices$R2 >= 0.999)
  expect_true(abs(fit$parameter[[1]] - Kg) / Kg < 0.1)
  expect_true(abs(fit$parameter[[2]] - I0) / I0 < 0.2)
  expect_true(abs(fit$parameter[[3]] - Ihd) / Ihd < 0.1)
  expect_true(abs(fit$parameter[[4]] - Id) < 1e3)

  res <- tsf::opti(
    case = "gda",
    lowerBounds = c(10, 0, 1e2, 0),
    upperBounds = c(1e8, 1, 1e7, 1e7),
    path = simulated,
    additionalParameters = c(h0, g0, Kd),
    ngen = 300,
    seed = 1234
  )
  expect_true(res$metrices$R2 >= 0.99)
  expect_true(abs(res$parameter[[1]] - Kg) / Kg < 0.3)
  expect_true(abs(res$parameter[[3]] - Ihd) / Ihd < 0.3)
  expect_true(abs(res$parameter[[4]] - Id) < 1e4)
}
test_gda_vapro()
