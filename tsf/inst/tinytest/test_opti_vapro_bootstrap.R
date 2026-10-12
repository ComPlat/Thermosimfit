library(tsf)
library(tinytest)

cases <- list(
  dba_dye_const = list(
    path = system.file("extdata", "dba_dye_const_real.txt", package = "tsf"),
    ap = 0.000151, lb = 1, ub = 1e8
  ),
  dba_host_const = list(
    path = system.file("extdata", "dba_host_const_real.txt", package = "tsf"),
    ap = 0.000151, lb = 1, ub = 1e5
  ),
  gda = list(
    path = system.file("extdata", "gda_real.txt", package = "tsf"),
    ap = c(50e-6, 292e-6, 33000), lb = 5e3, ub = 3e5
  ),
  ida = list(
    path = system.file("extdata", "ida_real.txt", package = "tsf"),
    ap = c(4.3e-6, 6e-6, 1.7e7), lb = 1e-15, ub = 1e10
  )
)

check_bootstrap_result <- function(res, nBoot, case_name, engine) {
  info <- paste(case_name, engine)
  expect_false(inherits(res, "ErrorClass"),
    info = paste(info, if (inherits(res, "ErrorClass")) res$message else ""))
  expect_true(is.data.frame(res$draws), info = info)
  expect_true(is.data.frame(res$summary), info = info)
  expect_equal(res$n_success + res$n_fail, nBoot, info = info)
  expect_true(res$n_success > 0, info = info)
}

test_vapro_bootstrap_r_engine <- function(case_name) {
  cfg <- cases[[case_name]]
  set.seed(1)
  res <- opti_vapro_bootstrap(case_name, cfg$lb, cfg$ub, cfg$path, cfg$ap,
    nBoot = 5, nGrid = 200, showProgress = FALSE
  )
  check_bootstrap_result(res, 5, case_name, "r")
}
for (case_name in names(cases)) {
  test_vapro_bootstrap_r_engine(case_name)
}

test_vapro_bootstrap_ast2ast_engine <- function(case_name) {
  cfg <- cases[[case_name]]
  set.seed(1)
  res <- opti_vapro_bootstrap(case_name, cfg$lb, cfg$ub, cfg$path, cfg$ap,
    nBoot = 5, nGrid = 200, engine = "ast2ast", showProgress = FALSE
  )
  check_bootstrap_result(res, 5, case_name, "ast2ast")
}
for (case_name in names(cases)) {
  test_vapro_bootstrap_ast2ast_engine(case_name)
}
