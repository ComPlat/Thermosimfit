# Per-particle parity between the plain-R PSO loss functions (lossFunctions.R)
# and their ast2ast-compiled counterparts (lossFunctionsPso_ast2ast.R), for
# one candidate parameter vector, across all four built-in error functions
# and all four cases. This is the unit underlying opti(engine = "ast2ast")'s
# per-particle fitness calls (see test_pso_vs_ast2ast.R for the full-run
# parity check built on top of this).
library(tinytest)
library(tsf)

error_names <- c("Rel. Error", "RMSE", "SSE", "Huber")

check_loss_matches <- function(case, lossFctR, envBuild, additionalParameters, path, seed = 2L) {
  df <- if (!is.data.frame(path)) tsf:::importData(path) else path
  spec <- tsf:::pso_a2a_spec(case)
  add_params <- spec$build_add_params(df, additionalParameters)
  loss_a2a <- ast2ast::translate(spec$loss_fct, args_f = spec$args_f, types_f = spec$types_f)

  n_sigs <- add_params$n_sigs
  set.seed(seed)
  particle <- c(runif(1, 1e3, 1e6), runif(3 * n_sigs, 0, 1e6))

  for (name in error_names) {
    env <- envBuild(tsf:::get_error_calc_fct(name))
    r_val <- lossFctR(particle, env)
    a2a_val <- loss_a2a(particle, add_params, tsf:::pso_error_code(name))
    expect_equal(a2a_val, r_val, tolerance = 1e-8, info = sprintf("%s / %s", case, name))
  }
}

test_dba_dye_const_loss_matches <- function() {
  path <- system.file("extdata", "dba_dye_const_real.txt", package = "tsf")
  envBuild <- function(error_calc_fct) {
    e <- new.env()
    e$error_calc_fct <- error_calc_fct
    e$n_sigs <- 1L
    df <- tsf:::importData(path)
    names(df)[1] <- "host"
    e$host <- df[, 1]
    e$signal <- df[, -1] |> as.data.frame()
    e$d0 <- 0.000151
    e
  }
  check_loss_matches("dba_dye_const", tsf:::lossFctDBA, envBuild, 0.000151, path)
}
test_dba_dye_const_loss_matches()

test_dba_host_const_loss_matches <- function() {
  path <- system.file("extdata", "dba_host_const_real.txt", package = "tsf")
  envBuild <- function(error_calc_fct) {
    e <- new.env()
    e$error_calc_fct <- error_calc_fct
    e$n_sigs <- 1L
    df <- tsf:::importData(path)
    names(df)[1] <- "dye"
    e$dye <- df[, 1]
    e$signal <- df[, -1] |> as.data.frame()
    e$h0 <- 0.000151
    e
  }
  check_loss_matches("dba_host_const", tsf:::lossFctHG, envBuild, 0.000151, path)
}
test_dba_host_const_loss_matches()

test_ida_loss_matches <- function() {
  path <- system.file("extdata", "ida_real.txt", package = "tsf")
  additionalParameters <- c(4.3e-6, 6e-6, 1.7e7)
  envBuild <- function(error_calc_fct) {
    e <- new.env()
    e$error_calc_fct <- error_calc_fct
    e$n_sigs <- 1L
    df <- tsf:::importData(path)
    names(df)[1] <- "guest"
    e$ga <- df[, 1]
    e$signal <- df[, -1] |> as.data.frame()
    e$h0 <- additionalParameters[1]
    e$d0 <- additionalParameters[2]
    e$kd <- additionalParameters[3]
    e
  }
  check_loss_matches("ida", tsf:::lossFctIDA, envBuild, additionalParameters, path)
}
test_ida_loss_matches()

test_gda_loss_matches <- function() {
  path <- system.file("extdata", "gda_real.txt", package = "tsf")
  additionalParameters <- c(50e-6, 292e-6, 33000)
  envBuild <- function(error_calc_fct) {
    e <- new.env()
    e$error_calc_fct <- error_calc_fct
    e$n_sigs <- 1L
    df <- tsf:::importData(path)
    names(df)[1] <- "dye"
    e$dye <- df[, 1]
    e$signal <- df[, -1] |> as.data.frame()
    e$h0 <- additionalParameters[1]
    e$ga0 <- additionalParameters[2]
    e$kd <- additionalParameters[3]
    e
  }
  check_loss_matches("gda", tsf:::lossFctGDA, envBuild, additionalParameters, path)
}
test_gda_loss_matches()
