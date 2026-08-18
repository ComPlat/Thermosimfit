# Dev smoke test for the ast2ast-compiled VAPRO path (tsf/R/lossFunctions_ast2ast.R,
# tsf/R/OptimizeVapro_ast2ast.R), covering all four cases: dba_dye_const,
# dba_host_const, ida and gda. For each case:
#   1. opti_vapro_ast2ast() runs both the plain-R fit and the ast2ast fit on
#      real data and prints both -- eyeball check that they agree.
#   2. microbenchmark compares vapro_grid_search (R) against
#      vapro_grid_search_a2a (ast2ast) on the same data.
#   3. vapro_ast2ast_bootstrap() runs a (small, for speed) bootstrap end to
#      end and prints n_success/n_fail and the summary -- not a real
#      bootstrap run, just checking it doesn't error and produces sane output.
library(microbenchmark)

cases <- list(
  dba_dye_const = list(
    path = "./tsf/inst/extdata/dba_dye_const_real.txt",
    additionalParameters = c(dye = 0.000151),
    lowerBounds = 1, upperBounds = 1e8
  ),
  dba_host_const = list(
    path = "./tsf/inst/extdata/dba_host_const_real.txt",
    additionalParameters = c(host = 0.000151),
    lowerBounds = 1, upperBounds = 1e5
  ),
  ida = list(
    path = "./tsf/inst/extdata/ida_real.txt",
    additionalParameters = c(host = 4.3e-6, dye = 6e-6, kHD = 1.7e7),
    lowerBounds = 1e-15, upperBounds = 1e10
  ),
  gda = list(
    path = "./tsf/inst/extdata/gda_real.txt",
    additionalParameters = c(host = 50e-6, guest = 292e-6, kHD = 33000),
    lowerBounds = 5e3, upperBounds = 3e5
  )
)

for (case in names(cases)) {
  cfg <- cases[[case]]
  df <- read.csv(cfg$path, sep = "\t", dec = ".", header = TRUE)

  cat("\n==============================\n", case, "\n==============================\n")

  # ---- fit comparison: R vs ast2ast --------------------------------------
  a2a_fit <- tsf:::opti_vapro_ast2ast(
    case, cfg$lowerBounds, cfg$upperBounds, df, cfg$additionalParameters,
    nGrid = 1000L
  )
  cat("R   param_hat:", a2a_fit$r$param_hat, " loss:", a2a_fit$r$loss_hat, "\n")
  cat("a2a param_hat:", a2a_fit$a2a$param_hat, " loss:", a2a_fit$a2a$loss_hat, "\n")

  # ---- benchmark ----------------------------------------------------------
  spec <- tsf:::vapro_a2a_spec(case)
  env <- tsf:::vapro_build_env(case, df, cfg$additionalParameters, tsf:::get_error_calc_fct("Rel. Error"))
  add_params <- spec$build_add_params(df, cfg$additionalParameters)
  loss_a2a <- ast2ast::translate(spec$loss_fct, args_f = spec$args_f_loss, types_f = spec$types_f)

  bm <- microbenchmark::microbenchmark(
    R = tsf:::vapro_grid_search(tsf:::vapro_loss_fct(case), env, cfg$lowerBounds, cfg$upperBounds, 1000L),
    ast2ast = tsf:::vapro_grid_search_a2a(loss_a2a, add_params, cfg$lowerBounds, cfg$upperBounds, 1000L),
    times = 10L
  )
  print(bm)

  # ---- bootstrap smoke test -----------------------------------------------
  # small nBoot on purpose -- just checking it runs end to end and produces
  # sane output, not a real bootstrap run (bump nBoot up for that).
  boot <- tsf:::vapro_ast2ast_bootstrap(
    case, cfg$lowerBounds, cfg$upperBounds, df, cfg$additionalParameters,
    nBoot = 50L, nGrid = 500L, seed = 1234L
  )
  cat("n_success:", boot$n_success, " n_fail:", boot$n_fail, "\n")
  print(boot$summary)
}
