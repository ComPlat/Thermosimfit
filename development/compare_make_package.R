# make_package migration check: the compiled functions of the installed tsf
# have to be bit-identical to translate() of the originals on branch main.
# Run from the repository root after installing tsf.
old <- new.env()
for (file in c("tsf/R/lossFunctions_ast2ast.R", "tsf/R/lossFunctionsPso_ast2ast.R")) {
  source_code <- system2("git", c("show", paste0("main:", file)), stdout = TRUE)
  eval(parse(text = source_code), envir = old)
}

cases <- list(
  dba_dye_const = list(
    model = "dba", path = "./tsf/inst/extdata/dba_dye_const_real.txt",
    additionalParameters = c(dye = 0.000151), lowerBounds = 1, upperBounds = 1e8
  ),
  dba_host_const = list(
    model = "dba", path = "./tsf/inst/extdata/dba_host_const_real.txt",
    additionalParameters = c(host = 0.000151), lowerBounds = 1, upperBounds = 1e5
  ),
  ida = list(
    model = "ida", path = "./tsf/inst/extdata/ida_real.txt",
    additionalParameters = c(host = 4.3e-6, dye = 6e-6, kHD = 1.7e7),
    lowerBounds = 1e-15, upperBounds = 1e10
  ),
  gda = list(
    model = "gda", path = "./tsf/inst/extdata/gda_real.txt",
    additionalParameters = c(host = 50e-6, guest = 292e-6, kHD = 33000),
    lowerBounds = 5e3, upperBounds = 3e5
  )
)

set.seed(1)
for (case in names(cases)) {
  cfg <- cases[[case]]
  model <- cfg$model
  df <- read.csv(cfg$path, sep = "\t", dec = ".", header = TRUE)
  add_params <- tsf:::vapro_a2a_spec(case)$build_add_params(df, cfg$additionalParameters)
  types_f <- old[[paste0("types_f_", model)]]

  loss_old <- ast2ast::translate(old[[sprintf("loss_fct_%s_a2a", model)]], types_f = types_f)
  grid_old <- ast2ast::translate(old[[sprintf("grid_search_%s_a2a", model)]], types_f = types_f)
  pso_old <- ast2ast::translate(old[[sprintf("loss_fct_pso_%s_a2a", model)]], types_f = types_f)
  loss_new <- tsf:::vapro_a2a_spec(case)$loss_fct
  grid_new <- tsf:::vapro_a2a_spec(case)$grid_fct
  pso_new <- tsf:::pso_a2a_spec(case)$loss_fct

  parameter <- exp(seq(log(cfg$lowerBounds), log(cfg$upperBounds), length.out = 500))
  stopifnot(identical(
    vapply(parameter, function(p) loss_old(p, add_params), numeric(1)),
    vapply(parameter, function(p) loss_new(p, add_params), numeric(1))
  ))

  stopifnot(identical(
    grid_old(cfg$lowerBounds, cfg$upperBounds, 1000L, add_params),
    grid_new(cfg$lowerBounds, cfg$upperBounds, 1000L, add_params)
  ))

  n_parameter <- 1L + 3L * add_params$n_sigs
  for (error_code in 1:4) {
    for (i in 1:200) {
      particle <- c(exp(runif(1, log(1e2), log(1e6))), runif(n_parameter - 1L, 0, 1e6))
      stopifnot(identical(
        pso_old(particle, add_params, error_code),
        pso_new(particle, add_params, error_code)
      ))
    }
  }
  cat(case, ": make_package and translate() are bit-identical\n")
}
