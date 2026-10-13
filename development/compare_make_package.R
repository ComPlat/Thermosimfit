# make_package migration check: the compiled functions of the installed tsf
# have to be bit-identical to translate() of the originals on branch main.
# Run from the repository root after installing tsf.
old <- new.env()
for (file in c("tsf/R/lossFunctions_ast2ast.R", "tsf/R/lossFunctionsPso_ast2ast.R")) {
  source_code <- system2("git", c("show", paste0("main:", file)), stdout = TRUE)
  eval(parse(text = source_code), envir = old)
}

df <- read.csv("./tsf/inst/extdata/ida_real.txt", sep = "\t", dec = ".", header = TRUE)
add_params <- tsf:::build_add_params_ida_a2a(df, c(host = 4.3e-6, dye = 6e-6, kHD = 1.7e7))

loss_old <- ast2ast::translate(old$loss_fct_ida_a2a, types_f = old$types_f_ida)
grid_old <- ast2ast::translate(old$grid_search_ida_a2a, types_f = old$types_f_ida)
pso_old <- ast2ast::translate(old$loss_fct_pso_ida_a2a, types_f = old$types_f_ida)

kga <- exp(seq(log(1e-15), log(1e10), length.out = 500))
stopifnot(identical(
  vapply(kga, function(k) loss_old(k, add_params), numeric(1)),
  vapply(kga, function(k) tsf:::loss_fct_ida_a2a(k, add_params), numeric(1))
))

stopifnot(identical(
  grid_old(1e-15, 1e10, 1000L, add_params),
  tsf:::grid_search_ida_a2a(1e-15, 1e10, 1000L, add_params)
))

set.seed(1)
n_parameter <- 1L + 3L * add_params$n_sigs
for (error_code in 1:4) {
  for (i in 1:200) {
    parameter <- c(exp(runif(1, log(1e2), log(1e6))), runif(n_parameter - 1L, 0, 1e6))
    stopifnot(identical(
      pso_old(parameter, add_params, error_code),
      tsf:::loss_fct_pso_ida_a2a(parameter, add_params, error_code)
    ))
  }
}
cat("IDA: make_package and translate() are bit-identical\n")
