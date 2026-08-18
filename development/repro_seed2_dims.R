library(tsf)

path <- system.file("extdata", "dba_dye_const_real.txt", package = "tsf")
df0 <- read.csv(path, header = FALSE, sep = "\t")
parameter <- c(3e3, 2.0, 1.65e7, 1.6e6)
envSetup <- new.env()
envSetup$d0 <- 5
envSetup$host <- df0[, 1]
envSetup$signal <- df0[, 2]
envSetup$n_sigs <- 1L
result <- tsf:::lossFctDBA(parameter, envSetup, TRUE)
df0[, 2] <- result$insilico
file <- tempfile(fileext = ".txt")
write.csv(df0, file, quote = FALSE, row.names = FALSE)

# ---- replicate opti()'s internals directly (optimize.R lines 190-273),
# bypassing its outer tryCatch (which does stop(conditionMessage(e)) and
# destroys the traceback), so we get the real call stack. -----------------
case <- "dba_dye_const"
lowerBounds <- c(1, 0, 1e2, 1e2)
upperBounds <- c(1e8, 1e4, 1e8, 1e8)
additionalParameters <- envSetup$d0
seed <- 2L
npop <- 40
ngen <- 100
errorThreshold <- -Inf
Topo <- FALSE
add_info <- ""
engine <- "ast2ast"
error_calc_fct <- "Rel. Error"

df <- tsf:::importData(file)
n_sigs <- ncol(df) - 1
lossFct <- tsf:::lossFctDBA
error_fct <- tsf:::get_error_calc_fct(error_calc_fct)
error_fct_name <- error_calc_fct
error_code <- tsf:::pso_error_code(error_calc_fct)

env <- new.env()
env$error_calc_fct <- error_fct
env$n_sigs <- n_sigs
names(df)[1] <- "host"
env$host <- df[, 1]
env$signal <- df[, -1] |> as.data.frame()
env$d0 <- additionalParameters[1]

spec <- tsf:::pso_a2a_spec(case)
add_params <- spec$build_add_params(df, additionalParameters)
loss_particle_a2a <- ast2ast::translate(spec$loss_fct, args_f = spec$args_f, types_f = spec$types_f)

runAsShiny <- new.env()
runAsShiny$insilico <- NULL

set.seed(seed)
options(error = function() { traceback(3) })

res <- tsf:::pso(
  env, lowerBounds, upperBounds, lossFct, ngen, npop,
  errorThreshold, Topo, FALSE, runAsShiny, add_info,
  engine = engine, loss_particle_a2a = loss_particle_a2a,
  add_params = add_params, error_code = error_code
)
cat("pso() OK\n")

params <- tsf:::create_params_df(res, case, n_sigs)
cat("create_params_df OK\n")

df2 <- tsf:::create_data_df(df, res, case, n_sigs)
cat("create_data_df OK\n")

lowerBounds2 <- tsf:::correct_names_params(lowerBounds, case, n_sigs)
upperBounds2 <- tsf:::correct_names_params(upperBounds, case, n_sigs)
additionalParameters2 <- tsf:::correct_names_additional_param(additionalParameters, case)
cat("correct_names_* OK\n")

signal_plots <- tsf:::plot_signals(df2, case, n_sigs)
cat("plot_signals OK\n")

d_hd_plot <- tsf:::plot_d_hd(df2, case, n_sigs)
cat("plot_d_hd OK\n")

m <- tsf:::metrices(df2, error_fct_name, n_sigs)
cat("metrices OK\n")

cat("ALL STEPS SUCCEEDED\n")
