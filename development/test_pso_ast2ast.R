# Dev smoke test for the ast2ast-compiled PSO path: engine = "ast2ast" now
# replaces only the per-particle loss evaluation (loss_fct_pso_*_a2a), and
# pso()'s loop (RNG order, async neighbor updates) is untouched -- so
# engine = "r" and engine = "ast2ast" should track each other to
# floating-point precision (a different uniroot implementation is the only
# source of difference), not diverge into different local optima.
library(tsf)

## ---- 1. compiled per-particle loss matches the plain R loss for one
## particle, across all 4 error functions ----------------------------------
cmp_single <- function(case, lossFctR, envBuild, additionalParameters, path) {
  df <- if (!is.data.frame(path)) tsf:::importData(path) else path
  spec <- tsf:::pso_a2a_spec(case)
  add_params <- spec$build_add_params(df, additionalParameters)
  loss_a2a <- ast2ast::translate(spec$loss_fct, args_f = spec$args_f, types_f = spec$types_f)

  n_sigs <- add_params$n_sigs
  set.seed(2)
  particle <- c(runif(1, 1e3, 1e6), runif(3 * n_sigs, 0, 1e6))

  for (name in c("Rel. Error", "RMSE", "SSE", "Huber")) {
    env <- envBuild(get_error_calc_fct = tsf:::get_error_calc_fct(name))
    r_val <- lossFctR(particle, env)
    code <- tsf:::pso_error_code(name)
    a2a_val <- loss_a2a(particle, add_params, code)
    cat(sprintf("  [%s / %s] R=%.10g a2a=%.10g relDiff=%.3g\n",
      case, name, r_val, a2a_val, abs(r_val - a2a_val) / max(abs(r_val), 1e-300)))
  }
}

cmp_single("dba_dye_const", tsf:::lossFctDBA,
  function(get_error_calc_fct) {
    e <- new.env(); e$error_calc_fct <- get_error_calc_fct; e$n_sigs <- 1
    df <- tsf:::importData(system.file("extdata", "dba_dye_const_real.txt", package = "tsf"))
    names(df)[1] <- "host"; e$host <- df[, 1]; e$signal <- df[, -1] |> as.data.frame()
    e$d0 <- 0.000151
    e
  }, 0.000151, system.file("extdata", "dba_dye_const_real.txt", package = "tsf"))

cmp_single("ida", tsf:::lossFctIDA,
  function(get_error_calc_fct) {
    e <- new.env(); e$error_calc_fct <- get_error_calc_fct; e$n_sigs <- 1
    df <- tsf:::importData(system.file("extdata", "ida_real.txt", package = "tsf"))
    names(df)[1] <- "guest"; e$ga <- df[, 1]; e$signal <- df[, -1] |> as.data.frame()
    e$h0 <- 4.3e-6; e$d0 <- 6e-6; e$kd <- 1.7e7
    e
  }, c(4.3e-6, 6e-6, 1.7e7), system.file("extdata", "ida_real.txt", package = "tsf"))

## ---- 2. full opti() run, engine="ast2ast" vs engine="r": trajectories
## should match closely (same seed, same RNG order, same async updates) ----
cmp_opti <- function(label, case, path, additionalParameters, lb, ub, trueKd, ngen = 60L) {
  capture_traj <- function(engine) {
    out <- capture.output({
      res <- tsf::opti(case,
        lowerBounds = lb, upperBounds = ub, path = path,
        additionalParameters = additionalParameters, ngen = ngen, seed = 1234,
        engine = engine
      )
    })
    errs <- grep('^\\[1\\] "[0-9.]+e[+-][0-9]+"$', out, value = TRUE)
    errs <- as.numeric(gsub('^\\[1\\] "|"$', "", errs))
    list(res = res, traj = errs)
  }
  a2a <- capture_traj("ast2ast")
  r <- capture_traj("r")

  cat(sprintf("[%s] K: r: %s ast2ast: %s (true %s)\n",
    label,
    format(r$res$parameter[[1]], scientific = TRUE),
    format(a2a$res$parameter[[1]], scientific = TRUE),
    format(trueKd, scientific = TRUE))
  cat(sprintf("[%s] final loss: r=%s ast2ast=%s\n",
    label,
    format(r$traj[length(r$traj)], scientific = TRUE),
    format(a2a$traj[length(a2a$traj)], scientific = TRUE)
  ))
  n <- min(length(r$traj), length(a2a$traj))
  reldiff <- abs(r$traj[1:n] - a2a$traj[1:n]) / pmax(abs(r$traj[1:n]), 1e-300)
  cat(sprintf("[%s] per-generation loss trajectory relDiff: max=%.3g mean=%.3g (n=%d gens)\n",
    label, max(reldiff), mean(reldiff), n))
}

Kd <- 5e6; I0 <- 25; Ihd <- 8e5; Id <- 1.5e5
d0 <- 1e-4; h0 <- seq(1e-15, 1.5e-4, length.out = 30)
sim <- tsf:::forward_dba_dye_const(Kd, Id, Ihd, d0, h0)
sim$Signal <- sim$Signal + I0
cmp_opti("dba_dye_const", "dba_dye_const", sim, d0,
  c(1, 0, 1e2, 1e2), c(1e8, 1e3, 1e7, 1e7), Kd, ngen = 100)

Kd <- 3e6; I0 <- 15; Ihd <- 6e5; Id <- 1.2e5
h0f <- 1e-4; d0v <- seq(1e-15, 1.5e-4, length.out = 30)
sim <- tsf:::forward_dba_host_const(Kd, Id, Ihd, h0f, d0v)
sim$Signal <- sim$Signal + I0
cmp_opti("dba_host_const", "dba_host_const", sim, h0f,
  c(6e5, 0, 1e4, 1e4), c(8e6, 1e2, 1e7, 8e5), Kd)

Kd <- 3e6; Kg <- 2e7; I0 <- 0; Ihd <- 1e6; Id <- 2e5
h0f <- 1e-6; d0f <- 1e-6; g0v <- seq(1e-15, 5e-6, length.out = 30)
sim <- tsf:::forward_ida(Kg, Ihd, Id, Kd, h0f, d0f, g0v)
sim$Signal <- sim$Signal + I0
cmp_opti("ida", "ida", sim, c(h0f, d0f, Kd),
  c(1e3, 0, 1e2, 1e-15), c(1e9, 1e4, 1e8, 1e8), Kg, ngen = 1000)

Kd <- 1.7e7; Kg <- 1.8e6; I0 <- 0.0408218; Ihd <- 602000; Id <- 0
h0f <- 1.65e-6; g0f <- 1.32e-6; d0v <- seq(1e-15, 7.5e-6, length.out = 30)
sim <- tsf:::forward_gda(Kd, Kg, Id, Ihd, h0f, g0f, d0v)
sim$Signal <- sim$Signal + I0
cmp_opti("gda", "gda", sim, c(h0f, g0f, Kd),
  c(10, 0, 1e2, 0), c(1e8, 1, 1e7, 1e7), Kg)
