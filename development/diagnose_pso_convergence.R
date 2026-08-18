# Diagnose the r-vs-ast2ast discrepancy reported for opti(engine=...): both
# converge to a high R2 fit but recover different Kd values. Question: is
# this convergence/multimodality (expected -- test_opti_vapro.R already
# tolerates up to 25% Kd error for plain PSO, much looser than VAPRO's 5%),
# or a bug in the synchronous ast2ast update? Capture the printed
# global_best_error trajectory for both engines and check it's smoothly
# decreasing (i.e. actually optimizing) for both, then check whether more
# budget (ngen) narrows the gap to the true Kd for both.
library(tsf)

Kd <- 5e6; I0 <- 25; Ihd <- 8e5; Id <- 1.5e5
d0 <- 1e-4; h0 <- seq(1e-15, 1.5e-4, length.out = 30)
sim <- tsf:::forward_dba_dye_const(Kd, Id, Ihd, d0, h0)
sim$Signal <- sim$Signal + I0
lb <- c(1, 0, 1e2, 1e2); ub <- c(1e8, 1e3, 1e7, 1e7)

run_capture <- function(engine, ngen, seed) {
  set.seed(seed)
  out <- capture.output({
    res <- tsf::opti("dba_dye_const",
      lowerBounds = lb, upperBounds = ub, path = sim, additionalParameters = d0,
      ngen = ngen, seed = seed, engine = engine
    )
  })
  errs <- grep("^\\[1\\] \"[0-9.]+e[+-][0-9]+\"$", out, value = TRUE)
  errs <- as.numeric(gsub('^\\[1\\] "|"$', "", errs))
  list(res = res, trajectory = errs)
}

for (ngen in c(300, 1500)) {
  cat("\n==== ngen =", ngen, "====\n")
  for (engine in c("r", "ast2ast")) {
    r <- run_capture(engine, ngen, 1234)
    traj <- r$trajectory
    traj <- traj[!is.na(traj)]
    cat(sprintf(
      "engine=%-8s K=%.6g (true %.6g, relErr=%.1f%%) R2=%.6f | traj: first=%.4g -> last=%.4g (n=%d), monotone_nonincreasing=%s\n",
      engine, r$res$parameter[[1]], Kd, 100 * abs(r$res$parameter[[1]] - Kd) / Kd,
      r$res$metrices$R2, traj[1], traj[length(traj)], length(traj),
      all(diff(traj) <= 1e-12)
    ))
  }
}
cat("\ndone\n")
