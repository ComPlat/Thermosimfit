filter_best_pso_runs <- function(params, metrices, best_pct = 50) {
  agg <- stats::aggregate(MeanSquareError ~ repetition, data = metrices, FUN = mean)
  agg <- agg[order(agg$MeanSquareError), ]
  keep_n <- max(1L, ceiling(nrow(agg) * best_pct / 100))
  keep_reps <- agg$repetition[seq_len(keep_n)]
  params[params$repetition %in% keep_reps, ]
}

# A fit's marginals all come from the same array, so one ks::kde() call
# covers every parameter instead of one per parameter.
marginal_modes <- function(fit, numPar) {
  dims <- vapply(fit$eval.points, length, integer(1))
  arr <- array(fit$estimate, dim = dims)
  vapply(seq_len(numPar), function(idx) {
    marg <- apply(arr, idx, sum)
    fit$eval.points[[idx]][which.max(marg)]
  }, numeric(1))
}

marginal_densities <- function(fit, numPar) {
  dims <- vapply(fit$eval.points, length, integer(1))
  arr <- array(fit$estimate, dim = dims)
  lapply(seq_len(numPar), function(idx) {
    marg <- apply(arr, idx, sum)
    data.frame(x = fit$eval.points[[idx]], y = marg / max(marg))
  })
}

#' Joint kernel-density bootstrap for repeated PSO fits
#'
#' Repeated PSO runs trace out an empirical distribution for each fitted
#' parameter. This estimates, for every parameter, the kernel-density mode
#' of that joint distribution and a bootstrap confidence interval on that
#' mode: the joint kernel density is fit once on the parameter estimates,
#' then re-fit on \code{n_boot} resamples (with replacement) of those same
#' estimates to build the mode's bootstrap distribution.
#'
#' @export
#' @param df is a data.frame with one row per PSO repetition and one column
#'        per core parameter (the binding constant plus I0/IHD/ID per
#'        signal), with any bookkeeping columns (repetition/dataset) already
#'        dropped. See \code{\link{pso_uncertainty_batch}} for computing this
#'        directly off a Batch-PSO result.
#' @param n_boot is the number of bootstrap resamples used to build the
#'        confidence interval for each parameter's kernel-density mode.
#'        The default value is set to 1000.
#' @param gridsize is the number of grid points per dimension used to
#'        evaluate the kernel density (passed to \code{ks::kde}); the mode is
#'        read off this grid, so it also bounds the mode's resolution. Cost
#'        scales with \code{gridsize^ncol(df)}. The default value is set to
#'        15.
#' @param seed is an optional integer argument defining the seed used before
#'        bootstrapping. In case the argument is not set the current time is
#'        used as seed.
#' @return a list containing: \code{mode} (named numeric vector, one entry per
#'         parameter), \code{lower_ci} and \code{upper_ci} (the same, 95%
#'         bootstrap interval), \code{density} (a named list of data.frames,
#'         one per parameter, with columns \code{x}/\code{y} for plotting the
#'         marginal of the joint kernel density) and \code{n_used} (the
#'         number of repetitions the joint density was fit on).
#' @examples
#' df <- data.frame(
#'   kG = rnorm(30, 1e7, 1e6),
#'   I0 = rnorm(30, 0, 0.01),
#'   IHD = rnorm(30, 1e5, 1e4),
#'   ID = rnorm(30, 100, 10)
#' )
#' pso_uncertainty(df, n_boot = 50)
pso_uncertainty <- function(df, n_boot = 1000L, gridsize = 15L, seed = NULL) {
  if (!is.data.frame(df)) {
    return(ErrorClass$new("df has to be of type data.frame"))
  }
  if (nrow(df) < 2) {
    return(ErrorClass$new("df needs at least 2 rows (repeated PSO runs) to estimate uncertainty"))
  }
  if (!is.numeric(n_boot) && !is.integer(n_boot)) {
    return(ErrorClass$new("n_boot has to be of type numeric or integer"))
  }
  if (n_boot < 1) {
    return(ErrorClass$new("n_boot has to be at least 1"))
  }
  if (is.null(seed)) {
    seed <- as.numeric(Sys.time())
  }
  set.seed(seed)

  numPar <- ncol(df)
  grid <- rep(gridsize, numPar)
  fitted <- tryCatch({
    H <- ks::Hpi(as.matrix(df))
    list(H = H, fit = ks::kde(df, H = H, gridsize = grid))
  }, error = function(e) {
    ErrorClass$new(paste(
      "Could not fit the joint kernel density (bandwidth selection failed:",
      conditionMessage(e),
      "). This usually means too few repeated PSO runs, or runs that landed",
      "on near-identical parameter values - try increasing the number of",
      "repetitions, lowering the best-by-error percentage kept, or widening",
      "the PSO search (npop/ngen)."
    ))
  })
  if (inherits(fitted, "ErrorClass")) {
    return(fitted)
  }
  H <- fitted$H
  fit <- fitted$fit

  modes <- matrix(NA_real_, nrow = n_boot, ncol = numPar)
  failures <- 0L
  for (i in seq_len(n_boot)) {
    boot <- df[sample.int(nrow(df), replace = TRUE), , drop = FALSE]
    fitBoot <- tryCatch(ks::kde(boot, H = H, gridsize = grid), error = function(e) NULL)
    if (is.null(fitBoot)) {
      failures <- failures + 1L
      next
    }
    modes[i, ] <- marginal_modes(fitBoot, numPar)
  }
  if (failures == n_boot) {
    return(ErrorClass$new("All bootstrap resamples failed to fit a kernel density"))
  }
  ci <- apply(modes, 2, stats::quantile, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)

  list(
    mode = stats::setNames(marginal_modes(fit, numPar), names(df)),
    lower_ci = stats::setNames(ci[1, ], names(df)),
    upper_ci = stats::setNames(ci[2, ], names(df)),
    density = stats::setNames(marginal_densities(fit, numPar), names(df)),
    n_used = nrow(df),
    seed = seed
  )
}

#' PSO uncertainty from a Batch-PSO result
#'
#' Turns a Batch-PSO result (repeated PSO fits, possibly across several
#' uploaded files/datasets) into the joint-kernel-density bootstrap of
#' \code{\link{pso_uncertainty}}. Each dataset's repetitions are first
#' ranked by mean squared error and the best \code{best_pct}% are kept (so a
#' dataset with worse PSO convergence doesn't get outvoted purely on raw MSE
#' terms) - this ranking/filtering happens separately per dataset, before
#' anything is combined; the survivors from every dataset are then pooled
#' into one joint distribution. Pooling across datasets is what captures
#' actual measurement noise (each file is an independent noisy replicate)
#' rather than only PSO's own run-to-run convergence variability - so this
#' is only meaningful when the batch has multiple files uploaded as
#' replicate measurements of the same system; with a single dataset it
#' still runs, but only reflects optimizer variability.
#'
#' @export
#' @param values is the batch result list as produced by \code{\link{batch}}
#'        or the Batch-PSO tab (i.e. an object with \code{$params} and
#'        \code{$metrices}, each a list with one entry per PSO repetition).
#' @param best_pct is the percentage (0-100) of best-by-error repetitions to
#'        keep per dataset before pooling. The default value is set to 50.
#' @param n_boot is passed through to \code{\link{pso_uncertainty}}. The
#'        default value is set to 1000.
#' @param gridsize is passed through to \code{\link{pso_uncertainty}}.
#' @param seed is passed through to \code{\link{pso_uncertainty}}.
#' @return see \code{\link{pso_uncertainty}}.
pso_uncertainty_batch <- function(values, best_pct = 50, n_boot = 1000L, gridsize = 15L, seed = NULL) {
  if (!is.list(values) || is.null(values$params) || is.null(values$metrices)) {
    return(ErrorClass$new("values has to be a batch result with $params and $metrices"))
  }
  if (!is.numeric(best_pct) || best_pct <= 0 || best_pct > 100) {
    return(ErrorClass$new("best_pct has to be a number in (0, 100]"))
  }
  params <- Reduce(rbind, values$params)
  metrices <- Reduce(rbind, values$metrices)
  if (!is.data.frame(params) || !is.data.frame(metrices)) {
    return(ErrorClass$new("values contains no successful repetitions"))
  }

  # Filtered independently per dataset (not across the pooled table) so a
  # dataset with worse PSO convergence can't be outvoted by a better one on
  # raw MSE terms before pooling.
  best <- lapply(unique(params$dataset), function(d) {
    filter_best_pso_runs(
      params[params$dataset == d, ],
      metrices[metrices$dataset == d, ],
      best_pct = best_pct
    )
  })
  pooled <- Reduce(rbind, best)
  core <- pooled[, !(names(pooled) %in% c("repetition", "dataset")), drop = FALSE]

  pso_uncertainty(core, n_boot = n_boot, gridsize = gridsize, seed = seed)
}
