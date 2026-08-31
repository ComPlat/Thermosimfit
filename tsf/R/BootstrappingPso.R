#' Parametric bootstrap for a PSO fit
#'
#' Performs a parametric (residual) bootstrap around a PSO fit obtained with
#' \code{\link{opti}}: the fitted curve is perturbed with Gaussian noise (per
#' -signal standard deviation estimated from the residuals of the original
#' fit, unless \code{sigma} is supplied), and for each replicate every
#' parameter is re-estimated by rerunning the particle swarm. Same design as
#' \code{\link{opti_vapro_bootstrap}}: the ast2ast-compiled per-particle loss
#' is translated once and reused for the point estimate and every replicate
#' (re-translating per replicate would dominate the runtime for anything but
#' a handful of replicates). The resulting empirical distribution is
#' summarized by mean, standard deviation and percentile confidence
#' intervals.
#'
#' @export
#' @param case is a character describing which system should be investigated.
#'        Either "dba_host_const", "dba_dye_const", "ida" or "gda".
#' @param lowerBounds is a numeric vector defining the lower boundaries of the
#'        parameter, same as \code{\link{opti}}.
#' @param upperBounds is a numeric vector defining the upper boundaries of the
#'        parameter, same as \code{\link{opti}}.
#' @param path is a filepath which contains tabular x-y data, or an already
#'        loaded data.frame. See \code{\link{opti}}.
#' @param additionalParameters are required parameters which are specific for
#'        each case. See \code{\link{opti}}.
#' @param nBoot is an optional integer argument defining the number of
#'        bootstrap replicates. The default value is set to 500.
#' @param sigma is an optional numeric argument defining the per-signal noise
#'        standard deviation used to simulate bootstrap data. If NULL (the
#'        default) sigma is estimated from the residuals of the original fit
#'        for each signal. A single numeric value is recycled across all
#'        signals.
#' @param npop is an optional integer argument defining the number of
#'        particles, for the original fit and every bootstrap replicate. The
#'        default value is set to 40.
#' @param ngen is an optional integer argument defining the number of
#'        generations, for the original fit and every bootstrap replicate.
#'        The default value is set to 1000.
#' @param seed is an optional integer argument defining the seed used before
#'        simulating the bootstrap data. In case the argument is not set the
#'        current time is used as seed.
#' @param error_calc_fct is an optional input defining how the error between
#'        the in silico signal and the measured signal is calculated. One of
#'        *Rel. Error*, *RMSE*, *SSE*, or *Huber*. The default is
#'        *Rel. Error*.
#' @param showProgress is an optional logical argument defining whether a
#'        text progress bar tracking the bootstrap replicates is printed.
#'        The default value is TRUE.
#' @return a list containing: \code{draws} (a data.frame with one row per
#'        successful bootstrap replicate containing the binding parameter and
#'        the linear parameters), \code{summary} (mean, sd and 2.5/50/97.5
#'        percentiles per parameter), \code{sigma} (the noise standard
#'        deviation used per signal), \code{n_success} (number of successful
#'        replicates), \code{n_fail} (number of failed replicates),
#'        \code{seed} and \code{point_estimate} (the original PSO fit, as
#'        returned by \code{\link{pso}}).
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
#' opti_bootstrap("ida", c(1, 0, 0, 0), c(10^9, 1, 1e5, 1e5), path,
#'   c(4.3, 6.0, 7079458),
#'   nBoot = 20, ngen = 50
#' )
opti_bootstrap <- function(case, lowerBounds, upperBounds, path,
                           additionalParameters, nBoot = 500L, sigma = NULL,
                           npop = 40L, ngen = 1000L, seed = NULL,
                           error_calc_fct = "Rel. Error", showProgress = TRUE) {
  validation <- tryCatch(expr = {
    if (!is.character(case)) {
      stop("case has to be of type character")
    }
    if (!(case %in% c("dba_dye_const", "dba_host_const", "ida", "gda"))) {
      stop("case is neither dba_dye_const, dba_host_const, ida or gda")
    }
    if (!is.numeric(lowerBounds) || length(lowerBounds) < 4) {
      stop("lowerBounds has to be a numeric vector with at least 4 entries")
    }
    if (!is.numeric(upperBounds) || length(upperBounds) < 4) {
      stop("upperBounds has to be a numeric vector with at least 4 entries")
    }
    if (any(upperBounds < lowerBounds)) {
      stop("lowerBounds < upperBounds not fulfilled")
    }
    if (!is.character(path) && !is.data.frame(path)) {
      stop("path has to be of type character or a data.frame")
    }
    if (!is.numeric(additionalParameters)) {
      stop("additionalParameters have to be of type numeric")
    }
    if (!is.numeric(nBoot) && !is.integer(nBoot)) {
      stop("nBoot has to be of type numeric or integer")
    }
    if (nBoot < 1) {
      stop("nBoot has to be at least 1")
    }
    if (!is.null(sigma) && !is.numeric(sigma)) {
      stop("sigma has to be NULL or numeric")
    }
    if (!is.character(error_calc_fct) || length(error_calc_fct) != 1 ||
      !(error_calc_fct %in% c("rel. Error", "Rel. Error", "RMSE", "SSE", "Huber"))) {
      stop('error_calc_fct has to be one of "Rel. Error", "RMSE", "SSE" or "Huber"')
    }
  }, error = function(e) {
    return(ErrorClass$new(conditionMessage(e)))
  }, interrupt = function(e) {
    return(ErrorClass$new("Interrupted by user"))
  })
  if (inherits(validation, "ErrorClass")) {
    return(validation)
  }

  if (is.null(seed)) {
    seed <- as.numeric(Sys.time())
  }
  error_fct <- get_error_calc_fct(error_calc_fct)
  error_code <- pso_error_code(error_calc_fct)

  df <- if (!is.data.frame(path)) importData(path) else path
  numberOfSignals <- ncol(df) - 1L
  numberOfObservations <- nrow(df)

  spec <- pso_a2a_spec(case)
  loss_a2a <- ast2ast::translate(spec$loss_fct, types_f = spec$types_f)
  lossFct <- pso_loss_fct(case)
  env <- vapro_build_env(case, df, additionalParameters, error_fct)
  runAsShiny <- new.env()
  runAsShiny$insilico <- NULL

  originalAddParams <- spec$build_add_params(df, additionalParameters)

  set.seed(seed)
  pointEstimate <- pso(
    env, lowerBounds, upperBounds, lossFct, ngen, npop, -Inf, FALSE, FALSE,
    runAsShiny, "",
    engine = "ast2ast", loss_particle_a2a = loss_a2a,
    add_params = originalAddParams, error_code = error_code
  )
  if (inherits(pointEstimate, "ErrorClass")) {
    return(pointEstimate)
  }
  fittedSignals <- matrix(pointEstimate[[1]]$insilico, nrow = numberOfObservations, ncol = numberOfSignals)
  observedSignals <- as.matrix(df[, -1, drop = FALSE])
  residuals <- observedSignals - fittedSignals

  if (is.null(sigma)) {
    sigma <- apply(residuals, 2, stats::sd)
  } else if (length(sigma) == 1) {
    sigma <- rep(sigma, numberOfSignals)
  } else if (length(sigma) != numberOfSignals) {
    return(ErrorClass$new("sigma has to be of length 1 or equal to the number of signals"))
  }

  bindingParameterDraws <- rep(NA_real_, nBoot)
  linearParameterDraws <- matrix(NA_real_, nrow = nBoot, ncol = 3 * numberOfSignals)
  failureCount <- 0L

  progressBar <- if (showProgress) {
    utils::txtProgressBar(min = 0, max = nBoot, style = 3)
  } else {
    NULL
  }
  for (replicate in seq_len(nBoot)) {
    simulatedNoise <- matrix(
      stats::rnorm(numberOfObservations * numberOfSignals, 0,
        sd = rep(sigma, each = numberOfObservations)
      ),
      nrow = numberOfObservations, ncol = numberOfSignals
    )
    bootstrapDf <- df
    bootstrapDf[, -1] <- fittedSignals + simulatedNoise
    bootstrapAddParams <- spec$build_add_params(bootstrapDf, additionalParameters)

    replicateFit <- tryCatch(
      pso(
        env, lowerBounds, upperBounds, lossFct, ngen, npop, -Inf, FALSE, FALSE,
        runAsShiny, "",
        engine = "ast2ast", loss_particle_a2a = loss_a2a,
        add_params = bootstrapAddParams, error_code = error_code
      ),
      error = function(e) NULL
    )
    replicateFailed <- is.null(replicateFit) || inherits(replicateFit, "ErrorClass")
    if (!replicateFailed) {
      paramVec <- replicateFit[[2]]
      bindingParameterDraws[replicate] <- paramVec[1]
      linearParameterDraws[replicate, ] <- paramVec[-1]
    } else {
      failureCount <- failureCount + 1L
    }
    if (showProgress) {
      utils::setTxtProgressBar(progressBar, replicate)
    }
  }
  if (showProgress) {
    close(progressBar)
  }

  successfulReplicates <- !is.na(bindingParameterDraws)
  bindingParameterDraws <- bindingParameterDraws[successfulReplicates]
  linearParameterDraws <- linearParameterDraws[successfulReplicates, , drop = FALSE]

  linearParameterNames <- as.vector(vapply(seq_len(numberOfSignals), function(signalIndex) {
    paste0(c("I0", "Ihd", "Id"), "_sig", signalIndex)
  }, character(3L)))
  colnames(linearParameterDraws) <- linearParameterNames

  bindingParameterName <- if (case %in% c("dba_host_const", "dba_dye_const")) {
    "Ka(HD) [1/M]"
  } else {
    "Ka(HG) [1/M]"
  }
  percentiles <- function(x) stats::quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE)
  summaryDf <- data.frame(
    param = c(bindingParameterName, linearParameterNames),
    mean = c(mean(bindingParameterDraws), colMeans(linearParameterDraws)),
    sd = c(stats::sd(bindingParameterDraws), apply(linearParameterDraws, 2, stats::sd)),
    q025 = c(percentiles(bindingParameterDraws)[1], apply(linearParameterDraws, 2, function(x) percentiles(x)[1])),
    q500 = c(percentiles(bindingParameterDraws)[2], apply(linearParameterDraws, 2, function(x) percentiles(x)[2])),
    q975 = c(percentiles(bindingParameterDraws)[3], apply(linearParameterDraws, 2, function(x) percentiles(x)[3])),
    row.names = NULL
  )

  draws <- data.frame(bindingParameterDraws, linearParameterDraws)
  names(draws)[1] <- bindingParameterName

  list(
    draws = draws,
    summary = summaryDf,
    sigma = stats::setNames(sigma, paste0("sig", seq_len(numberOfSignals))),
    n_success = length(bindingParameterDraws),
    n_fail = failureCount,
    seed = seed,
    point_estimate = pointEstimate
  )
}

plot_uncertainty_result <- function(df) {
  base_size <- 10
  df$param <- factor(df$param, levels = df$param)
  ggplot(df, aes(x = .data[["param"]], y = .data[["estimate"]])) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), width = 0.2) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~param, scales = "free") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = base_size),
      axis.title = element_text(size = base_size * 1.2),
      strip.text = element_text(size = base_size, face = "bold")
    )
}
