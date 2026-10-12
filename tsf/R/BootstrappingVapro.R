#' Bootstrap uncertainty estimates for a VAPRO fit
#'
#' Performs a parametric (residual) bootstrap around a VAPRO fit obtained with
#' \code{\link{opti_vapro}}: the fitted curve is perturbed with Gaussian noise
#' (per-signal standard deviation estimated from the residuals of the
#' original fit, unless \code{sigma} is supplied), and for each replicate the
#' binding parameter is re-estimated with the same exponential grid search
#' used by \code{\link{opti_vapro}}, with the I0/IHD/ID parameters re-profiled
#' via non-negative least squares. The resulting empirical distribution is
#' summarized by mean, standard deviation and percentile confidence intervals.
#'
#' @export
#' @import rootSolve
#' @importFrom nnls nnls
#' @param case is a character describing which system should be investigated. Either:
#' "dba_host_const", "dba_dye_const", "ida" or "gda".
#' @param lowerBounds is a numeric vector of length 1 defining the lower boundary of the
#'        nonlinear binding parameter.
#' @param upperBounds is a numeric vector of length 1 defining the upper boundary of the
#'        nonlinear binding parameter.
#' @param path is a filepath which contains tabular x-y data, or an already loaded
#'        data.frame. See \code{\link{opti_vapro}}.
#' @param additionalParameters are required parameters which are specific for each case.
#'        See \code{\link{opti_vapro}}.
#' @param nBoot is an optional integer argument defining the number of bootstrap
#'        replicates. The default value is set to 500.
#' @param sigma is an optional numeric argument defining the per-signal noise standard
#'        deviation used to simulate bootstrap data. If NULL (the default) sigma is
#'        estimated from the residuals of the original fit for each signal. A single
#'        numeric value is recycled across all signals.
#' @param nGrid is an optional integer argument defining the number of exponentially
#'        spaced grid points used for the linear search of the nonlinear binding
#'        parameter, both for the original fit and every bootstrap replicate. The
#'        default value is set to 1000.
#' @param seed is an optional integer argument defining the seed used before simulating
#'        the bootstrap data. In case the argument is not set the current time is used
#'        as seed.
#' @param error_calc_fct is an optional input defining how the error between the in
#'        silico signal and the measured signal is calculated. See \code{\link{opti_vapro}}.
#' @param showProgress is an optional logical argument defining whether a text progress
#'        bar tracking the bootstrap replicates is printed. The default value is TRUE.
#' @param engine is an optional character argument selecting the fitting backend: "r"
#'        (the default) uses the plain R/nnls-package grid search, identical to previous
#'        versions of this function. "ast2ast" uses the ast2ast-compiled grid search
#'        (see \code{\link{vapro_ast2ast_bootstrap}}) instead, which is substantially
#'        faster for large nBoot/nGrid since every replicate fit runs compiled instead of
#'        interpreted -- at the cost of only supporting \code{error_calc_fct = "Rel. Error"}
#'        (the compiled loss is hardwired to relative error) and not (yet) returning the
#'        insilico signal columns that the "r" engine's per-replicate evaluation produces.
#' @return a list containing: \code{draws} (a data.frame with one row per successful
#'        bootstrap replicate containing the binding parameter, the linear parameters
#'        and the loss), \code{summary} (mean, sd and 2.5/50/97.5 percentiles per
#'        parameter), \code{sigma} (the noise standard deviation used per signal),
#'        \code{n_success} (number of successful replicates), \code{n_fail} (number of
#'        failed replicates) and \code{seed}.
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
#' opti_vapro_bootstrap("ida", 1e3, 1e9, path, c(4.3, 6.0, 7079458), nBoot = 50)
opti_vapro_bootstrap <- function(case, lowerBounds, upperBounds,
                                 path, additionalParameters,
                                 nBoot = 500L, sigma = NULL,
                                 nGrid = 1000L, seed = NULL,
                                 error_calc_fct = "Rel. Error",
                                 showProgress = TRUE,
                                 engine = c("r", "ast2ast")) {
  engine <- match.arg(engine)
  validation <- tryCatch(expr = {
    if (!is.character(case)) {
      stop("case has to be of type character")
    }
    if (!(case %in% c("dba_dye_const", "dba_host_const", "ida", "gda"))) {
      stop("case is neither dba_dye_const, dba_host_const, ida or gda")
    }
    if (!is.numeric(lowerBounds) || length(lowerBounds) != 1) {
      stop("lowerBounds has to be a numeric vector of length 1")
    }
    if (!is.numeric(upperBounds) || length(upperBounds) != 1) {
      stop("upperBounds has to be a numeric vector of length 1")
    }
    if (upperBounds < lowerBounds) {
      stop("lowerBounds < upperBounds not fulfilled")
    }
    if (lowerBounds <= 0) {
      stop("lowerBounds has to be > 0 since the grid is spaced exponentially")
    }
    if (!is.character(path) && !is.data.frame(path)) {
      stop("path has to be of type character or a data.frame")
    }
    if (!is.numeric(additionalParameters)) {
      stop("additionalParameters have to be of type numeric")
    }
    if (case %in% c("dba_host_const", "dba_dye_const") && length(additionalParameters) != 1) {
      stop("additionalParameters have to be of length 1")
    }
    if (case %in% c("ida", "gda") && length(additionalParameters) != 3) {
      stop("additionalParameters have to be of length 3")
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
    if (engine == "ast2ast" && !identical(error_calc_fct, "Rel. Error")) {
      stop("engine = \"ast2ast\" only supports error_calc_fct = \"Rel. Error\" (the compiled loss is hardwired to relative error)")
    }
  }, error = function(e) {
    return(ErrorClass$new(conditionMessage(e)))
  }, interrupt = function(e) {
    return(ErrorClass$new("Interrupted by user"))
  })
  if (inherits(validation, "ErrorClass")) {
    return(validation)
  }

  if (engine == "ast2ast") {
    return(vapro_ast2ast_bootstrap(
      case = case, lowerBounds = lowerBounds, upperBounds = upperBounds,
      path = path, additionalParameters = additionalParameters,
      nBoot = nBoot, sigma = sigma, nGrid = nGrid, seed = seed,
      showProgress = showProgress
    ))
  }

  if (is.null(seed)) {
    seed <- as.numeric(Sys.time())
  }

  error_fct <- NULL
  if (is.character(error_calc_fct)) {
    error_fct <- get_error_calc_fct(error_calc_fct)
  } else {
    check_error_calc_function(error_calc_fct)
    error_fct <- error_calc_fct
  }

  df <- if (!is.data.frame(path)) importData(path) else path
  numberOfSignals <- ncol(df) - 1

  lossFct <- vapro_loss_fct(case)
  originalEnv <- vapro_build_env(case, df, additionalParameters, error_fct)

  pointEstimate <- vapro_grid_search(lossFct, originalEnv, lowerBounds, upperBounds, nGrid)
  fittedSignals <- as.matrix(pointEstimate$ev[[1]][, seq_len(numberOfSignals), drop = FALSE])
  observedSignals <- as.matrix(originalEnv$signal)
  residuals <- observedSignals - fittedSignals

  if (is.null(sigma)) {
    sigma <- apply(residuals, 2, stats::sd)
  } else if (length(sigma) == 1) {
    sigma <- rep(sigma, numberOfSignals)
  } else if (length(sigma) != numberOfSignals) {
    return(ErrorClass$new("sigma has to be of length 1 or equal to the number of signals"))
  }

  numberOfObservations <- nrow(originalEnv$signal)
  bindingParameterDraws <- rep(NA_real_, nBoot)
  linearParameterDraws <- matrix(NA_real_, nrow = nBoot, ncol = 3 * numberOfSignals)
  lossDraws <- rep(NA_real_, nBoot)
  failureCount <- 0L

  set.seed(seed)
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
    bootstrapEnv <- deep_copy_environment(originalEnv)
    bootstrapEnv$signal <- as.data.frame(fittedSignals + simulatedNoise)

    replicateFit <- tryCatch(
      vapro_grid_search(lossFct, bootstrapEnv, lowerBounds, upperBounds, nGrid),
      error = function(e) NULL
    )
    replicateFailed <- is.null(replicateFit) ||
      !is.finite(replicateFit$loss_hat) ||
      replicateFit$loss_hat >= .Machine$double.xmax
    if (!replicateFailed) {
      bindingParameterDraws[replicate] <- replicateFit$param_hat
      linearParameterDraws[replicate, ] <- replicateFit$ev[[2]]
      lossDraws[replicate] <- replicateFit$loss_hat
    }
    if (replicateFailed) {
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
  lossDraws <- lossDraws[successfulReplicates]

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

  draws <- data.frame(bindingParameterDraws, linearParameterDraws, loss = lossDraws)
  names(draws)[1] <- bindingParameterName

  list(
    draws = draws,
    summary = summaryDf,
    sigma = stats::setNames(sigma, paste0("sig", seq_len(numberOfSignals))),
    n_success = length(bindingParameterDraws),
    n_fail = failureCount,
    seed = seed
  )
}
