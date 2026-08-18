build_add_params_ida_a2a <- function(df, additionalParameters) {
  names(df)[1] <- "guest"
  ga <- df[, 1]
  signal_df <- df[, -1] |> as.data.frame()
  n_sigs <- as.integer(ncol(signal_df))
  signal_flat <- as.vector(as.matrix(signal_df)) # column-major, matches R's own layout

  structure(
    list(
      kd = additionalParameters[3],
      h0 = additionalParameters[1],
      d0 = additionalParameters[2],
      ga = ga,
      n_sigs = n_sigs,
      signal = signal_flat
    ),
    class = "AddParamsIda"
  )
}

# Shared by both dba_dye_const (host varies, dye/d0 fixed) and
# dba_host_const (dye varies, host/h0 fixed) -- same underlying equation
# (see types_f_dba's header comment in lossFunctions_ast2ast.R), only the
# fixed quantity is recycled to the varying one's length here, mirroring
# solve_h_dba()'s own rep_len() in DerivedForwardEquations.R.
build_add_params_dba_a2a <- function(df, additionalParameters, case) {
  n <- nrow(df)
  if (case == "dba_dye_const") {
    names(df)[1] <- "host"
    h0 <- df[, 1]
    d0 <- rep_len(additionalParameters[1], n)
  } else {
    names(df)[1] <- "dye"
    d0 <- df[, 1]
    h0 <- rep_len(additionalParameters[1], n)
  }
  signal_df <- df[, -1] |> as.data.frame()
  n_sigs <- as.integer(ncol(signal_df))
  signal_flat <- as.vector(as.matrix(signal_df))

  structure(
    list(
      h0 = h0,
      d0 = d0,
      n_sigs = n_sigs,
      signal = signal_flat
    ),
    class = "AddParamsDba"
  )
}

# GDA sweeps dye (d0) at fixed host (h0) and guest (ga0) -- IDA's mirror
# image, see types_f_gda's header comment in lossFunctions_ast2ast.R.
build_add_params_gda_a2a <- function(df, additionalParameters) {
  names(df)[1] <- "dye"
  dye <- df[, 1]
  signal_df <- df[, -1] |> as.data.frame()
  n_sigs <- as.integer(ncol(signal_df))
  signal_flat <- as.vector(as.matrix(signal_df))

  structure(
    list(
      kd = additionalParameters[3],
      h0 = additionalParameters[1],
      d0 = dye,
      g = additionalParameters[2],
      n_sigs = n_sigs,
      signal = signal_flat
    ),
    class = "AddParamsGda"
  )
}

# Dispatch table mirroring vapro_loss_fct()/vapro_build_env() (OptimizeVapro.R)
# for the ast2ast-compiled side: which types_f/loss/grid-search functions and
# add_params builder apply to a given `case`.
vapro_a2a_spec <- function(case) {
  if (case %in% c("dba_dye_const", "dba_host_const")) {
    list(
      types_f = types_f_dba,
      args_f_loss = args_f_loss_dba,
      loss_fct = loss_fct_dba_a2a,
      args_f_grid = args_f_grid_search_dba,
      grid_fct = grid_search_dba_a2a,
      build_add_params = function(df, additionalParameters) {
        build_add_params_dba_a2a(df, additionalParameters, case)
      },
      bound_name = "Ka(HD) [1/M]"
    )
  } else if (case == "ida") {
    list(
      types_f = types_f_ida,
      args_f_loss = args_f_loss_ida,
      loss_fct = loss_fct_ida_a2a,
      args_f_grid = args_f_grid_search_ida,
      grid_fct = grid_search_ida_a2a,
      build_add_params = build_add_params_ida_a2a,
      bound_name = "Ka(HG) [1/M]"
    )
  } else if (case == "gda") {
    list(
      types_f = types_f_gda,
      args_f_loss = args_f_loss_gda,
      loss_fct = loss_fct_gda_a2a,
      args_f_grid = args_f_grid_search_gda,
      grid_fct = grid_search_gda_a2a,
      build_add_params = build_add_params_gda_a2a,
      bound_name = "Ka(HG) [1/M]"
    )
  } else {
    stop("case has to be one of dba_dye_const, dba_host_const, ida or gda")
  }
}

vapro_grid_search_a2a <- function(loss_a2a, add_params, lowerBounds, upperBounds, nGrid) {
  exponentialGrid <- exp(seq(log(lowerBounds), log(upperBounds), length.out = nGrid))
  errorsPerGridPoint <- vapply(exponentialGrid, function(candidateParameter) {
    loss_a2a(candidateParameter, add_params)
  }, numeric(1L))
  bestIndex <- which.min(errorsPerGridPoint)

  neighborhoodLower <- exponentialGrid[max(bestIndex - 1L, 1L)]
  neighborhoodUpper <- exponentialGrid[min(bestIndex + 1L, nGrid)]
  localRefinement <- stats::optimize(
    function(candidateParameter) loss_a2a(candidateParameter, add_params),
    interval = c(neighborhoodLower, neighborhoodUpper)
  )

  refinementImproved <- localRefinement$objective <= errorsPerGridPoint[bestIndex]
  list(
    param_hat = if (refinementImproved) localRefinement$minimum else exponentialGrid[bestIndex],
    loss_hat = if (refinementImproved) localRefinement$objective else errorsPerGridPoint[bestIndex]
  )
}

#' ast2ast-compiled VAPRO fit, alongside the plain-R fit, for every case
#'
#' Same case dispatch as \code{\link{opti_vapro}} (dba_dye_const,
#' dba_host_const, ida or gda), but additionally runs the ast2ast-compiled
#' grid search and returns both results side by side -- meant for validating
#' the compiled path agrees with the R/nnls-package one, not (yet) as the
#' primary entry point. error_calc_fct is not configurable here: the
#' compiled loss is hardwired to relative error, matching the R side's
#' default.
#'
#' @param case one of "dba_dye_const", "dba_host_const", "ida" or "gda".
#' @param lowerBounds,upperBounds bounds for the nonlinear binding parameter
#'        (Kd or Kga depending on case).
#' @param path file path or already-loaded data.frame, same as opti_vapro.
#' @param additionalParameters same as opti_vapro.
#' @param nGrid number of exponential grid points (default 1000).
#' @return a list with \code{r} (plain-R fit, as returned by
#'         vapro_grid_search) and \code{a2a} (ast2ast fit).
opti_vapro_ast2ast <- function(case, lowerBounds, upperBounds,
                                path, additionalParameters,
                                nGrid = 1000L) {
  spec <- vapro_a2a_spec(case)
  df <- if (!is.data.frame(path)) importData(path) else path

  # ---- R / nnls-package approach -----------------------------------------
  env <- vapro_build_env(case, df, additionalParameters, get_error_calc_fct("Rel. Error"))
  r_fit <- vapro_grid_search(vapro_loss_fct(case), env, lowerBounds, upperBounds, nGrid)

  # ---- ast2ast approach ---------------------------------------------------
  add_params <- spec$build_add_params(df, additionalParameters)
  loss_a2a <- ast2ast::translate(spec$loss_fct, args_f = spec$args_f_loss, types_f = spec$types_f)
  a2a_fit <- vapro_grid_search_a2a(loss_a2a, add_params, lowerBounds, upperBounds, nGrid)

  list(r = r_fit, a2a = a2a_fit)
}

#' Parametric bootstrap for a VAPRO fit, using the ast2ast-compiled grid
#' search for every fit (point estimate + all nBoot replicates), for any of
#' the four cases.
#'
#' Same design as opti_vapro_bootstrap(): fit once, estimate per-signal
#' noise sigma from the residuals of that fit (unless sigma is supplied),
#' then for each replicate simulate new Gaussian noise around the fitted
#' curve and refit. Noise generation stays in R -- ast2ast has no RNG, and
#' reproducibility (set.seed()) depends on R's own RNG algorithm anyway, so
#' there'd be nothing to gain compiling that part. Every *fit* (the coarse
#' grid search over the nonlinear binding parameter, root-find + NNLS per
#' grid point) runs compiled.
#'
#' @param case one of "dba_dye_const", "dba_host_const", "ida" or "gda".
#' @param lowerBounds,upperBounds bounds for the nonlinear binding parameter.
#' @param path file path or already-loaded data.frame, same as opti_vapro.
#' @param additionalParameters same as opti_vapro.
#' @param nBoot number of bootstrap replicates (default 500).
#' @param sigma per-signal noise sd; NULL (default) estimates it from the
#'        residuals of the original fit. A single value is recycled.
#' @param nGrid number of exponential grid points, for the original fit and
#'        every replicate (default 1000).
#' @param seed RNG seed; if NULL the current time is used.
#' @param showProgress print a text progress bar (default TRUE).
#' @return a list: draws (one row per successful replicate), summary
#'         (mean/sd/2.5%/50%/97.5% per parameter), sigma, n_success, n_fail,
#'         seed, point_estimate (the original GridResult).
vapro_ast2ast_bootstrap <- function(case, lowerBounds, upperBounds,
                                     path, additionalParameters,
                                     nBoot = 500L, sigma = NULL,
                                     nGrid = 1000L, seed = NULL,
                                     showProgress = TRUE) {
  if (is.null(seed)) {
    seed <- as.numeric(Sys.time())
  }
  nGrid <- as.integer(nGrid)

  spec <- vapro_a2a_spec(case)
  df <- if (!is.data.frame(path)) importData(path) else path
  numberOfSignals <- ncol(df) - 1L
  numberOfObservations <- nrow(df)

  grid_a2a <- ast2ast::translate(spec$grid_fct, args_f = spec$args_f_grid, types_f = spec$types_f)

  originalAddParams <- spec$build_add_params(df, additionalParameters)

  pointEstimate <- grid_a2a(lowerBounds, upperBounds, nGrid, originalAddParams)

  fittedSignals <- matrix(pointEstimate$fitted_signal, nrow = numberOfObservations, ncol = numberOfSignals)
  observedSignals <- matrix(originalAddParams$signal, nrow = numberOfObservations, ncol = numberOfSignals)
  residuals <- observedSignals - fittedSignals

  if (is.null(sigma)) {
    sigma <- apply(residuals, 2, stats::sd)
  } else if (length(sigma) == 1) {
    sigma <- rep(sigma, numberOfSignals)
  } else if (length(sigma) != numberOfSignals) {
    stop("sigma has to be of length 1 or equal to the number of signals")
  }

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
    simulatedSignal <- fittedSignals + simulatedNoise

    bootstrapAddParams <- originalAddParams
    bootstrapAddParams$signal <- as.vector(simulatedSignal)

    replicateFit <- tryCatch(
      grid_a2a(lowerBounds, upperBounds, nGrid, bootstrapAddParams),
      error = function(e) NULL
    )
    replicateFailed <- is.null(replicateFit) ||
      !is.finite(replicateFit$loss_hat) ||
      replicateFit$loss_hat >= .Machine$double.xmax
    if (!replicateFailed) {
      bindingParameterDraws[replicate] <- replicateFit$param_hat
      linearParameterDraws[replicate, ] <- replicateFit$betas
      lossDraws[replicate] <- replicateFit$loss_hat
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
  lossDraws <- lossDraws[successfulReplicates]

  linearParameterNames <- as.vector(vapply(seq_len(numberOfSignals), function(signalIndex) {
    paste0(c("I0", "Ihd", "Id"), "_sig", signalIndex)
  }, character(3L)))
  colnames(linearParameterDraws) <- linearParameterNames

  percentiles <- function(x) stats::quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE)
  summaryDf <- data.frame(
    param = c(spec$bound_name, linearParameterNames),
    mean = c(mean(bindingParameterDraws), colMeans(linearParameterDraws)),
    sd = c(stats::sd(bindingParameterDraws), apply(linearParameterDraws, 2, stats::sd)),
    q025 = c(percentiles(bindingParameterDraws)[1], apply(linearParameterDraws, 2, function(x) percentiles(x)[1])),
    q500 = c(percentiles(bindingParameterDraws)[2], apply(linearParameterDraws, 2, function(x) percentiles(x)[2])),
    q975 = c(percentiles(bindingParameterDraws)[3], apply(linearParameterDraws, 2, function(x) percentiles(x)[3])),
    row.names = NULL
  )

  draws <- data.frame(bindingParameterDraws, linearParameterDraws, loss = lossDraws)
  names(draws)[1] <- spec$bound_name

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
