vapro_loss_fct <- function(case) {
  if (case == "dba_host_const") {
    lossFctHGVapro
  } else if (case == "dba_dye_const") {
    lossFctDBAVapro
  } else if (case == "ida") {
    lossFctIDAVapro
  } else if (case == "gda") {
    lossFctGDAVapro
  }
}

vapro_build_env <- function(case, df, additionalParameters, error_fct) {
  env <- new.env()
  env$error_calc_fct <- error_fct
  env$n_sigs <- ncol(df) - 1
  if (case == "dba_host_const") {
    names(df)[1] <- "dye"
    env$dye <- df[, 1]
    env$signal <- df[, -1] |> as.data.frame()
    env$h0 <- additionalParameters[1]
  } else if (case == "dba_dye_const") {
    names(df)[1] <- "host"
    env$host <- df[, 1]
    env$signal <- df[, -1] |> as.data.frame()
    env$d0 <- additionalParameters[1]
  } else if (case == "ida") {
    names(df)[1] <- "guest"
    env$ga <- df[, 1]
    env$signal <- df[, -1] |> as.data.frame()
    env$h0 <- additionalParameters[1]
    env$d0 <- additionalParameters[2]
    env$kd <- additionalParameters[3]
  } else if (case == "gda") {
    names(df)[1] <- "dye"
    env$dye <- df[, 1]
    env$signal <- df[, -1] |> as.data.frame()
    env$h0 <- additionalParameters[1]
    env$ga0 <- additionalParameters[2]
    env$kd <- additionalParameters[3]
  }
  env
}

deep_copy_environment <- function(env) {
  list2env(as.list(env, all.names = TRUE), envir = new.env(parent = emptyenv()))
}

vapro_grid_search <- function(lossFct, env, lowerBounds, upperBounds, nGrid) {
  exponentialGrid <- exp(seq(log(lowerBounds), log(upperBounds), length.out = nGrid))
  errorsPerGridPoint <- vapply(exponentialGrid, function(candidateParameter) {
    lossFct(candidateParameter, env, eval = FALSE)
  }, numeric(1L))
  bestIndex <- which.min(errorsPerGridPoint)

  neighborhoodLower <- exponentialGrid[max(bestIndex - 1L, 1L)]
  neighborhoodUpper <- exponentialGrid[min(bestIndex + 1L, nGrid)]
  localRefinement <- stats::optimize(
    function(candidateParameter) lossFct(candidateParameter, env, eval = FALSE),
    interval = c(neighborhoodLower, neighborhoodUpper)
  )

  refinementImproved <- localRefinement$objective <= errorsPerGridPoint[bestIndex]
  bindingParameterEstimate <- if (refinementImproved) {
    localRefinement$minimum
  } else {
    exponentialGrid[bestIndex]
  }
  lossEstimate <- if (refinementImproved) {
    localRefinement$objective
  } else {
    errorsPerGridPoint[bestIndex]
  }

  evaluation <- lossFct(bindingParameterEstimate, env, eval = TRUE)
  list(
    param_hat = bindingParameterEstimate,
    loss_hat = lossEstimate,
    ev = evaluation
  )
}

#' Optimize algebraic systems which describe thermodynamic binding systems using
#' the VAPRO (variable projection) approach
#'
#' Compared to \code{\link{opti}} only the nonlinear binding parameter
#' (Ka(HD) or Ka(HG)) is searched, via a linear grid search over the log
#' (i.e. exponentially spaced) parameter space, followed by a local
#' continuous refinement (\code{\link[stats]{optimize}}) restricted to the
#' bracket around the best grid point. The coarse grid keeps the search
#' robust against multiple local optima over the wide parameter range, while
#' the local refinement avoids the discretization artifacts a pure grid
#' search would otherwise introduce (identical estimates across many
#' bootstrap replicates whenever the true parameter uncertainty is smaller
#' than the grid spacing). The linear intercept/slope parameters (I0, IHD,
#' ID) for each signal are not part of the search space.
#' Instead they are profiled out in closed form for every candidate binding
#' parameter using non-negative least squares (\code{\link[nnls]{nnls}}).
#'
#' @export
#' @import rootSolve
#' @import ggplot2
#' @import patchwork
#' @importFrom nnls nnls
#' @param case is a character describing which system should be investigated. Either:
#' "dba_host_const",
#' "dba_dye_const", "ida" or "gda".
#' @param lowerBounds is a numeric vector of length 1 defining the lower boundary of the
#'        nonlinear binding parameter. In case of *dba_dye_const* or *dba_host_const*
#'        this is *khd*. In case of *ida* and *gda* this is *kg*.
#' @param upperBounds is a numeric vector of length 1 defining the upper boundary of the
#'        nonlinear binding parameter. The order is the same as for the lower boundary.
#' @param path is a filepath which contains tabular x-y data.
#'        The concentraion of dye or guest respectivly is assumed to be in the first column.
#'        Furthermore, should the corresponding signals be stored in the other columns.
#'        As an alternative an already loaded data.frame can be passed to the function.
#' @param additionalParameters are required parameters which are specific for each case.
#'        In case of *dba_host_const* a numeric vector of length 1 is expected which contains the concentration of the host.
#'        In case of *dba_dye_const* a numeric vector of length 1 is expected which contains the concentration of the dye.
#'        In case of *ida* a numeric vector of length 3 is expected which contains the concentration of the host, dye and the *khd* parameter.
#'        In case of *gda* a numeric vector of length 3 is expected which contains the concentration of the host, guest and the *khd* parameter.
#' @param nGrid is an optional integer argument defining the number of exponentially spaced grid points used for the linear search of the nonlinear binding parameter between lowerBounds and upperBounds. The default value is set to 1000.
#' @param error_calc_fct is an optional input defining how the error between the in silico signal and the measured signal is calculated.
#'        One can use one of the following predefined functions as character vectors: *Rel. Error*, *RMSE*, *SSE*, or *Huber*. The default function is *Rel. Error*.
#'        Alternatively a function can be passed to opti_vapro, which has to expect two arguments, first the insilico signal followed by the measured signal.
#' @return either an instance of ErrorClass if something went wrong. Otherwise the optimized parameter and the *insilico* signal values are returned.
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
#' opti_vapro("ida", 1, 10^9, path, c(4.3, 6.0, 7079458))
opti_vapro <- function(case, lowerBounds, upperBounds,
                       path, additionalParameters,
                       nGrid = 1000L,
                       error_calc_fct = "Rel. Error") {
  validation <- tryCatch(expr = {
    if (!is.character(case)) {
      stop("case has to be of type character")
    }
    if (!(case %in% c("dba_dye_const", "dba_host_const", "ida", "gda"))) {
      stop("case is neither dba_dye_const, dba_host_const, ida or gda")
    }
    if (!is.numeric(lowerBounds)) {
      stop("lowerBounds have to be of type numeric")
    }
    if (length(lowerBounds) != 1) {
      stop("lowerBounds vector has to be of length 1")
    }
    if (!is.numeric(upperBounds)) {
      stop("upperBounds have to be of type numeric")
    }
    if (length(upperBounds) != 1) {
      stop("upperBounds vector has to be of length 1")
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
    if (case == "ida" && length(additionalParameters) != 3) {
      stop("additionalParameters have to be of length 3")
    }
    if (case == "gda" && length(additionalParameters) != 3) {
      stop("additionalParameters have to be of length 3")
    }
    if (!is.numeric(nGrid) && !is.integer(nGrid)) {
      stop("nGrid has to be of type numeric or integer")
    }
    if (nGrid < 2) {
      stop("nGrid has to be at least 2")
    }
  }, error = function(e) {
    return(ErrorClass$new(conditionMessage(e)))
  }, interrupt = function(e) {
    return(ErrorClass$new("Interrupted by user"))
  })
  if (inherits(validation, "ErrorClass")) {
    return(validation)
  }

  error_fct <- NULL
  error_fct_name <- NULL
  if (is.character(error_calc_fct)) {
    error_fct_name <- error_calc_fct
    error_fct <- get_error_calc_fct(error_calc_fct)
  } else {
    error_fct_name <- "User defined function"
    check_error_calc_function(error_calc_fct)
    error_fct <- error_calc_fct
  }

  df <- tryCatch(
    expr = {
      if (!is.data.frame(path)) {
        importData(path)
      } else {
        path
      }
    },
    error = function(e) {
      return(NULL)
    },
    interrupt = function(e) {
      return(NULL)
    }
  )
  n_sigs <- ncol(df) - 1

  lossFct <- vapro_loss_fct(case)
  env <- vapro_build_env(case, df, additionalParameters, error_fct)

  tryCatch(
    {
      fit <- vapro_grid_search(lossFct, env, lowerBounds, upperBounds, nGrid)
      res <- list(fit$ev[[1]], c(fit$param_hat, fit$ev[[2]]))

      params <- create_params_df(res, case, n_sigs)
      df <- create_data_df(df, res, case, n_sigs)
      bound_name <- if (case %in% c("dba_host_const", "dba_dye_const")) {
        "Ka(HD) [1/M]"
      } else {
        "Ka(HG) [1/M]"
      }
      lowerBounds <- stats::setNames(data.frame(lowerBounds), bound_name)
      upperBounds <- stats::setNames(data.frame(upperBounds), bound_name)
      additionalParameters <- correct_names_additional_param(
        additionalParameters, case
      )
      signal_plots <- plot_signals(df, case, n_sigs)
      d_hd_plot <- plot_d_hd(df, case, n_sigs)
      return(list(
        data = df, parameter = params,
        signal_plots = signal_plots,
        d_hd_plot = d_hd_plot,
        metrices = metrices(df, error_fct_name, n_sigs),
        loss = fit$loss_hat,
        additionalParameters = additionalParameters,
        lowerBounds = lowerBounds, upperBounds = upperBounds,
        nGrid = nGrid,
        n_sigs = n_sigs
      ))
    },
    error = function(e) {
      stop(conditionMessage(e))
    }
  )
}
