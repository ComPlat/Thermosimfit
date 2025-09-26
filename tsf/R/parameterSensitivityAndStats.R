# TODO: update the plot and the text

`%||%` <- function(a, b) if (!is.null(a)) a else b

kde4d_intern <- function(df, n_samp = 20000) {
  stopifnot(ncol(df) == 4)
  colnames(df) <- colnames(df) %||% paste0("V", seq_len(ncol(df)))
  H   <- ks::Hpi(as.matrix(df))
  fit <- ks::kde(as.matrix(df), H = H)
  draws <- as.data.frame(ks::rkde(n = n_samp, fhat = fit))
  colnames(draws) <- colnames(df)
  marginals <- lapply(seq_len(ncol(draws)), function(j) {
    d <- density(draws[[j]], n = 512)
    data.frame(x = d$x, y = d$y)
  })
  names(marginals) <- colnames(draws)
  list(fit = fit, draws = draws, marginals = marginals)
}
kde4d_with_smr <- function(df, prob = 0.95, n_samp = 20000) {
  stopifnot(ncol(df) == 4)
  ki   <- kde4d_intern(df, n_samp = n_samp)
  fit  <- ki$fit
  Xs   <- ki$draws
  grid <- expand.grid(fit$eval.points)
  dens <- as.vector(fit$estimate)
  mode <- as.numeric(grid[which.max(dens), ])
  names(mode) <- colnames(df)
  alpha <- (1 - prob) / 2
  qs <- t(apply(Xs, 2, quantile, probs = c(alpha, 1 - alpha), na.rm = TRUE))
  colnames(qs) <- c("lower", "upper")
  rownames(qs) <- colnames(df)
  list(
    mode     = mode,
    lower_ci = qs[, "lower"],
    upper_ci = qs[, "upper"],
    df       = ki$marginals
  )
}

# # NOTE: Using Contour Levels for Significant Model Regions
# # Significant Model Regions (SMR)
# kde4d_intern <- function(df) {
#   mins <- apply(df, 2, min)
#   maxs <- apply(df, 2, max)
#   res <- ks::kde(df, xmin = mins, xmax = maxs)
#   grid_points <- expand.grid(res$eval.points)
#   joint_densities <- as.vector(res$estimate)
#   density_data <- cbind(grid_points, joint_density = joint_densities)
#   return(density_data)
# }

# kde4d_with_smr <- function(df, prob = 0.95) {
#   df <- df[, 1:4]
#   res <- ks::kde(df)
#   level <- paste0(prob * 100, "%")
#   density_threshold <- res$cont[level]
#   grid_points <- expand.grid(res$eval.points)
#   densities <- as.vector(res$estimate)
#   significant_points <- grid_points[densities >= density_threshold, ]
#   mode_index <- which.max(densities)
#   mode <- grid_points[mode_index, ]
#   mode <- ifelse(mode < 0, 0, mode) |> as.numeric()
#   CIs <- apply(significant_points, 2, range)
#   lc <- CIs[1, ]
#   lc <- ifelse(lc < 0, 0, lc)
#   uc <- CIs[2, ]
#   uc <- ifelse(uc < 0, 0, uc)
#   res <- kde4d_intern(df)
#   df <- lapply(1:4, function(x) {
#     i <- parent.frame()$i[]
#     data.frame(x = res[, i], y = res[, 5])
#   })
#   return(list(
#     mode = mode,
#     lower_ci = lc,
#     upper_ci = uc,
#     df = df
#   ))
# }

jkd <- function(df) {
  res <- kde4d_with_smr(df)
  res$mode
  res$lower_ci
  res$upper_ci
  res <- lapply(1:4, function(idx) {
    mode <- res$mode[idx]
    l <- res$lower_ci[idx]
    u <- res$upper_ci[idx]
    df_temp <- data.frame(
      values = c(mode, l, u),
      type = c("mode", "lower", "upper")
    )
    names(df_temp)[1] <- names(df)[idx]
    return(df_temp)
  })
  res <- lapply(res, function(x) {
    x[, 1]
  })
  res <- Reduce(rbind, res) |> as.data.frame()
  res <- cbind(names(df)[1:4], res)
  names(res) <- c("Parameter", "mode", "lower", "upper")
  row.names(res) <- NULL
  return(res)
}

make_joint_sampler_kde <- function(df, lb, ub) {
  eps <- 1e-6
  p <- ncol(df)
  to_unit <- function(X) {
    U <- sweep(X, 2, lb, "-")
    U <- sweep(U, 2, (ub - lb), "/")
    pmin(pmax(U, eps), 1 - eps)
  }
  to_param <- function(U) {
    U <- sweep(U, 2, (ub - lb), "*")
    sweep(U, 2, lb, "+")
  }
  U <- to_unit(as.matrix(df))
  Z <- qlogis(U)
  kde_obj <- ks::kde(Z)
  function(n) {
    Znew <- ks::rkde(n = n, fhat = kde_obj)
    Unew <- plogis(Znew)
    Xnew <- to_param(Unew)
    Xdf <- as.data.frame(Xnew)
    colnames(Xdf) <- colnames(df)
    Xdf
  }
}
sobolVariance_dep <- function(parameter_df, lossFct, env, lb, ub, parameterNames, runAsShiny) {
  n <- 1000
  nboot <- 100
  joint_sampler <- make_joint_sampler_kde(parameter_df, lb, ub)
  X <- joint_sampler(n)
  names(X) <- parameterNames
  sobolFun <- function(X) {
    p <- NULL
    if (is.data.frame(X) || is.matrix(X)) {
      return(sapply(1:nrow(X), function(x) {
        lossFct(as.numeric(X[x, ]), env, FALSE)
      }))
    } else {
      p <- as.numeric(X)
    }
    lossFct(p, env, FALSE)
  }
  sh <- shapleysobol_knn(model = sobolFun, X = X, nboot = nboot)
  ggplot(sh) +
    theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 8)) +
    ylab("Explained fraction of variance (Shapley effects)")
}

# Monte Carlo Estimation of Sobol’ Indices
sobolVariance <- function(lossFct, env, lb, ub, parameterNames, runAsShiny) {
  n <- 1000
  numPar <- length(lb)
  X1 <- data.frame(matrix(runif(numPar * n), nrow = n))
  X2 <- data.frame(matrix(runif(numPar * n), nrow = n))
  names(X1) <- parameterNames
  names(X2) <- parameterNames
  sobolFun <- function(X) {
    p <- NULL
    if (is.data.frame(X)) {
      return(sapply(1:nrow(X), function(x) {
        progress <- (100.0 / nrow(X) * x)
        if (progress %% 1 == 0) {
          print(progress)
        }
        temp <- lb + (ub - lb) * X[x, ]
        temp <- as.numeric(temp)
        lossFct(temp, env, FALSE)
      }))
    } else {
      p <- as.numeric(lb + (ub - lb) * X)
    }
    lossFct(p, env, FALSE)
  }
  x <- sensitivity::sobol(model = sobolFun, X1 = X1, X2 = X2, order = 2, nboot = 100)
  pl <- ggplot(x)
  xBreaks <- layer_scales(pl)$x$break_positions()
  xLabels <- layer_scales(pl)$x$limits
  pl + theme(
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
    scale_x_continuous(
      breaks = xBreaks,
      labels = xLabels
    ) +
    ylab("Explained fraction of variance")
}

#' Optimize algebraic systems which describe thermodynamic binding systems
#'
#' @export
#' @import rootSolve
#' @import ggplot2
#' @rawNamespace import(sensitivity, except=c(print.src))
#' @param case is a character describing which system should be investigated. Either:
#' "dba_host_const", "dba_dye_const", "ida" or "gda".
#' @param parameters is a numeric vector containing already optimized parameter.
#'        In case of *hg* the order of the parameters is: *khd*, *I0*, *IHD* and *ID*
#'        In case of *ida* and *ga* the order of the parameters is: *kg*, *I0*, *IHD* and *ID*.
#' @param percentage is the percentage +/- from parameters in which the sensitivity should be analysed.
#' @param OffsetBoundaries in case percentage is not suitable a numeric vector (equivalent to parameters) can be used which is added/substracted from parameters. It is only possible to set either percentage or OffsetBoundaries.
#' @param path is a filepath which contains tabular x-y data. The concentraion of dye or guest respectivly is assumed to be in the first column. Furthermore, should the corresponding signal be stored in the second column. As an alternative an already loaded data.frame can be passed to the function.
#' @param additionalParameters are required parameters which are specific for each case.
#'        In case of *hg* a numeric vector of length 1 is expected which contains the concentration of the host.
#'        In case of *ida* a numeric vector of length 3 is expected which contains the concentration of the host, dye and the *khd* parameter.
#'        In case of *gda* a numeric vector of length 3 is expected which contains the concentration of the host, guest and the *khd* parameter.
#' @param runAsShiny is internally used when running the algorithm from shiny.
#' @param error_calc_fct is an optional input defining how the error between the in silico signal and the measured signal is calculated.
#'        One can use one of the following predefined functions as character vectors: *Rel. Error*, *RMSE*, *SSE*, or *Huber*. The default function is *Rel. Error*.
#'        Alternatively a function can be passed to opti, which has to expect two arguments, first the insilico signal followed by the measured signal.
#' @return a plot showing the sensitivity
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
#' res <- opti("ida", c(1, 0, 0, 0), c(10^9, 10^6, 10^6, 10^6), path, c(4.3, 6.0, 7079458))
#' sensitivity("ida", res[[2]], path, c(4.3, 6.0, 7079458), 20)
sensitivity <- function(case, parameters, path, additionalParameters,
                        percentage = NULL, OffsetBoundaries = NULL, runAsShiny = FALSE, error_calc_fct = "rel. Error") {
  if (!is.character(case)) {
    stop("case has to be of type character")
  }
  if (!(case %in% c("dba_dye_const", "dba_host_const", "ida", "gda"))) {
    stop("case is neither dba_dye_const, dba_host_const, ida or gda")
  }
  if (!is.data.frame(parameters)) {
    stop("optimizedParameters have to be of type data.frame")
  }
  if (length(parameters) == 0) {
    stop("optimizedParameters vector seems to be empty")
  }
  if (length(parameters) > 4) {
    stop("optimizedParameters vector has more than 4 entries")
  }
  if (case == "hg" && length(additionalParameters) != 1) {
    stop("additionalParameters have to be of length 1")
  }
  if (case == "ida" && length(additionalParameters) != 3) {
    stop("additionalParameters have to be of length 3")
  }
  if (case == "gda" && length(additionalParameters) != 3) {
    stop("additionalParameters have to be of length 3")
  }
  error_fct <- NULL
  error_fct_name <- NULL
  if (is.character(error_calc_fct)) {
    error_fct_name <- error_calc_fct
    error_fct <- get_error_calc_fct(error_calc_fct)
  } else {
    error_fct_name <- substitute(error_calc_fct)
    check_error_calc_function(error_calc_fct)
    error_fct <- error_calc_fct
  }

  lowerBounds <- NULL
  upperBounds <- NULL
  if (!is.null(percentage) && !is.null(OffsetBoundaries)) {
    stop("percentage and OffserBoundaries cannot be used together")
  }
  if (!is.null(percentage)) {
    if (!is.numeric(percentage)) {
      stop("Percentage has to be numeric")
    }
    if (percentage < 0.01) {
      stop("Percentage has to be at least 0.1")
    }
    perturbationFactor <- percentage / 100
    lowerBounds <- parameters - (parameters) * perturbationFactor
    upperBounds <- parameters + (parameters) * perturbationFactor
  } else if (!is.null(OffsetBoundaries)) {
    lowerBounds <- parameters - OffsetBoundaries
    upperBounds <- parameters + OffsetBoundaries
  } else {
    stop("Neither percentage nor OffsetBoundaries were defined")
  }

  if (!is.character(path) && !is.data.frame(path)) {
    stop("path has to be of type character or a data.frame")
  }
  df <- NULL
  if (!is.data.frame(path)) {
    df <- try(importData(path))
    if (class(df) == "try-error") {
      stop("Could not read file")
    }
  } else {
    df <- path
  }

  parameters <- as.numeric(parameters)
  env <- new.env()
  env$error_calc_fct <- error_fct
  parameterNames <- NULL
  if (case == "dba_host_const") {
    names(df) <- c("dye", "signal")
    lossFct <- lossFctHG
    env$dye <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
    parameterNames <- c("kHD", "I0", "IHD", "ID")
  } else if (case == "dba_dye_const") {
    names(df) <- c("host", "signal")
    lossFct <- lossFctDBA
    env$host <- df[, 1]
    env$signal <- df[, 2]
    env$d0 <- additionalParameters[1]
    parameterNames <- c("kHD", "I0", "IHD", "ID")
  } else if (case == "ida") {
    names(df) <- c("guest", "signal")
    lossFct <- lossFctIDA
    env$ga <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
    env$d0 <- additionalParameters[2]
    env$kd <- additionalParameters[3]
    parameterNames <- c("kGuest", "I0", "IHD", "ID")
  } else if (case == "gda") {
    names(df) <- c("dye", "signal")
    lossFct <- lossFctGDA
    env$dye <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
    env$ga0 <- additionalParameters[2]
    env$kd <- additionalParameters[3]
    parameterNames <- c("kGuest", "I0", "IHD", "ID")
  }
  tryCatch(expr = {
    sobolVariance(lossFct, env, lowerBounds, upperBounds, parameterNames, runAsShiny)
  }, interrupt = function(e) {
    stop("Interrputed the calculation of the Sobol indices")
  }, error = function(e) {
    em <- conditionMessage(e)
    return(em)
  })
}
