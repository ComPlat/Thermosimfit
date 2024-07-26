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
        if (is(runAsShiny, "Communicator")) {
          status <- runAsShiny$getStatus()
          if (status == "interrupt") {
            return()
          }
          if (x %% 10 == 0) {
            runAsShiny$running((100 / nrow(X)) * x)
          }
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
  x <- sobol(model = sobolFun, X1 = X1, X2 = X2, order = 2, nboot = 100)
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
#' @import sensitivity
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
#' @return either an instance of ErrorClass if something went wrong. Otherwise plots showing the sensitivity are returned.
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
#' res <- opti("ida", c(1, 0, 0, 0), c(10^9, 10^6, 10^6, 10^6), path, c(4.3, 6.0, 7079458))
#' sensitivity("ida", res[[2]], path, c(4.3, 6.0, 7079458), 20)
sensitivity <- function(case, parameters, path, additionalParameters,
                        percentage = NULL, OffsetBoundaries = NULL, runAsShiny = FALSE) {
  if (!is.character(case)) {
    return(ErrorClass$new("case has to be of type character"))
  }
  if (!(case %in% c("dba_dye_const", "dba_host_const", "ida", "gda"))) {
    return(ErrorClass$new("case is neither dba_dye_const, dba_host_const, ida or gda"))
  }
  if (!is.data.frame(parameters)) {
    return(ErrorClass$new("optimizedParameters have to be of type data.frame"))
  }
  if (length(parameters) == 0) {
    return(ErrorClass$new("optimizedParameters vector seems to be empty"))
  }
  if (length(parameters) > 4) {
    return(ErrorClass$new("optimizedParameters vector has more than 4 entries"))
  }
  if (case == "hg" && length(additionalParameters) != 1) {
    return(ErrorClass$new("additionalParameters have to be of length 1"))
  }
  if (case == "ida" && length(additionalParameters) != 3) {
    return(ErrorClass$new("additionalParameters have to be of length 3"))
  }
  if (case == "gda" && length(additionalParameters) != 3) {
    return(ErrorClass$new("additionalParameters have to be of length 3"))
  }

  lowerBounds <- NULL
  upperBounds <- NULL
  if (!is.null(percentage) && !is.null(OffsetBoundaries)) {
    return(ErrorClass$new("percentage and OffserBoundaries cannot be used together"))
  }
  if (!is.null(percentage)) {
    if (!is.numeric(percentage)) {
      return(ErrorClass$new("Percentage has to be numeric"))
    }
    if (percentage < 0.01) {
      return(ErrorClass$new("Percentage has to be at least 0.1"))
    }
    perturbationFactor <- percentage / 100
    lowerBounds <- parameters - (parameters) * perturbationFactor
    upperBounds <- parameters + (parameters) * perturbationFactor
  } else if (!is.null(OffsetBoundaries)) {
    lowerBounds <- parameters - OffsetBoundaries
    upperBounds <- parameters + OffsetBoundaries
  } else {
    return(ErrorClass$new("Neither percentage nor OffsetBoundaries were defined"))
  }

  if (!is.character(path) && !is.data.frame(path)) {
    return(ErrorClass$new("path has to be of type character or a data.frame"))
  }
  df <- NULL
  if (!is.data.frame(path)) {
    df <- try(importData(path))
    if (class(df) == "try-error") {
      return(ErrorClass$new("Could not read file"))
    }
  } else {
    df <- path
  }

  parameters <- as.numeric(parameters)
  env <- new.env()
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
  sobolVariance(lossFct, env, lowerBounds, upperBounds, parameterNames, runAsShiny)
}

