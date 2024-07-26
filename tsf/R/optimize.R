#' Optimize algebraic systems which describe thermodynamic binding systems
#'
#' @export
#' @import rootSolve
#' @import ggplot2
#' @import patchwork
#' @param case is a character describing which system should be investigated. Either: "hg", "ida" or "gda".
#' @param lowerBounds is a numeric vector defining the lower boundaries of the parameter.
#'        In case of *hg* the order of the parameters is: *khd*, *I0*, *IHD* and *ID*
#'        In case of *ida* and *ga* the order of the parameters is: *kg*, *I0*, *IHD* and *ID*.
#' @param upperBounds is a numeric vector defining the upper boundaries of the parameter.
#'        The order is the same as for the lower boundaries.
#' @param path is a filepath which contains tabular x-y data. The concentraion of dye or guest respectivly is assumed to be in the first column. Furthermore, should the corresponding signal be stored in the second column. As an alternative an already loaded data.frame can be passed to the function.
#' @param additionalParameters are required parameters which are specific for each case.
#'        In case of *hg* a numeric vector of length 1 is expected which contains the concentration of the host.
#'        In case of *ida* a numeric vector of length 3 is expected which contains the concentration of the host, dye and the *khd* parameter.
#'        In case of *gda* a numeric vector of length 3 is expected which contains the concentration of the host, guest and the *khd* parameter.
#' @param npop is an optional integer argument defining the number of particles during optimization. The default value is set to 40.
#' @param ngen is an optional integer argument defining the number of generations of the particle swarm optimization. The default value is set to 200.
#' @param Topology is an optional character argument defining which topology should be used by the particle swarm algorithm. The options are "star" and "random". The default topology is the "random" topology.
#' @param errorThreshold is an optional numeric argument defining a sufficient small error which acts as a stop signal for the particle swarm algorithm. The default value is set to -Inf.
#' @param runAsShiny is internally used when running the algorithm from shiny.
#' @return either an instance of ErrorClass if something went wrong. Otherwise the optimized parameter and the *insilico* signal values are returned.
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
#' opti("ida", c(1, 0, 0, 0), c(10^9, 10^6, 10^6, 10^6), path, c(4.3, 6.0, 7079458))
opti <- function(case, lowerBounds, upperBounds, path, additionalParameters,
                 npop = 40, ngen = 200, Topology = "random", errorThreshold = -Inf, runAsShiny = FALSE) {
  if (!is.character(case)) {
    return(ErrorClass$new("case has to be of type character"))
  }
  if (!(case %in% c("hg", "ida", "gda"))) {
    return(ErrorClass$new("case is neither hg, ida or gda"))
  }
  if (!is.numeric(lowerBounds)) {
    return(ErrorClass$new("lowerBounds have to be of type numeric"))
  }
  if (length(lowerBounds) == 0) {
    return(ErrorClass$new("lowerBounds vector seems to be empty"))
  }
  if (length(lowerBounds) > 4) {
    return(ErrorClass$new("lowerBounds vector has more than 4 entries"))
  }
  if (!is.numeric(upperBounds)) {
    return(ErrorClass$new("upperBounds have to be of type numeric"))
  }
  if (length(upperBounds) == 0) {
    return(ErrorClass$new("upperBounds vector seems to be empty"))
  }
  if (length(upperBounds) > 4) {
    return(ErrorClass$new("upperBounds vector has more than 4 entries"))
  }

  if (!is.character(path) && !is.data.frame(path)) {
    return(ErrorClass$new("path has to be of type character or a data.frame"))
  }

  if (!is.numeric(additionalParameters)) {
    return(ErrorClass$new("additionalParameters have to be of type numeric"))
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
  if (!is.numeric(npop) && !is.integer(npop)) {
    return(ErrorClass$new("npop has to be of type numeric or integer"))
  }
  if (npop <= 5 || npop > 400) {
    return(ErrorClass$new("npop has to be between 5 and 400"))
  }
  if (!is.numeric(ngen) && !is.integer(ngen)) {
    return(ErrorClass$new("ngen has to be of type numeric or integer"))
  }
  if (ngen <= 5 || npop > 10^7) {
    return(ErrorClass$new("ngen has to be between 5 and 10^7"))
  }
  if (!is.character(Topology)) {
    return(ErrorClass$new("Topology has to be of type character"))
  }
  if (!(Topology %in% c("star", "random"))) {
    return(ErrorClass$new("Topology is neither star or random"))
  }
  Topo <- FALSE
  if (Topology == "star") Topo <- TRUE
  if (!is.numeric(errorThreshold)) {
    return(ErrorClass$new("errorThreshold has to be of type numeric"))
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
  check <- upperBounds < lowerBounds
  if (any(check == TRUE)) {
    return(ErrorClass$new("lowerBounds < upperBounds not fulfilled"))
  }

  lossFct <- NULL
  env <- new.env()
  if (case == "hg") {
    names(df) <- c("dye", "signal")
    lossFct <- lossFctHG
    env$dye <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
  } else if (case == "ida") {
    names(df) <- c("guest", "signal")
    lossFct <- lossFctIDA
    env$ga <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
    env$d0 <- additionalParameters[2]
    env$kd <- additionalParameters[3]
  } else if (case == "gda") {
    names(df) <- c("dye", "signal")
    lossFct <- lossFctGDA
    env$dye <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
    env$ga0 <- additionalParameters[2]
    env$kd <- additionalParameters[3]
  }

  res <- pso(
    env, lowerBounds, upperBounds, lossFct, ngen, npop,
    errorThreshold, Topo, FALSE, runAsShiny
  )

  if (case == "hg") {
    df$signal_insilico <- res[[1]][, 1]
    df$d <- res[[1]][, 2]
    df$hd <- res[[1]][, 3]
    params <- data.frame(
      khd = res[[2]][1], I0 = res[[2]][2],
      IHD = res[[2]][3], ID = res[[2]][4]
    )
    return(list(
      df, params, plotTSF(df, "dye"),
      metrices(df$signal, df$signal_insilico)
    ))
  } else if (case == "ida") {
    df$signal_insilico <- res[[1]][, 1]
    df$d <- res[[1]][, 2]
    df$hd <- res[[1]][, 3]
    params <- data.frame(
      kguest = res[[2]][1], I0 = res[[2]][2],
      IHD = res[[2]][3], ID = res[[2]][4]
    )
    global_minimum <- res[[2]][5]
    return(list(
      df, params, plotTSF(df, "guest"),
      metrices(df$signal, df$signal_insilico)
    ))
  } else if (case == "gda") {
    df$signal_insilico <- res[[1]][, 1]
    df$d <- res[[1]][, 2]
    df$hd <- res[[1]][, 3]
    params <- data.frame(
      kguest = res[[2]][1], I0 = res[[2]][2],
      IHD = res[[2]][3], ID = res[[2]][4]
    )
    global_minimum <- res[[2]][5]
    return(list(
      df, params, plotTSF(df, "dye"),
      metrices(df$signal, df$signal_insilico)
    ))
  }
}
