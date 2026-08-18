#' Optimize algebraic systems which describe thermodynamic binding systems
#'
#' @export
#' @import rootSolve
#' @import ggplot2
#' @import patchwork
#' @param case is a character describing which system should be investigated. Either:
#' "dba_host_const",
#' "dba_dye_const", "ida" or "gda".
#' @param lowerBounds is a numeric vector defining the lower boundaries of the parameter.
#'        In case of *dba_dye_const* or *dba_host_const the first parameter is: *khd*.
#'        In case of *ida* and *ga* the first parameter is: *kg*.
#'        Afterwards the parameters *I0*, *IHD* and *ID* have to be repeated for each signal respectivly.
#' @param upperBounds is a numeric vector defining the upper boundaries of the parameter.
#'        The order is the same as for the lower boundaries.
#' @param path is a filepath which contains tabular x-y data.
#'        The concentraion of dye or guest respectivly is assumed to be in the first column.
#'        Furthermore, should the corresponding signals be stored in the other columns.
#'        As an alternative an already loaded data.frame can be passed to the function.
#' @param additionalParameters are required parameters which are specific for each case.
#'        In case of *dba_host_const* a numeric vector of length 1 is expected which contains the concentration of the host.
#'        In case of *dba_dye_const* a numeric vector of length 1 is expected which contains the concentration of the dye.
#'        In case of *ida* a numeric vector of length 3 is expected which contains the concentration of the host, dye and the *khd* parameter.
#'        In case of *gda* a numeric vector of length 3 is expected which contains the concentration of the host, guest and the *khd* parameter.
#' @param seed is an optional integer argument defining the seed which is set directly for the optimization. In case the argument is not set the current time is used as seed.
#' @param npop is an optional integer argument defining the number of particles during optimization. The default value is set to 40.
#' @param ngen is an optional integer argument defining the number of generations of the particle swarm optimization. The default value is set to 200.
#' @param Topology is an optional character argument defining which topology should be used by the particle swarm algorithm. The options are "star" and "random". The default topology is the "random" topology.
#' @param errorThreshold is an optional numeric argument defining a sufficient small error which acts as a stop signal for the particle swarm algorithm. The default value is set to -Inf.
#' @param error_calc_fct is an optional character argument defining how the error between the
#'        in silico signal and the measured signal is calculated. One of *Rel. Error*, *RMSE*,
#'        *SSE*, or *Huber*. The default is *Rel. Error*.
#' @param add_info is an optional character argument which is printed during optimization
#' @param engine is an optional character argument selecting the fitness-evaluation backend for
#'        the particle swarm optimization: "ast2ast" (the default) is an ast2ast-compiled
#'        per-particle loss, "r" is the plain-R loss function -- see \code{\link{pso}}'s engine
#'        parameter for details. Results match to floating-point precision either way.
#' @return either an instance of ErrorClass if something went wrong. Otherwise the optimized parameter and the *insilico* signal values are returned.
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
#' opti("ida", c(1, 0, 0, 0), c(10^9, 10^6, 10^6, 10^6), path, c(4.3, 6.0, 7079458))
opti <- function(case, lowerBounds, upperBounds,
                 path, additionalParameters,
                 seed = NULL,
                 npop = 40, ngen = 200,
                 Topology = "random",
                 errorThreshold = -Inf, error_calc_fct = "Rel. Error", add_info = "",
                 engine = c("ast2ast", "r")) {
  engine <- match.arg(engine)
  validation <- tryCatch(expr = {
    if (!is.character(case)) {
      stop("case has to be of type character")
    }
    if (!(case %in% c("dba_dye_const", "dba_host_const", "ida", "gda"))) {
      stop("case is neither dba_dye_const, dba_host_const, ida or gda")
    }
    if (!is.numeric(lowerBounds)) {
      return(ErrorClass$new("lowerBounds have to be of type numeric"))
    }
    if (length(lowerBounds) == 0) {
      stop("lowerBounds vector seems to be empty")
    }
    if (length(lowerBounds) < 4) {
      stop("lowerBounds vector has less than 4 entries")
    }
    if (!is.numeric(upperBounds)) {
      stop("upperBounds have to be of type numeric")
    }
    if (length(upperBounds) == 0) {
      stop("upperBounds vector seems to be empty")
    }
    if (length(upperBounds) < 4) {
      stop("upperBounds vector has less than 4 entries")
    }
    if (!is.character(path) && !is.data.frame(path)) {
      stop("path has to be of type character or a data.frame")
    }
    if (!is.numeric(additionalParameters)) {
      stop("additionalParameters have to be of type numeric")
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
    if (!is.numeric(seed) && !is.integer(seed) && !is.null(seed)) {
      stop("Invalid seed argument found")
    }
    if (is.null(seed)) {
      seed <- as.numeric(Sys.time())
    }
    if (!is.numeric(npop) && !is.integer(npop)) {
      stop("npop has to be of type numeric or integer")
    }
    if (npop <= 5 || npop > 400) {
      stop("npop has to be between 5 and 400")
    }
    if (!is.numeric(ngen) && !is.integer(ngen)) {
      stop("ngen has to be of type numeric or integer")
    }
    if (ngen <= 5 || npop > 10^7) {
      stop("ngen has to be between 5 and 10^7")
    }
    if (!is.character(Topology)) {
      stop("Topology has to be of type character")
    }
    if (!(Topology %in% c("star", "random"))) {
      stop("Topology is neither star or random")
    }
    if (!is.numeric(errorThreshold)) {
      stop("errorThreshold has to be of type numeric")
    }
    check <- upperBounds < lowerBounds
    if (any(check == TRUE)) {
      stop("lowerBounds < upperBounds not fulfilled")
    }
    if (!is.character(error_calc_fct) || length(error_calc_fct) != 1 ||
      !(error_calc_fct %in% c("rel. Error", "Rel. Error", "RMSE", "SSE", "Huber"))) {
      stop('error_calc_fct has to be one of "Rel. Error", "RMSE", "SSE" or "Huber"')
    }
    error_fct_name <- error_calc_fct
    error_fct <- get_error_calc_fct(error_calc_fct)
    error_code <- pso_error_code(error_calc_fct)
  }, error = function(e) {
    return(ErrorClass$new(conditionMessage(e)))
  }, interrupt = function(e) {
    return(ErrorClass$new("Interrupted by user"))
  })
  if (inherits(validation, "ErrorClass")) {
    return(validation)
  }

  Topo <- tryCatch(
    expr = {
      if (Topology == "star") {
        TRUE
      } else {
        FALSE
      }
    },
    interrupt = function(e) {
      return(NULL)
    },
    error = function(e) {
      return(NULL)
    }
  )

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

  lossFct <- tryCatch(
    expr = {
      if (case == "dba_host_const") {
        lossFctHG
      } else if (case == "dba_dye_const") {
        lossFctDBA
      } else if (case == "ida") {
        lossFctIDA
      } else if (case == "gda") {
        lossFctGDA
      }
    },
    error = function(e) {
      return(NULL)
    },
    interrupt = function(e) {
      return(NULL)
    }
  )

  env <- tryCatch(expr = {
    env <- new.env()
    env$error_calc_fct <- error_fct
    env$n_sigs <- n_sigs
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
  }, error = function(e) {
    return(NULL)
  }, interrupt = function(e) {
    return(NULL)
  })

  loss_particle_a2a <- NULL
  add_params <- NULL
  if (engine == "ast2ast") {
    spec <- pso_a2a_spec(case)
    add_params <- spec$build_add_params(df, additionalParameters)
    loss_particle_a2a <- ast2ast::translate(
      spec$loss_fct, args_f = spec$args_f, types_f = spec$types_f
    )
  }

  runAsShiny <- tryCatch(expr = {
    runAsShiny <- new.env()
    runAsShiny$insilico <- NULL
    runAsShiny
  }, error = function(e) {
    return(NULL)
  }, interrupt = function(e) {
    return(NULL)
  })

  tryCatch(
    {
      set.seed(seed)
      res <- pso(
        env, lowerBounds, upperBounds, lossFct, ngen, npop,
        errorThreshold, Topo, FALSE, runAsShiny, add_info,
        engine = engine, loss_particle_a2a = loss_particle_a2a,
        add_params = add_params, error_code = error_code
      )
      params <- create_params_df(res, case, n_sigs)
      df <- create_data_df(df, res, case, n_sigs)
      lowerBounds <- correct_names_params(lowerBounds, case, n_sigs)
      upperBounds <- correct_names_params(upperBounds, case, n_sigs)
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
        seed = seed, additionalParameters = additionalParameters,
        lowerBounds = lowerBounds, upperBounds = upperBounds,
        npop = npop, ngen = ngen, Topology = Topology,
        n_sigs = n_sigs
      ))
    },
    interrupt = function(e) {
      res <- runAsShiny$insilico
      params <- create_params_df(res, case, n_sigs)
      df <- create_data_df(df, res, case, n_sigs)
      lowerBounds <- correct_names_params(lowerBounds, case, n_sigs)
      upperBounds <- correct_names_params(upperBounds, case, n_sigs)
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
        seed = seed, additionalParameters = additionalParameters,
        lowerBounds = lowerBounds, upperBounds = upperBounds,
        npop = npop, ngen = ngen, Topology = Topology,
        n_sigs = n_sigs
      ))
    },
    error = function(e) {
      stop(conditionMessage(e))
    }
  )
}
