create_task_queue <- function(case, lowerBounds, upperBounds, list_df,
                              additionalParameters, seed, npop, ngen,
                              Topology, error_calc_fct, errorThreshold, num_rep, num_cores) {
  if (num_cores <= 0) {
    stop("num_cores has to be >= 1")
  }
  # num cores calculation
  size <- length(list_df) * num_rep
  if (size <= 0) {
    stop("Size is <= 0;
      Is number of datasets or number of replicates <= 0?")
  }
  if (num_cores > size) {
    num_cores <- size
  }

  # seed case and generation of seeds
  seed_case <- determine_seed_case(seed, num_rep)
  seed_origin <- NULL
  if (seed_case == 3) {
    seed_origin <- seed
  }
  seeds <- numeric(size)
  seeds_from <- 1:1e6
  for (i in seq_len(size)) {
    if (seed_case == 1) {
      seed <- sample(seeds_from, 1)
    } else if (seed_case == 3) {
      if (i %in% seq(1, size, num_rep)) {
        seed <- seed_origin
      } else {
        seed <- sample(seeds_from, 1)
      }
    } else if (seed_case == 2) {
      seed <- seed # TODO: check is this correct
    }
    seeds[i] <- seed
  }

  # create message for each optimization
  messages <- character(size)
  counter_messages <- 1
  for (i in seq_len(length(list_df))) {
    for (j in seq_len(num_rep)) {
      messages[counter_messages] <-
        paste0("Dataset = ", i, "; Replicate = ", j)
      counter_messages <- counter_messages + 1
    }
  }

  # 3. Fill task queue
  dfs <- rep(list_df, each = num_rep)
  TaskQueue$new(
    case,
    lowerBounds, upperBounds, dfs,
    additionalParameters, seeds,
    npop, ngen, Topology, error_calc_fct, errorThreshold,
    messages, num_cores
  )
}

#' Runs a batch of optimization tasks
#'
#' @export
#' @param case is a character argument which specifies the optimization case.
#' Either "dba_dye_const", "dba_host_const", "ida" or "gda"
#' @param lowerBounds is a numeric vector with the lower bounds for the optimization
#' @param upperBounds is a numeric vector with the upper bounds for the optimization
#' @param path is a character argument which specifies the path to the data
#' @param additionalParameters is a numeric vector with additional parameters
#'        In case of *dba_dye_const* or *dba_host_const the order of the parameters is: *khd*, *I0*, *IHD* and *ID*
#'        In case of *ida* and *ga* the order of the parameters is: *kg*, *I0*, *IHD* and *ID*.
#' @param seed is an optional integer argument defining the seed which is set directly for the optimization. In case the argument is not set the current time is used as seed.
#' @param npop is an optional integer argument defining the number of particles during optimization. The default value is set to 40.
#' @param ngen is an optional integer argument defining the number of generations of the particle swarm optimization. The default value is set to 200.
#' @param Topology is an optional character argument defining which topology should be used by the particle swarm algorithm. The options are "star" and "random". The default topology is the "random" topology.
#' @param error_calc_fct is an optional input function which is used to calculate the error. The function should expect two arguments, first the insilico signal followed by the measured signal.
#' @param errorThreshold is an optional numeric argument defining a sufficient small error which acts as a stop signal for the particle swarm algorithm. The default value is set to -Inf.
#' @param num_rep is an optional integer argument defining the number of replicates for each dataset
#' @param num_cores is an optional integer argument defining the maximum number of cores which should be used for the optimization
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDABatch.csv")
#' lowerBounds <- c(
#'   kG = 1000,
#'   I0 = 0,
#'   IHD = 0,
#'   ID = 0
#' )
#' upperBounds <- c(
#'   kG = 10^8,
#'   I0 = 100,
#'   IHD = 10^7,
#'   ID = 10^7
#' )
#' additionalParameters <- c(
#'   host = 1e-6,
#'   dye = 1e-6,
#'   kHD = 3e6
#' )
#' tsf::batch(
#'   "ida",
#'   lowerBounds, upperBounds,
#'   path, additionalParameters,
#'   ngen = 20
#' )
batch <- function(case,
                  lowerBounds, upperBounds,
                  path,
                  additionalParameters,
                  seed = NA, npop = 40, ngen = 200, Topology = "random",
                  error_calc_fct = NULL,
                  errorThreshold = -Inf, num_rep = 1, num_cores = 1) {
  if (!(case %in% c("dba_dye_const", "dba_host_const", "ida", "gda"))) {
    stop("case is neither dba_dye_const, dba_host_const, ida or gda")
  }
  # import data
  list_df <- importDataBatch(path)
  if (!is.list(list_df)) {
    return(ErrorClass$new("Could not import data"))
  }
  for (i in seq_along(list_df)) {
    if (!is.data.frame(list_df[[i]])) {
      return(ErrorClass$new("Found non data.frame entry"))
    }
  }
  if (is.null(error_calc_fct)) {
    error_calc_fct <- rel_err
  }

  tq <- create_task_queue(
    case, lowerBounds, upperBounds, list_df,
    additionalParameters, seed, npop, ngen,
    Topology, error_calc_fct, errorThreshold, num_rep, num_cores
  )

  # 4. assign tasks
  tq$assign()
  old_status <- character(num_cores)

  while (TRUE) {
    new_status <- tq$get_status(old_status)
    cat(new_status)
    cat("\n")
    old_status <- new_status
    if (!tq$queue_empty() && tq$check()) {
      tq$assign()
    }
    if (tq$queue_empty()) break
    Sys.sleep(1)
  }

  list <- tq$seperate_results()
  list(
    list,
    plotStates(list),
    plotParams(list),
    plotMetrices(list)
  )
}
