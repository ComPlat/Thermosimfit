#' Particle swarm optimization
#'
#' @description Interface to the particle swarm optimization (pso) algorithm.
#'
#' @export
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable, runExample))
#' @rawNamespace import(shinyWidgets, except=c(alert))
#' @import shinydashboard
#' @import shinyjs
#' @import openxlsx
#' @import DT
#' @param env is something that is passed to the loss function in addition to the parameters which get optimized
#' @param lb is a numeric vector defining the lower boundaries of the parameter
#' @param ub is a numeric vector defining the upper boundaries of the parameter
#' @param loss is the loss function for which the optimal parameter set should be found
#' @param ngen is the number of generations the pso should run
#' @param npop is the number of particles which should be used during optimization
#' @param error_threshold is a number defining a sufficient small error at which the optimization is stopped
#' @param global is a logical parameter. If set to TRUE a global star topology is used.
#'        Thus, each particle compares itself with the global best particle of the entire swarm. In contrast,
#'        if global is set to FALSE a random arbitrary neighborhood is used instead. Thus, each particle has
#'        a neighberhood which contains K neighbours where K is between 0 and 3. From the swarm K neighbours are
#'        drawn randomly. From the neighberhood the best particle is used for comparison. The neighberhood is
#'        calculated for each generation.
#' @param save_swarm is a logical value defining whether the entire optimization should be saved.
#' @param run_as_shiny is an internal parameter which is used when running the shiny app interface.
#' @param add_message is an optional character argument which is printed during optimization
#' @examples
#' rosenbrock <- function(parameter, env, Ignore) {
#'   value <- 0
#'   for (i in 1:(length(parameter) - 1)) {
#'     value <- value +
#'       100 * (parameter[i + 1] - parameter[i]^2)^2 +
#'       (1 - parameter[i])^2
#'   }
#'   return(value)
#' }
#'
#' set.seed(1234)
#' res <- tsf::pso(
#'   new.env(), rep(-10, 3), rep(10, 3), rosenbrock, 1000, 40,
#'   0.00001, TRUE, FALSE
#' )
pso <- function(env, lb, ub, loss, ngen, npop, error_threshold, global = FALSE,
                save_swarm = FALSE, run_as_shiny = FALSE, add_message = "") {
  stopifnot(length(lb) == length(ub))
  if (length(lb) != length(ub)) {
    stop("length of lb and ub differ")
  }
  if (!is.numeric(lb)) {
    stop("lb has to be of type numeric")
  }
  if (!is.numeric(ub)) {
    stop("ub has to be of type numeric")
  }
  if (!is.function(loss)) {
    stop("loss has to be of type function")
  }
  if (!is.numeric(ngen)) {
    stop("ngen has to be of type numeric")
  }
  if (!is.numeric(npop)) {
    stop("npop has to be of type numeric")
  }
  if (length(ngen) != 1) {
    stop("ngen has to be a scalar")
  }
  if (length(npop) != 1) {
    stop("npop has to be a scalar")
  }
  if (length(lb) <= 0) {
    stop("lb and ub need at least one element")
  }
  if (any(lb) < 0) {
    stop("lb should be at least 0")
  }
  if (any(ub) < 0) {
    stop("ub should be at least 0")
  }
  if (ngen <= 10) {
    stop("ngen has to be at least 10")
  }
  if (npop <= 5) {
    stop("npop has to be at least 5")
  }
  if (!is.numeric(error_threshold)) {
    stop("error_threshold has to be of type numeric")
  }
  if (length(error_threshold) != 1) {
    stop("error_threshold has to be a scalar")
  }
  if (!is.logical(global)) {
    stop("global has to be of type logical")
  }
  if (length(global) != 1) {
    stop("global has to be a scalar")
  }
  if (!is.logical(save_swarm)) {
    stop("save_swarm has to be of type logical")
  }
  if (length(save_swarm) != 1) {
    stop("save_swarm has to be a scalar")
  }
  if (any(lb > ub)) {
    w <- which(lb > ub)
    w <- paste(w, collapse = ", ")
    return(ErrorClass$new(paste("lb > ub for: ", w)))
  }
  loss_fct <- function(...) {
    e <- try(loss(...))
    if (inherits(e, "try-error")) {
      stop("Could not evaluate the loss function")
    }
    return(e)
  }

  npar <- length(lb)
  swarm <- matrix(0, nrow = npop, ncol = npar)
  v <- matrix(0, nrow = npop, ncol = npar)
  swarm_bests <- numeric(npop)
  swarm_errors <- numeric(npop)
  initial_cog <- 2.5
  final_cog <- 0.5
  initial_soc <- 0.5
  final_soc <- 2.5
  w <- 0.5
  w_max <- 0.9
  w_min <- 0.4

  memory <- matrix(0, nrow = ngen * npop, ncol = npar)
  error_memory <- numeric(ngen * npop)

  if (any(lb < 0) || any(ub < 0)) {
    for (i in seq(npop)) {
      swarm[i, ] <- runif(npar, min = lb, max = ub)
      swarm_errors[i] <- loss_fct(swarm[i, ], env)
      swarm_bests[i] <- swarm_errors[i]
    }
  } else {
    lb <- ifelse(lb <= 0, 10^-15, lb)
    ub <- ifelse(ub <= 0, 10^-15, ub)
    lb <- log(lb)
    ub <- log(ub)

    for (i in seq(npop)) {
      swarm[i, ] <- runif(npar, min = lb, max = ub)
      swarm_errors[i] <- loss_fct(exp(swarm[i, ]), env)
      swarm_bests[i] <- swarm_errors[i]
    }

    swarm <- exp(swarm)
    lb <- exp(lb)
    ub <- exp(ub)
  }
  global_best <- which.min(swarm_bests)
  global_best_vec <- swarm[global_best, ]
  global_best_error <- swarm_bests[global_best]
  swarm_best_params <- swarm
  k <- 3

  correct_below_lb <- function(target, threshold) {
    for (i in seq_len(length(target))) {
      if (target[i] < threshold[i]) target[i] <- threshold[i]
    }
    return(target)
  }

  correct_above_ub <- function(target, threshold) {
    for (i in seq_len(length(target))) {
      if (target[i] > threshold[i]) target[i] <- threshold[i]
    }
    return(target)
  }

  calc_neighberhood <- function() {
    neighberhood <- matrix(0L, ncol = k, nrow = npop)
    for (i in seq(npop)) {
      nneighbour <- sample(1:k, 1)
      neighbours <- sample(1:npop, nneighbour)
      if (length(neighbours) < k) {
        diff <- k - length(neighbours)
        neighbours <- c(neighbours, rep(NA, diff))
      }
      neighberhood[i, ] <- neighbours
    }
    neighberhood
  }
  neighberhood <- calc_neighberhood()
  convergence_check <- 0
  no_improvement <- 0

  iter <- 1
  while (iter < ngen) {
    if (iter == 1 || convergence_check != 0) {
      neighberhood <- calc_neighberhood()
    }

    if (save_swarm) memory[((iter * npop) + 1):((iter + 1) * npop), ] <- swarm

    w <- w_max - iter * (w_max - w_min) / ngen
    cog <- initial_cog - (initial_cog - final_cog) * (iter + 1) / ngen
    soc <- initial_soc - (initial_soc - final_soc) * (iter + 1) / ngen

    for (i in seq(npop)) {
      current_neighberhood <- neighberhood[i, ]
      current_neighberhood <- current_neighberhood[!is.na(current_neighberhood)]
      local_best <- which.min(swarm_bests[current_neighberhood])
      local_best <- current_neighberhood[local_best]
      local_best_vec <- swarm[local_best, ]

      if (global) {
        local_best_vec <- swarm[global_best, ]
      }

      v[i, ] <- w * v[i, ] +
        cog * runif(1) * (swarm_best_params[i, ] - swarm[i, ]) +
        soc * runif(1) * (local_best_vec - swarm[i, ])
      swarm[i, ] <- swarm[i, ] + v[i, ]

      swarm[i, ] <- correct_below_lb(swarm[i, ], lb)
      swarm[i, ] <- correct_above_ub(swarm[i, ], ub)

      error <- loss_fct(swarm[i, ], env)

      if (save_swarm) error_memory[((iter * npop) + i)] <- error

      if (!is.infinite(error) && !is.na(error) &&
        error < swarm_bests[i]) {
        swarm_bests[i] <- error
        swarm_best_params[i, ] <- swarm[i, ]
      }
      if (!is.infinite(error) && !is.na(error) &&
        error < global_best_error) {
        global_best <- i
        global_best_vec <- swarm[i, ]
        global_best_error <- error
        no_improvement <- 0
      } else {
        no_improvement <- no_improvement + 1
      }
      convergence_check <- convergence_check + 1
    }

    iter <- iter + 1

    print(add_message)
    print(iter)
    print(format_scientific(global_best_vec))
    print(format_scientific(global_best_error))
    if (is.environment(run_as_shiny)) {
      run_as_shiny$insilico <- list(
        loss_fct(global_best_vec, env, TRUE),
        c(global_best_vec)
      )
    }

    if (global_best_error < error_threshold) {
      if (no_improvement > 1000) {
        break
      }
      break
    }
  }

  insilico <- loss_fct(global_best_vec, env, TRUE)
  if (save_swarm) {
    return(list(
      insilico, c(global_best_vec),
      memory, error_memory
    ))
  }
  return(list(insilico, c(global_best_vec)))
}
