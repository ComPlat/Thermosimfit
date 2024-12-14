extract_best_runs <- function(res) {
  states <- res$states
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  states <- Reduce(rbind, states)
  params <- Reduce(rbind, params)
  states <- lapply(unique(errors$dataset), function(x) {
    states_subset <- states[states$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50, ]
    states_subset[states_subset$repetition %in% errors_subset$repetition, ]
  })
  states <- Reduce(rbind, states)
  return(states)
}

load_data <- function(path) {
  load(path)
  states <- extract_best_runs(res[[1]])
  df <- data.frame(
    var = rep(states[, 1], 2),
    signal = c(states[, 2], states[, 3]),
    dataset = rep(states[, "dataset"], 2),
    repetition = rep(states[, "repetition"], 2),
    group = c(rep(
      "Measured",
      nrow(states)
    ), rep(
      "Simulated",
      nrow(states)
    ))
  )
  model <- aov(signal ~ group + Error(var), data = df)
  summary(model)
}

load_data("dba_100Runs.RData")
load_data("ida_100.RData")
load_data("gda_100.RData")


load_params <- function(path) {
  load(path)
  res <- res[[1]]
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  params <- Reduce(rbind, params)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50, ]
    params_subset[params_subset$repetition %in% errors_subset$repetition, ]
  })
  params <- Reduce(rbind, params)
  lapply(names(params)[1:4], function(idx) {
    model <- aov(params[, idx] ~ dataset, data = params)
    print(idx)
    summary(model) |> print()
  })
  return()
}

load_params("dba_100Runs.RData")
load_params("ida_100.RData")
load_params("gda_100.RData")


library(mixtools)
load_params <- function(path) {
  load(path)
  res <- res[[1]]
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  params <- Reduce(rbind, params)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50, ]
    params_subset[params_subset$repetition %in% errors_subset$repetition, ]
  })
  params <- Reduce(rbind, params)
  par(mfrow = c(2, 2))
  lapply(names(params)[1:4], function(idx) {
    print(idx)
    e <- try({
      mix_model <- normalmixEM(params[, idx], k = 2)
      mix_model$mu |> print()
      mix_model$sigma |> print()
      plot(mix_model, which = 2)
      legend("topright", legend = idx)
    })
    if (inherits(e, "try-error")) {
      hist(params[, idx])
      legend("topright", legend = idx)
    }
  })
  return()
}

load_params("dba_100Runs.RData")
load_params("ida_100.RData")
load_params("gda_100.RData")







library(mixtools)
fit_mixture_model <- function(data, k = 2) {
  initial_means <- sample(data, k)
  initial_sd <- rep(sd(data) / 2, k)
  initial_weights <- rep(1 / k, k)
  mix_model <- normalmixEM(data,
    k = k,
    lambda = initial_weights,
    mu = initial_means, sigma = initial_sd
  )
  print(mix_model$mu)
  print(mix_model$sigma)
  return(mix_model)
}
plot_mixture <- function(data, mix_model, idx) {
  hist(data,
    breaks = 30,
    prob = TRUE, main = paste("Mixture Model for", idx), xlab = idx
  )
  x_vals <- seq(min(data), max(data), length.out = 100)
  y_vals <- sapply(x_vals, function(x) {
    sum(mix_model$lambda * dnorm(x, mean = mix_model$mu, sd = mix_model$sigma))
  })
  lines(x_vals, y_vals, col = "blue", lwd = 2)
  legend("topright", legend = c("Gaussian Mixture"), col = "blue", lwd = 2)
}
load_params <- function(path) {
  load(path)
  res <- res[[1]]
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  params <- Reduce(rbind, params)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[
      order(errors_subset$MeanSquareError),
    ][1:50, ]
    params_subset[params_subset$repetition %in% errors_subset$repetition, ]
  })
  params <- Reduce(rbind, params)
  par(mfrow = c(2, 2))
  lapply(names(params)[1:4], function(idx) {
    tryCatch(
      {
        data <- params[, idx]
        mix_model <- fit_mixture_model(data, k = 4)
        plot_mixture(data, mix_model, idx)
      },
      error = function(e) {
        print(e)
        hist(params[, idx], main = paste("Histogram for", idx), xlab = idx)
        legend("topright", legend = idx)
      }
    )
  })
  return()
}
load_params("dba_100Runs.RData")
load_params("ida_100.RData")
load_params("gda_100.RData")


load_data <- function(path) {
  load(path)
  res <- res[[1]]
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  params <- Reduce(rbind, params)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[
      order(errors_subset$MeanSquareError),
    ][1:50, ]
    params_subset[params_subset$repetition %in% errors_subset$repetition, ]
  })
  params <- Reduce(rbind, params)
  return(params)
}
pso <- function(loss_fct, lb, ub, data,
                npop = 40, ngen = 1000, global = FALSE) {
  npar <- length(lb)
  error_threshold <- -Inf
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

  if (any(lb < 0) || any(ub < 0)) {
    for (i in seq(npop)) {
      swarm[i, ] <- runif(npar, min = lb, max = ub)
      swarm_errors[i] <- loss_fct(swarm[i, ], data)
      swarm_bests[i] <- swarm_errors[i]
    }
  } else {
    lb <- ifelse(lb <= 0, 10^-15, lb)
    ub <- ifelse(ub <= 0, 10^-15, ub)
    lb <- log(lb)
    ub <- log(ub)

    for (i in seq(npop)) {
      swarm[i, ] <- runif(npar, min = lb, max = ub)
      swarm_errors[i] <- loss_fct(exp(swarm[i, ]), data)
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

      error <- loss_fct(swarm[i, ], data)

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
    if ((iter %% 100) == 0) {
      print(iter)
      print(global_best_vec)
      print(global_best_error)
    }

    if (global_best_error < error_threshold) {
      if (no_improvement > 1000) {
        break
      }
      break
    }
  }
  return(global_best_vec)
}
densi <- function(par, data) {
  w1 <- par[1]
  w2 <- par[4]
  w3 <- par[7]
  w4 <- par[9]
  total_weight <- w1 + w2 + w3 + w4
  w1 <- w1 / total_weight
  w2 <- w2 / total_weight
  w3 <- w3 / total_weight
  w4 <- w4 / total_weight
  norm_part <- w1 * dnorm(data, mean = par[2], sd = par[3])
  gamma_part <- w2 * dgamma(data, shape = par[5], rate = par[6])
  exp_part <- w3 * dexp(data, rate = par[8])
  lognorm_part <- w4 * dlnorm(data, meanlog = par[10], sdlog = par[11])
  log_density <- log(
    norm_part + gamma_part + exp_part + lognorm_part
  ) # NOTE: better error values by double log
  return(log_density)
}
fit_mixture_model <- function(data, name) {
  loss_fct <- function(par, data) {
    densities <- densi(par, data)
    sum(log(densities[densities > 0]))
  }
  minf <- 0.01
  lb <- c(
    0, minf * min(data), minf * min(data),
    0, minf * min(data), minf * min(data),
    0, minf * min(data),
    0, minf * min(data), minf * min(data)
  )
  lb <- ifelse(lb < 10^-15, 10^-15, lb)
  maxf <- 5
  ub <- c(
    0.5, maxf * max(data), maxf * max(data),
    0.1, maxf * max(data), maxf * max(data),
    0.1, maxf * max(data),
    0.5, maxf * max(data), maxf * max(data)
  )
  res <- pso(loss_fct, lb, ub, data,
    npop = 40, ngen = 10000, global = FALSE
  )
  x_vals <- seq(min(data), max(data), length.out = 100)
  y_vals <- densi(res, x_vals) |> exp()
  hist(
    data,
    prob = TRUE,
    xlab = name
  )
  lines(x_vals, y_vals, col = "blue", lwd = 2)
}
fit <- function(path) {
  params <- load_data(path)
  par(mfrow = c(2, 2))
  lapply(names(params)[1:4], function(idx) {
    tryCatch(
      {
        data <- params[, idx]
        data <- (data - min(data)) / (max(data) - min(data))
        fit_mixture_model(data, idx)
      },
      error = function(e) {
        print(e)
        hist(params[, idx], main = paste("Histogram for", idx), xlab = idx)
        legend("topright", legend = idx)
      }
    )
  })
  return()
}
fit("dba_100Runs.RData")
fit("ida_100.RData")
fit("gda_100.RData")
