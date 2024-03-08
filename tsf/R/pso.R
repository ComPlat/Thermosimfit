# particle swarm optimization
pso <- function(env, lb, ub, loss, ngen, npop, error_threshold, global = FALSE, 
                saveSwarm = FALSE, runAsShiny = NULL) {
  stopifnot(length(lb) == length(ub))
  npar = length(lb)
  swarm <- matrix(0, nrow = npop, ncol = npar)
  v <- matrix(0, nrow = npop, ncol = npar)
  swarm_bests <- numeric(npop)
  swarm_errors <- numeric(npop)
  initial_cog = 2.5
  final_cog = 0.5
  initial_soc = 0.5
  final_soc = 2.5
  w = 0.5
  w_max = 0.9
  w_min = 0.4
  
  memory <- matrix(0, nrow = ngen * npop, ncol = npar)
  error_memory <- numeric(ngen * npop)
  
  for (i in seq(npop)) {
    swarm[i, ] <-  runif(npar, min = lb, max = ub)
    swarm_errors[i] <- loss(swarm[i, ], env)
    swarm_bests[i] <- swarm_errors[i]
  }

  global_best <- which.min(swarm_bests)
  global_best_vec <- swarm[global_best, ]
  global_best_error <- swarm_bests[global_best]
  swarm_best_params <- swarm
  K <- 3
  
  correctBelowLB <- function(target, threshold) {
    for (i in 1:length(target)) {
      if (target[i] < threshold[i]) target[i] <- threshold[i]
    }
    return(target)
  }
  
  correctAboveUB <- function(target, threshold) {
    for (i in 1:length(target)) {
      if (target[i] > threshold[i]) target[i] <- threshold[i]
    }
    return(target)
  }
  
  calc_neighberhood <- function() {
    neighberhood <- matrix(0L, ncol = K, nrow = npop)	
    for (i in seq(npop)) {
      nneighbour <- sample(1:K, 1)
      neighbours <- sample(1:npop, nneighbour)
      if (length(neighbours) < K) {
        diff = K - length(neighbours)
        neighbours <- c(neighbours, rep(NA, diff))
      }
      neighberhood[i, ] <- neighbours
    }
    neighberhood
  }
  neighberhood <- calc_neighberhood()
  convergence_check <- 0
  
  iter <- 1
  while (iter < ngen) {
    if (iter == 1 || convergence_check != 0) {
      neighberhood <- calc_neighberhood()
    }
    
    if(saveSwarm) memory[((iter * npop) + 1):((iter + 1) * npop), ] <- swarm
    
    w <- w_max - iter * (w_max - w_min) / ngen;
    cog <- initial_cog - (initial_cog - final_cog) * (iter + 1) / ngen;
    soc <- initial_soc - (initial_soc - final_soc) * (iter + 1) / ngen;

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
      
      swarm[i, ] <- correctBelowLB(swarm[i, ], lb)
      swarm[i, ] <- correctAboveUB(swarm[i, ], ub) 

      error <- loss(swarm[i, ], env)
      
      if(saveSwarm) error_memory[((iter * npop) + i)] <- error
      
      if (!is.infinite(error) & !is.na(error) &
         error < swarm_bests[i]) {
        swarm_bests[i] <- error
        swarm_best_params[i, ] <- swarm[i, ]
      }
      if (!is.infinite(error) & !is.na(error) &
         error < global_best_error) {
        global_best <- i
        global_best_vec <- swarm[i, ]
        global_best_error <- error
        #convergence_check <- 0
      }
      convergence_check <- convergence_check + 1
    }
    
    iter <- iter + 1
    
    if (iter %% 1 == 0 & is.logical(runAsShiny)) {
       print(iter)
       print(global_best_vec)
       print(global_best_error)
     } else if(is(runAsShiny, "Communicator")) {
       if(iter %% 5 == 0) {
          status <- runAsShiny$getStatus()
          if (status == "interrupt") {
            insilico <- loss(global_best_vec, env, TRUE)
            return(list(insilico, c(global_best_vec)))  
          }
          gbv <- c(kX = global_best_vec[1], I0 = global_best_vec[2],
                   IHD = global_best_vec[3], ID = global_best_vec[4])
          gbv <- Map(function(a, b) {
            paste(a, " = ", b)
          }, names(gbv), gbv) |> unlist() |> paste(collapse = ", ")
          runAsShiny$running((100/ngen) * iter)
          runAsShiny$setData(
            paste(
              paste0((100/ngen) * iter, "% completed;"),
              gbv,
              "; Error: ", global_best_error,
              collapse = "\n"
            )
          )
       }
    }
    
    if(global_best_error < error_threshold) {
      break
    }
  }
  
  insilico <- loss(global_best_vec, env, TRUE)
  if(saveSwarm) {
    return(list(insilico, c(global_best_vec), 
                memory, error_memory))	  
  } 
  return(list(insilico, c(global_best_vec)))	
}
