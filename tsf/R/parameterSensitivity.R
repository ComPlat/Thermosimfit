
# is also called variance based sensitivity analysis
# https://en.wikipedia.org/wiki/Variance-based_sensitivity_analysis
# issue: check again
calculateSobolIndices <- function(lossFct, env, perturbed, meanError, varError) {
  num_params <- ncol(perturbed)
  sobol_indices <- numeric(num_params)
  
  for (i in 1:num_params) {
    perturbed_high <- perturbed
    perturbed_low <- perturbed
    perturbed_high[, i] <- perturbed_high[, i] + perturbed_high[, i] * 0.1
    perturbed_low[, i] <- perturbed_low[, i] - perturbed_low[, i] * 0.1
    
    errors_high <- apply(perturbed_high, 1, function(row) lossFct(row, env, FALSE))
    errors_low <- apply(perturbed_low, 1, function(row) lossFct(row, env, FALSE))
    
    sobol_indices[i] <- mean((errors_high - meanError) * (errors_low - meanError)) / (2 * varError)
  }
  
  return(sobol_indices)
}

sensitvityPlot <- function(parameterCol, Errors) {
  df <- data.frame(parameter = parameterCol, error = Errors)
  ggplot(df, aes(x = parameter, y = error)) +
    geom_point()
}


#' Optimize algebraic systems which describe thermodynamic binding systems
#'
#' @export
#' @import rootSolve
#' @import ggplot2
#' @param case is a character describing which system should be investigated. Either: "hg", "ida" or "gda".
#' @param parameters is a numeric vector containing already optimized parameter. 
#'        In case of *hg* the order of the parameters is: *khd*, *I0*, *IHD* and *ID*
#'        In case of *ida* and *ga* the order of the parameters is: *kg*, *I0*, *IHD* and *ID*.
#' @param path is a filepath which contains tabular x-y data. The concentraion of dye or guest respectivly is assumed to be in the first colum. Furthermore, should the corresponding signal be stored in the second column. 
#' @param additionalParameters are required parameters which are specific for each case.
#'        In case of *hg* a numeric vector of length 1 is expected which contains the concentration of the host.
#'        In case of *ida* a numeric vector of length 3 is expected which contains the concentration of the host, dye and the *khd* parameter.
#'        In case of *gda* a numeric vector of length 3 is expected which contains the concentration of the host, guest and the *khd* parameter.
#' @return either an instance of ErrorClass if something went wrong. Otherwise plots showing the sensitivity are returned.
#' @examples
#' path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
#' res <- opti("ida", c(1, 0, 0, 0), c(10^9, 10^6, 10^6, 10^6), path, c(4.3, 6.0, 7079458)) 
#' sensitivity("ida", res[[2]], path, c(4.3, 6.0, 7079458))
sensitivity <- function(case, parameters, path, additionalParameters) {
  if(!is.character(case)) return(ErrorClass$new("case has to be of type character"))
  if(!(case %in% c("hg", "ida", "gda"))) return(ErrorClass$new("case is neither hg, ida or gda"))
  if(!is.data.frame(parameters)) return(ErrorClass$new("optimizedParameters have to be of type numeric"))
  if(length(parameters) == 0) return(ErrorClass$new("optimizedParameters vector seems to be empty"))
  if(length(parameters) > 4) return(ErrorClass$new("optimizedParameters vector has more than 4 entries"))
  if(case == "hg" && length(additionalParameters) != 1) return(ErrorClass$new("additionalParameters have to be of length 1"))
  if(case == "ida" && length(additionalParameters) != 3) return(ErrorClass$new("additionalParameters have to be of length 3"))
  if(case == "gda" && length(additionalParameters) != 3) return(ErrorClass$new("additionalParameters have to be of length 3"))
  df <- try(importData(path))
  if (class(df) == "try-error") return(ErrorClass$new("Could not read file"))
  parameters <- as.numeric(parameters)
  env <- new.env()
  if(case == "hg") {
    names(df) <- c("dye", "signal")
    lossFct <- lossFctHG
    env$dye <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
  } else if(case == "ida") {
    names(df) <- c("guest", "signal")
    lossFct <- lossFctIDA
    env$ga <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
    env$d0 <- additionalParameters[2]
    env$kd <- additionalParameters[3]
  } else if(case == "gda") {
    names(df) <- c("dye", "signal")
    lossFct <- lossFctGDA
    env$dye <- df[, 1]
    env$signal <- df[, 2]
    env$h0 <- additionalParameters[1]
    env$ga0 <- additionalParameters[2]
    env$kd <- additionalParameters[3]
  }
  perturbationFactor <- 0.1
  n <- 1000
  perturbed <- matrix(nrow = n, ncol = length(parameters))
  errors <- numeric(n)
  for (i in 1:n) {
    temp <- parameters * (1 + perturbationFactor*
         (runif(length(parameters)) - 0.5))
    perturbed[i, ] <- temp
    errors[i] <- lossFct(temp, env, FALSE)
  }
  
  meanError <- mean(errors)
  varError <- var(errors)
  sobol_indices <- calculateSobolIndices(lossFct, env,
                                         perturbed, meanError, varError)
  
  p <- list()
  for (i in 1:length(parameters)) {
    p[[i]] <- sensitvityPlot(perturbed[, i], errors)
  }
  sobol_plot <- ggplot(data.frame(parameter = 1:length(parameters), sobol = sobol_indices),
                       aes(x = factor(parameter), y = sobol)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    ggtitle("Sobol Indices")
  p[[length(parameters) + 1]] <- sobol_plot
  
  return(p)
}