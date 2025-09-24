library(ggplot2)
library(cowplot)

get_data <- function(path) {
  load(path)
  parameter <- lapply(seq_len(length(result)), function(idx) {
    res <- result[[idx]][[2]]
    return(res)
  })
  parameter <- Reduce(rbind, parameter)
  parameter <- apply(parameter, 2, mean)
  parameter <- as.numeric(t(parameter))
  df <- result[[1]][[1]]
  ap <- result[[1]]$additionalParameters
  return(
    list(
      parameter = parameter,
      data = df,
      additionalParameters = ap
    )
  )
}

# Creates the contour plot of the interaction
# that explain the most variance
create_contour_plot <- function(case, params, env, pi) {
  lossFct <- tryCatch(
    expr = {
      if (case == "dba_host_const") {
        tsf:::lossFctHG
      } else if (case == "dba_dye_const") {
        tsf:::lossFctDBA
      } else if (case == "ida") {
        tsf:::lossFctIDA
      } else if (case == "gda") {
        tsf:::lossFctGDA
      }
    },
    error = function(e) {
      return(NULL)
    },
    interrupt = function(e) {
      return(NULL)
    }
  )

  names <- c("Ka(HD) [1/M]", "I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  if (case == "ida" || case == "gda") {
    names <- c("Ka(HG) [1/M]", "I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  }

  all <- 1:4
  ui <- all[!all %in% pi]
  params_list <- list()
  for (i in 1:4) {
    if (i %in% ui) {
      params_list[[i]] <- params[i]
    } else {
      lb <- params[i] * 0.7
      ub <- params[i] * 1.3
      if (case == "gda" && i == 2) {
        lb <- 0
        ub <- 75
      }
      params_list[[i]] <- seq(
        lb,
        ub,
        length.out = 20
      )
    }
  }

  names(params_list) <- names
  grid <- expand.grid(params_list)
  grid$error <- apply(grid, 1, function(row) {
    lossFct(row, env, FALSE)
  })
  p <- ggplot(
    data = grid,
    aes(
      x = grid[, pi[1]],
      y = grid[, pi[2]],
      z = error
    )
  ) +
    geom_contour_filled(binwidth = 0.5) +
    labs(
      x = names[pi[1]],
      y = names[pi[2]], fill = "Rel. Error"
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 12)
    )
  return(list(plot = p, grid = grid))
}

plot_dba <- function(path) {
  data <- get_data(path)
  df <- data$data
  parameter <- data$parameter
  ap <- data$additionalParameters
  env <- new.env()
  env$host <- df[, 1]
  env$signal <- df[, 2]
  env$d0 <- ap[1]
  pi <- c(1, 3) # Ka(HD), I(HD)
  create_contour_plot("dba_dye_const", parameter, env, pi)
}

plot_ida <- function(path) {
  data <- get_data(path)
  df <- data$data
  parameter <- data$parameter
  ap <- data$additionalParameters
  env <- new.env()
  env$ga <- df[, 1]
  env$signal <- df[, 2]
  env$h0 <- ap[1]
  env$d0 <- ap[2]
  env$kd <- ap[3]
  pi <- c(1, 3) # Ka(HG), I(HD)
  create_contour_plot("ida", parameter, env, pi)
}

plot_gda <- function(path) {
  data <- get_data(path)
  df <- data$data
  parameter <- data$parameter
  ap <- data$additionalParameters
  env <- new.env()
  env$dye <- df[, 1]
  env$signal <- df[, 2]
  env$h0 <- ap[1]
  env$ga0 <- ap[2]
  env$kd <- ap[3]
  pi <- c(1, 3) # Ka(HG), I(HD)
  create_contour_plot("gda", parameter, env, pi)
}

p_dba_cp <- plot_dba("../DecentFitParameterVariance/DBA_10_different_seeds.RData")
p_ida_cp <- plot_ida("../DecentFitParameterVariance/IDA_10_different_seeds.RData")
p_gda_cp <- plot_gda("../DecentFitParameterVariance/GDA_10_different_seeds.RData")
save(p_dba_cp, p_ida_cp, p_gda_cp, file = "ContourPlots.RData")
