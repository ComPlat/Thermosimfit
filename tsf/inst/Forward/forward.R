# Load necessary libraries
library(ggplot2)

# Define the function
forward_ida <- function(Kd, Kg, Id, Ihd, h0, d0, g0_values) {
  valid_g0 <- c()
  Signal_values <- c()

  # Function to calculate residual for h
  equation_h <- function(h, Kd, Kg, h0, d0, g0) {
    if (h <= 0) {
      return(Inf)
    }
    denom_Kd <- 1 + Kd * h
    if (denom_Kd == 0) {
      return(Inf)
    }
    h_d <- (Kd * h * d0) / denom_Kd
    if (g0 == 0) {
      h_g <- 0
    } else {
      denom_Kg <- 1 + Kg * h
      if (denom_Kg == 0) {
        return(Inf)
      }
      h_g <- (Kg * h * g0) / denom_Kg
    }
    residual <- h + h_d + h_g - h0
    return(residual)
  }

  # Solve for h using uniroot for each value of g0
  for (g0 in g0_values) {
    try(
      {
        h_sol <- uniroot(
          f = equation_h,
          lower = 1e-20,
          upper = h0,
          tol = 1e-14,
          Kd = Kd,
          Kg = Kg,
          h0 = h0,
          d0 = d0,
          g0 = g0
        )$root

        if (h_sol <= 0) {
          print(paste("Invalid h for g0 =", format(g0, scientific = TRUE), "Skipping."))
          next
        }

        denom_Kd <- 1 + Kd * h_sol
        d_sol <- d0 / denom_Kd
        if (d_sol <= 0) {
          print(paste("Invalid d for g0 =", format(g0, scientific = TRUE), "Skipping."))
          next
        }

        h_d_sol <- Kd * h_sol * d_sol
        Signal <- Id * d_sol + Ihd * h_d_sol

        valid_g0 <- c(valid_g0, g0)
        Signal_values <- c(Signal_values, Signal)
      },
      silent = TRUE
    )
  }

  # Create a data frame with the results
  results_table <- data.frame(
    g0 = valid_g0,
    Signal = Signal_values
  )

  # Print the results
  print("Computed Signal values:")
  print(results_table)

  # Plot the results
  ggplot(results_table, aes(x = g0, y = Signal)) +
    geom_line() +
    geom_point() +
    labs(title = "Signal vs. g0", x = "g0 (M)", y = "Signal (arbitrary units)") +
    theme_minimal()

  return(results_table)
}

# Example usage
Kd <- 1e9
Kg <- 1e9
Id <- 1e6
Ihd <- 1e7
h0 <- 1e-9
d0 <- 2e-9
g0_values <- c(0.0, 2e-9, 4e-9, 6e-9, 8e-9, 1e-8, 2e-8, 4e-8)

results <- forward_ida(Kd, Kg, Id, Ihd, h0, d0, g0_values)
