# TODO: write tests
forward_dba_dye_const <- function(Kd, Id, Ihd, d0, h0_values) {
  valid_h0 <- c()
  Signal_values <- c()
  for (h0 in h0_values) {
    try(
      {
        h_sol <- uniroot(
          f = equation_h_dba,
          lower = 1e-20,
          upper = h0,
          tol = 1e-14,
          Kd = Kd,
          h0 = h0,
          d0 = d0
        )$root

        if (h_sol <= 0) {
          print(paste("Invalid h for h0 =", format(h0, scientific = TRUE), "Skipping."))
          next
        }

        denom_Kd <- 1 + Kd * h_sol
        d_sol <- d0 / denom_Kd
        if (d_sol <= 0) {
          print(paste("Invalid d for h0 =", format(h0, scientific = TRUE), "Skipping."))
          next
        }

        h_d_sol <- Kd * h_sol * d_sol
        Signal <- Id * d_sol + Ihd * h_d_sol

        valid_h0 <- c(valid_h0, h0)
        Signal_values <- c(Signal_values, Signal)
      },
      silent = TRUE
    )
  }

  results_table <- data.frame(
    h0 = valid_h0,
    Signal = Signal_values
  )

  return(results_table)
}

forward_dba_host_const <- function(Kd, Id, Ihd, h0, d0_values) {
  valid_d0 <- c()
  Signal_values <- c()
  for (d0 in d0_values) {
    try(
      {
        h_sol <- uniroot(
          f = equation_h_dba,
          lower = 1e-20,
          upper = d0,
          tol = 1e-14,
          Kd = Kd,
          h0 = h0,
          d0 = d0
        )$root

        if (h_sol <= 0) {
          print(paste("Invalid h for d0 =", format(d0, scientific = TRUE), "Skipping."))
          next
        }

        denom_Kd <- 1 + Kd * h_sol
        d_sol <- d0 / denom_Kd
        if (d_sol <= 0) {
          print(paste("Invalid d for d0 =", format(d0, scientific = TRUE), "Skipping."))
          next
        }

        h_d_sol <- Kd * h_sol * d_sol
        Signal <- Id * d_sol + Ihd * h_d_sol

        valid_d0 <- c(valid_d0, d0)
        Signal_values <- c(Signal_values, Signal)
      },
      silent = TRUE
    )
  }
  results_table <- data.frame(
    d0 = valid_d0,
    Signal = Signal_values
  )

  return(results_table)
}


forward_ida <- function(Kd, Kg, Id, Ihd, h0, d0, g0_values) {
  valid_g0 <- c()
  Signal_values <- c()
  for (g0 in g0_values) {
    try(
      {
        h_sol <- uniroot(
          f = equation_h_ida_gda,
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

  results_table <- data.frame(
    g0 = valid_g0,
    Signal = Signal_values
  )

  return(results_table)
}

forward_gda <- function(Kd, Kg, Id, Ihd, h0, g0, d0_values) {
  valid_d0 <- c()
  Signal_values <- c()

  for (d0 in d0_values) {
    try(
      {
        h_sol <- uniroot(
          f = equation_h_ida_gda,
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
          print(paste("Invalid h for d0 =", format(d0, scientific = TRUE), "Skipping."))
          next
        }

        denom_Kd <- 1 + Kd * h_sol
        d_sol <- d0 / denom_Kd
        if (d_sol <= 0) {
          print(paste("Invalid d for d0 =", format(d0, scientific = TRUE), "Skipping."))
          next
        }

        h_d_sol <- Kd * h_sol * d_sol
        Signal <- Id * d_sol + Ihd * h_d_sol

        valid_d0 <- c(valid_d0, d0)
        Signal_values <- c(Signal_values, Signal)
      },
      silent = TRUE
    )
  }

  results_table <- data.frame(
    d0 = valid_d0,
    Signal = Signal_values
  )

  return(results_table)
}
