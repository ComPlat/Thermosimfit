# eqns: [0 = h + hd + hga -h0,
#        0 = d + hd -d0,
#        0 = ga + hga -ga0,
#        0 = hga / (h*ga) -kga,
#        0 = hd / (h*d) -kd],

# denom Kd
# Eq Nr.1
# Kd = hd / (h * d)
# hd = Kd * h * d
# Eq Nr.2
# d0 = d + hd
# d0 = d + Kd * h * d
# d0 = d * (1 + Kd * h)
# d = d0 / (1 + Kd * h)
# Back substitution in Eq Nr.1
# hd = Kd * h * d
# hd = Kd * h * (d0 / (1 + Kd * h))

# denom Kg
# Eq Nr.1
# Kg = hg / (h * g)
# hg = Kg * h * g
# Eq Nr.2
# g0 = g + hg
# g0 = g + Kg * h * g
# g0 = g * (1 + Kg * h)
# g = g0 / (1 + Kg * h)
# Back substitution in Eq Nr. 1
# hg = Kg * h * (g0 / (1 + Kg * h))
library(ggplot2)

forward_ida <- function(Kd, Kg, Id, Ihd, h0, d0, g0_values) {
  valid_g0 <- c()
  Signal_values <- c()

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

  results_table <- data.frame(
    g0 = valid_g0,
    Signal = Signal_values
  )

  return(results_table)
}

load("../IDA_10_different_seeds.RData")

parameter <- lapply(seq_len(length(result)), function(idx) {
  res <- result[[idx]][[2]]
  return(res)
})
parameter <- Reduce(rbind, parameter)
parameter <- apply(parameter, 2, mean)
# Example usage
Kd <- 1.70E+07
Kg <- parameter[1]
Id <- parameter[4]
Ihd <- parameter[3]
h0 <- 4.3 * 10^-6
d0 <- 6 * 10^-6
g0_values <- result[[1]]$data[, 1]
signal_measured <- result[[1]]$data[, 2]
signal_simulated <- result[[1]]$data[, 3]

results <- forward_ida(Kd, Kg, Id, Ihd, h0, d0, g0_values)
df <- data.frame(
  guest = g0_values,
  signal_measured = signal_measured,
  signal_simulated = signal_simulated,
  signal_forward = results[, 2]
)
df <- tidyr::pivot_longer(df, cols = -c(guest))
df

ggplot(
  data = df,
  aes(x = guest, y = value, colour = name)
) +
  geom_point()
