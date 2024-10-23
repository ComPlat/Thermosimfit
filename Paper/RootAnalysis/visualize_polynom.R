# DBA
# =============================================
load("../DBA_10_different_seeds.RData")
seeds <- lapply(result, function(x) x$seed)
parameter <- lapply(seq_len(length(result)), function(idx) {
  res <- result[[idx]][[2]]
  res$seed <- seeds[[idx]]
  return(res)
})
parameter <- Reduce(rbind, parameter)
parameter <- apply(parameter, 2, mean) |>
  as.numeric() |>
  (\(x) x[1])()
kd <- parameter[1]

d0 <- result[[1]]$additionalParameters[[1]]

host <- result[[1]]$data[, 1]
h0 <- host[25]

kd_values <- seq(10, 10^9, length.out = 5)

generate_curve_hd <- function(kd, d0, h0) {
  curve(((x^2 + (-h0 - d0) * x + d0 * h0) * kd - x),
    from = 0, to = h0,
    ylab = "Polynomial Value",
    xlab = "hd",
    main = paste("Curve with kd =", kd, ", d0 =", d0, ", h0 =", h0)
  )
}

par(mfrow = c(3, 2))
for (kd_i in kd_values) {
  generate_curve_hd(kd_i, d0, h0)
}

generate_curve_d <- function(kd, d0, h0) {
  curve(((x * h0 - x * d0 + x^2) * kd - d0 + x),
    from = 0, to = h0,
    ylab = "Polynomial Value",
    xlab = "d",
    main = paste("Curve with kd =", kd, ", d0 =", d0, ", h0 =", h0)
  )
}
par(mfrow = c(3, 2))
for (kd_i in kd_values) {
  generate_curve_d(kd_i, d0, h0)
}


# IDA
# =============================================
load("../IDA_10_different_seeds.RData")
seeds <- lapply(result, function(x) x$seed)
parameter <- lapply(seq_len(length(result)), function(idx) {
  res <- result[[idx]][[2]]
  res$seed <- seeds[[idx]]
  return(res)
})
parameter <- Reduce(rbind, parameter)
parameter <- apply(parameter, 2, mean) |>
  as.numeric() |>
  (\(x) x[1])()
kga <- parameter[1]

h0 <- result[[1]]$additionalParameters[[1]]
d0 <- result[[1]]$additionalParameters[[2]]
kd <- result[[1]]$additionalParameters[[3]]

guest <- result[[1]]$data[, 1]
ga0 <- guest[16]

# NOTE: hd
curve((-(((
  x^3 + (-h0 + ga0 - d0) * x^2 + (d0 * h0 - d0 * ga0) * x
) * kd - x^2) * kga)
- (-x^3 + (h0 + 2 * d0) * x^2 + (-(2 * d0 * h0) - d0^2) * x +
    d0^2 * h0) * kd^2 - (x^2 - d0 * x) * kd))
# NOTE: d
curve(((x * kd + 1) * ((((x * d0 - x^2) * h0 + d0 * (2 * x^2 - x * ga0) + x^2 *
  ga0 - x * d0^2 - x^3
) * kd
  - d0^2 + 2 * x * d0 - x^2)
* kga
  + (x^2 * h0 - x^2 * d0 + x^3) * kd^2 + (x^2 - x *
    d0) * kd
)))
