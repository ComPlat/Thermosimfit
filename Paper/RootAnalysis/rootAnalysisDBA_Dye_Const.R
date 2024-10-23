library(ggplot2)
library(rootSolve)

dba_dye_const_root <- function(h0, kd, d0) {
  hd_fct <- function(hd) {
    ((hd^2 + (-h0 - d0) * hd + d0 * h0) * kd - hd)
  }
  d_fct <- function(d) {
    ((d * h0 - d * d0 + d^2) * kd - d0 + d)
  }

  h0 <- ifelse(h0 == 0, 10^-15, h0)

  hd_root <- uniroot.all(
    hd_fct, c(0, h0),
    tol = .Machine$double.eps^15, maxiter = 10000, n = 1000
  )
  d_root <- uniroot.all(
    d_fct, c(0, d0),
    tol = .Machine$double.eps^15, maxiter = 10000, n = 1000
  )

  return(list(d_roots = d_root, hd_roots = hd_root))
}

# Define grid of kd and d0 values
# NOTE: assume that all values are in M
density <- 20
d0_vals <- seq(10^-15, 1, length.out = density)
h0_vals <- seq(10^-15, 1, length.out = density)
kd_vals <- seq(0, 10^9, length.out = density)
grid <- expand.grid(
  h0 = h0_vals,
  kd = kd_vals,
  d0 = d0_vals
)
result <- apply(grid, 1, function(x) {
  res <- dba_dye_const_root(x[1], x[2], x[3])
  if (length(res$d_roots) > 1) {
    stop("Found more than one d root")
  }
  if (length(res$hd_roots) > 1) {
    stop("Found more than one hd root")
  }
  data.frame(
    d_root = res$d_roots,
    hd_root = res$hd_roots,
    h0 = x[1],
    kd = x[2],
    d0 = x[3]
  )
})

result <- do.call(rbind, result)

ggplot() +
  geom_point(
    data = result,
    aes(
      x = d0,
      y = kd,
      color = d_root
    )
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(x = "D0", y = "kd", color = "D root")

ggplot() +
  geom_point(
    data = result,
    aes(
      x = h0,
      y = kd,
      color = hd_root
    )
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(x = "H0", y = "kd", color = "HD root")

ggplot() +
  geom_point(
    data = result,
    aes(
      x = h0,
      y = d0,
      color = hd_root
    )
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(x = "H0", y = "d0", color = "HD root")

ggplot() +
  geom_point(
    data = result,
    aes(
      x = h0,
      y = d0,
      color = d_root
    )
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(x = "H0", y = "d0", color = "D root")
