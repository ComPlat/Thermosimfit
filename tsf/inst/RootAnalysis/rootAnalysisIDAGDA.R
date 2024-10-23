library(ggplot2)
library(rootSolve)

ida_root <- function(kga, kd, h0, d0, ga0) {
  hd_fct <- function(hd) {
    (-(((
      hd^3 + (-h0 + ga0 - d0) * hd^2 + (d0 * h0 - d0 * ga0) * hd
    ) * kd - hd^2) * kga)
    - (-hd^3 + (h0 + 2 * d0) * hd^2 + (-(2 * d0 * h0) - d0^2) * hd +
        d0^2 * h0) * kd^2 - (hd^2 - d0 * hd) * kd)
  }
  d_fct <- function(d) {
    ((d * kd + 1) * ((((d * d0 - d^2) * h0 + d0 * (2 * d^2 - d * ga0) + d^
      2 * ga0 - d * d0^2 - d^3
    ) * kd
      - d0^2 + 2 * d * d0 - d^2)
    * kga
      + (d^2 * h0 - d^2 * d0 + d^3) * kd^2 + (d^2 - d *
        d0) * kd
    ))
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
density <- 10
d0_vals <- seq(10^-15, 1, length.out = density)
h0_vals <- seq(10^-15, 1, length.out = density)
ga0_vals <- seq(10^-15, 1, length.out = density)
kd_vals <- seq(0, 10^9, length.out = density)
kga_vals <- seq(0, 10^9, length.out = density)
grid <- expand.grid(
  kga = kga_vals,
  kd = kd_vals,
  h0 = h0_vals,
  d0 = d0_vals,
  ga0 = ga0_vals
)
result <- lapply(1:nrow(grid), function(idx) {
  if (idx %% 1000 == 0) {
    print(paste0((100 / nrow(grid)) * idx, "%"))
  }
  x <- grid[idx, ]
  res <- ida_root(x$kga, x$kd, x$h0, x$d0, x$ga0)
  if (length(res$d_roots) > 1) {
    warning("Found more than one d root")
  }
  if (length(res$hd_roots) > 1) {
    warning("Found more than one hd root")
  }
  if (length(res$d_roots) == 0) {
    res$d_roots <- NA
    warning("Found no d root")
  }
  if (length(res$hd_roots) == 0) {
    res$hd_roots <- NA
    warning("Found no hd root")
  }
  data.frame(
    d_root = res$d_roots,
    hd_root = res$hd_roots,
    kga = x$kga,
    kd = x$kd,
    h0 = x$h0,
    d0 = x$d0,
    ga0 = x$ga0
  )
})

save(result, file = "ida_gda_root.RData")
result <- do.call(rbind, result)
save(result, file = "ida_gda_root_combined.RData")
