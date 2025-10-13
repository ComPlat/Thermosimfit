library(ggplot2)
library(rootSolve)
library(nnls)

files <- list.files("./tsf/R", full.names = TRUE)
trash <- lapply(files, source)

data <- readLines("~/Documents/Thermosimfit/Tests/GlobalAnalysis/Full_Spectrum_all_Replicas.csv")
data <- data[1:18] # First repetition
data <- lapply(data, function(line) {
  strsplit(line, ",")[[1]]
})
names <- data[[1]]
data <- do.call(rbind, data[2:length(data)]) |> as.data.frame()
names(data) <- names
data <- data[, c(-7, -8)] # Removed these as data times instead of numbers were found
df <- apply(data, 2, as.numeric) |> as.data.frame()
df[[1]] <- df[[1]] * 10^6 # M to µM

lossFctIDA <- function(parameter, env, eval = FALSE) {
  # Non linear part: Ka optimized
  # =======================================================================
  calc_hd_and_d <- function(kga, kd, h0, d0, ga) {
    hdFct <- function(hd) {
      # from ida_gda_eliminate_hd.mac
      (-(((
        hd^3 + (-h0 + ga0 - d0) * hd^2 + (d0 * h0 - d0 * ga0) * hd
      ) * kd - hd^2) * kga)
        - (-hd^3 + (h0 + 2 * d0) * hd^2 + (-(2 * d0 * h0) - d0^2) * hd +
          d0^2 * h0) * kd^2 - (hd^2 - d0 * hd) * kd)
    }
    dFct <- function(d) {
      # from ida_gda_eliminate_d.mac
      ((d * kd + 1) * ((((d * d0 - d^2) * h0 + d0 * (2 * d^2 - d * ga0) + d^
        2 * ga0 - d * d0^2 - d^3
      ) * kd
        - d0^2 + 2 * d * d0 - d^2)
        * kga
        + (d^2 * h0 - d^2 * d0 + d^3) * kd^2 + (d^2 - d *
          d0) * kd
      ))
    }
    ga <- ifelse(ga == 0, 10^-15, ga)
    d <- numeric(length(ga))
    hd <- numeric(length(ga))
    for (i in seq_along(ga)) {
      ga0 <- ga[i]
      max <- d0
      hdRoot <- uniroot.all(hdFct, c(0, max),
        tol = .Machine$double.eps^15,
        maxiter = 10000, n = 1000
      )
      if (length(hdRoot) > 1) hdRoot <- hdRoot[length(hdRoot)]
      if (hdRoot > h0) {
        hdRoot <- h0
      } else if (hdRoot > d0) {
        hdRoot <- d0
      }
      dRoot <- uniroot.all(dFct, c(0, max),
        tol = .Machine$double.eps^15,
        maxiter = 10000, n = 1000
      )
      if (length(dRoot) > 1) dRoot <- dRoot[length(dRoot)]
      if (dRoot > d0) dRoot <- d0
      d[i] <- dRoot
      hd[i] <- hdRoot
    }
    list(d = d, hd = hd)
  }
  d_hd <- calc_hd_and_d(parameter, env$kd, env$h0, env$d0, env$ga)

  # Linear part: determine the optimal I parameters
  # =======================================================================
  X <- cbind(Intercept = 1, hd = d_hd$hd, d = d_hd$d)
  betas <- list()

  fit_one <- function(y) {
    eps <- 1e-12
    w   <- 1 / pmax(abs(y), eps)
    Xw <- X * w
    yw <- y * w
    fit <- nnls(Xw, yw)
    beta <- coef(fit) # all >= 0
    betas <<- c(betas, beta)
    as.vector(X %*% beta)
  }
  # Run non negative least square for each signal independently to determine Is
  if (!is.data.frame(env$signal)) env$signal <- as.data.frame(env$signal)
  insilico_signals <- lapply(1:ncol(env$signal), function(x) {
    fit_one(env$signal[[x]])
  })

  if (eval) {
    insilico_mat <- Reduce(cbind, insilico_signals)
    return(list(
      data.frame(insilico_mat, d = d_hd$d, hd = d_hd$hd),
      c(parameter, do.call(c, betas))
    ))
  }
  # Calc mean error across all signals
  # =======================================================================
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}

env <- new.env()
env$h0 <- 4.3
env$d0 <- 6
env$ga <- df[[1]]
env$signal <- df[, -1]
env$kd <- 1.7e1
env$error_calc_fct <- tsf:::rel_err
env$n_sigs <- dim(df) - 1
grid <- seq(0, 10^5, length.out = 1000)
errors <- vapply(grid, function(ka) {
  lossFctIDA(ka, env)
}, numeric(1))
best <- grid[[which.min(errors)]]
best
plot(grid, errors)
res <- lossFctIDA(best, env, TRUE)

df_m <- data.frame(
  x = rep(df[[1]], 249),
  y = stack(df[, -1])$values,
  ind = stack(df, -1)$ind, group = "measured"
)
df_is <- res[[1]][, 1:249]
names(df_is)[1:249] <- names(df)[2:250]
df_is <- data.frame(
  x = df_m$x,
  y = stack(df_is)$values, ind = stack(df_is)$ind,
  group = "in silico"
)
df_comp <- rbind(df_m, df_is)
df_comp$ind <- as.numeric(df_comp$ind)
sub <- df_comp[df_comp$ind %in% c(1, 50, 100, 150, 245), ]
ggplot(data = sub) +
  geom_point(aes(x, y, colour = group)) +
  facet_wrap(~ ind, scales = "free")
