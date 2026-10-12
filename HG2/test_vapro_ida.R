library(rootSolve)
library(ggplot2)
library(nnls)

ps <- function(v) {
  sapply(v, function(elem) {
    formatC(elem)
  })
}

rel_err   <- function(yhat, y) {
  eps <- 1e-12
  sum(abs(y - yhat) / pmax(abs(y), eps))
}

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
    fit <- nnls(Xw, yw) # Non-Negative Least Squares
    beta <- coef(fit) # all >= 0
    betas <<- c(betas, beta)
    as.vector(X %*% beta)
  }
  if (!is.data.frame(env$signal)) env$signal <- as.data.frame(env$signal)
  # Run non negative least square for each signal independently to determine Is
  insilico_signals <- lapply(1:ncol(env$signal), function(x) {
    fit_one(env$signal[[x]])
  })

  if (eval) {
    insilico_mat <- Reduce(cbind, insilico_signals)
    return(list(
      data.frame(insilico_mat, d = d_hd$d, hd = d_hd$hd),
      do.call(c, betas)
    ))
  }
  # Calc mean error across all signals
  # =======================================================================
  return(mean(
    Map(env$error_calc_fct, insilico_signals, as.list(env$signal)) |> unlist(),
    na.rm = TRUE
  ))
}

beta_param_names <- function(K) {
  as.vector(t(outer(seq_len(K), c("I0", "Ihd", "Id"),
    function(k, p) paste0(p, "_sig", k))))
}

vapro_fit <- function(env, kga_lower = 1e3, kga_upper = 1e9, tol = .Machine$double.eps^0.5) {
  opt <- optimize(lossFctIDA, interval = c(kga_lower, kga_upper),
    env = env, tol = tol)
  kga_hat <- opt$minimum
  ev <- lossFctIDA(kga_hat, env, eval = TRUE)
  K <- ncol(env$signal)
  betas <- ev[[2]]
  names(betas) <- beta_param_names(K)
  list(
    kga    = kga_hat,
    betas  = betas,
    fitted = ev[[1]],
    loss   = opt$objective
  )
}

vapro_bootstrap <- function(env, kga_hat, n_boot = 500, sigma = NULL,
                            kga_lower = 1e3, kga_upper = 1e9, seed) {
  ev <- lossFctIDA(kga_hat, env, eval = TRUE)
  K <- ncol(env$signal)
  N <- nrow(env$signal)
  fitted_signals <- as.matrix(ev[[1]][, seq_len(K), drop = FALSE])
  observed <- as.matrix(env$signal)
  resid <- observed - fitted_signals
  if (is.null(sigma)) {
    sigma <- apply(resid, 2, sd)
    message("Estimated noise sigma per signal: ",
      paste(formatC(sigma, digits = 3), collapse = ", "))
  } else if (length(sigma) == 1) {
    sigma <- rep(sigma, K)
  }

  kga_boot <- rep(NA_real_, n_boot)
  beta_boot <- matrix(NA_real_, nrow = n_boot, ncol = 3 * K)
  loss_boot <- rep(NA_real_, n_boot)
  fail <- 0

  for (b in seq_len(n_boot)) {
    noise <- matrix(rnorm(N * K, 0, sd = rep(sigma, each = N)), nrow = N, ncol = K)
    boot_env <- env
    boot_env$signal <- as.data.frame(fitted_signals + noise)
    res <- tryCatch({
      opt <- optimize(lossFctIDA, interval = c(kga_lower, kga_upper), env = boot_env)
      ev_b <- lossFctIDA(opt$minimum, boot_env, eval = TRUE)
      list(kga = opt$minimum, betas = ev_b[[2]], loss = opt$objective)
    }, error = function(e) NULL)
    if (is.null(res)) { fail <- fail + 1; next }
    kga_boot[b] <- res$kga
    beta_boot[b, ] <- res$betas
    loss_boot[b] <- res$loss
  }

  ok <- !is.na(kga_boot)
  kga_boot <- kga_boot[ok]
  beta_boot <- beta_boot[ok, , drop = FALSE]
  loss_boot <- loss_boot[ok]
  colnames(beta_boot) <- beta_param_names(K)
  q <- function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE)
  summary_df <- data.frame(
    param = c("kga", colnames(beta_boot)),
    mean  = c(mean(kga_boot), colMeans(beta_boot)),
    sd    = c(sd(kga_boot),   apply(beta_boot, 2, sd)),
    q025  = c(q(kga_boot)[1], apply(beta_boot, 2, function(x) q(x)[1])),
    q500  = c(q(kga_boot)[2], apply(beta_boot, 2, function(x) q(x)[2])),
    q975  = c(q(kga_boot)[3], apply(beta_boot, 2, function(x) q(x)[3])),
    row.names = NULL
  )
  if (fail > 0) message(fail, " of ", n_boot, " bootstrap fits failed.")
  pars_loss <- data.frame(
    kga_boot = kga_boot, beta_boot, loss_boot
  )
  list(
    pars_loss, summary = summary_df,
    sigma = sigma, n_success = length(kga_boot)
  )
}

df <- read.csv("./Tests/IDA/forKonrad-conc-vs-signal.csv", header = TRUE, sep = ";")
names(df)[1] <- "guest"
env <- new.env(parent = emptyenv())
env$ga <- df[, 1]
env$signal <- df[, -1] |> as.data.frame()
env$h0 <- 1.00E-06
env$d0 <- 1.00E-06
env$kd <- 3.00E+06
env$error_calc_fct <- rel_err
parameter <- seq(1, 1E+8, length.out = 1000L)
errors <- vapply(parameter, function(kga) {
  lossFctIDA(kga, env, eval = FALSE)
}, numeric(1L))
plot(parameter, errors)
kga_hat <- parameter[which.min(errors)]

vb <- vapro_bootstrap(env, kga_hat, n_boot = 200, sigma = NULL, kga_lower = 1e4, kga_upper = 1e10, seed = 1234)
vb
d <- vb[[1L]]
plot(d$kga_boot, d$loss_boot)
