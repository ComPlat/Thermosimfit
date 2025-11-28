# TODO: implement model as PSO
# TODO: add this model only in backend
# TODO: add the HG2 model also with: signalInsilico <- I0 + IH*h + IG*g + IHG*hg + IHGG*hgg
# TODO: analyse the data from Frank
library(parallel)
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

simulate <- function(Kg1, Kg2) {
  I0   <- 0
  IH   <- 0
  IG   <- 1e6
  IHG  <- 1e7
  IHG2 <- 1e5
  H0 <- 3e-4
  G0_values <- seq(0, 1e-3, length.out = 20)

  balance_fun <- function(G0) {
    function(g) {
      denom <- 1 + Kg1*g + (Kg1*Kg2)*(g^2)
      Hf    <- H0 / denom
      HG    <- Kg1 * Hf * g
      HG2   <- Kg2 * HG * g
      g + HG + 2*HG2 - G0
    }
  }
  find_g_free <- function(G0, tol = 1e-14, max_expand = 50) {
    f    <- balance_fun(G0)
    lo   <- 0
    hi   <- max(G0, 1e-12)

    flo <- f(lo)
    fhi <- f(hi)

    k <- 0
    while (!is.na(flo) && !is.na(fhi) && flo * fhi > 0 && k < max_expand) {
      hi  <- hi * 2 + 1e-12
      fhi <- f(hi)
      k   <- k + 1
    }

    if (is.na(flo) || is.na(fhi) || flo * fhi > 0) {
      return(NA_real_)
    }

    tryCatch(uniroot(f, lower = lo, upper = hi, tol = tol)$root,
      error = function(e) NA_real_)
  }
  signals <- numeric(length(G0_values))

  for (i in seq_along(G0_values)) {
    G0 <- G0_values[i]

    g_free <- find_g_free(G0)

    if (is.na(g_free)) {
      signals[i] <- NA_real_
    } else {
      denom <- 1 + Kg1*g_free + (Kg1*Kg2)*(g_free^2)
      Hf    <- H0 / denom
      HG    <- Kg1 * Hf * g_free
      HG2   <- Kg2 * HG * g_free
      S <- I0 + IH*Hf + IG*g_free + IHG*HG + IHG2*HG2
      signals[i] <- S
    }
  }
  data.frame(G0 = G0_values, Signal = signals)
}

lossFctHG2 <- function(parameter, env, eval = FALSE) {
  kg  <- parameter[1]
  kgg <- parameter[2]
  h0  <- env$h0
  guest  <- as.numeric(env$guest)
  signal <- as.numeric(env$signal)

  find_g_free <- function(H0, Kg1, Kg2, G0, tol = 1e-14, max_expand = 50) {
    if (G0 == 0) return(0)
    f <- function(g) {
      denom <- 1 + Kg1*g + (Kg1*Kg2)*(g^2)
      Hf <- H0 / denom
      HG <- Kg1 * Hf * g
      HG2 <- Kg2 * HG * g
      g + HG + 2*HG2 - G0
    }
    lo <- 0
    hi <- max(G0, 1e-12)
    flo <- f(lo)
    fhi <- f(hi)
    k <- 0

    repeat {
      if (is.na(flo) || is.na(fhi)) break
      if ((flo * fhi) <= 0) break # Sign change detected
      if (k < max_expand) break
      hi <- hi * 2 + 1e-12
      fhi <- f(hi)
      k <- k + 1
    }
    if (is.na(flo) || is.na(fhi) || flo * fhi > 0) return(NA_real_)
    tryCatch(
      uniroot(f, lower = lo, upper = hi, tol = tol)$root,
      error = function(e) NA_real_
    )
  }

  n <- length(guest)
  h   <- numeric(n)
  g   <- numeric(n)
  hg  <- numeric(n)
  hgg <- numeric(n)
  bad <- logical(n)

  # nonlinear
  for (i in seq_len(n)) {
    g0 <- guest[i]
    g_free <- find_g_free(h0, kg, kgg, g0)
    if (is.na(g_free)) {
      bad[i] <- TRUE
      next
    }
    denom <- 1 + kg*g_free + (kg*kgg)*(g_free^2)
    h[i] <- h0 / denom
    g[i] <- g_free
    hg[i] <- kg * h[i] * g_free
    hgg[i] <- kgg * hg[i] * g_free
  }

  if (any(bad)) {
    keep <- !bad & is.finite(signal)
  } else {
    keep <- is.finite(signal)
  }
  X <- cbind(Intercept = 1, h = h, g = g, hg = hg, hgg = hgg)[keep, , drop = FALSE]
  y <- signal[keep]

  eps <- 1e-12
  w   <- 1 / pmax(abs(y), eps)
  # X = nxp matrix, 1 --> across rows, w = n vector (weights), multiply
  # --> for(i in seq_len(nrow(X))) X[i, ] * w[i]
  Xw  <- sweep(X, 1, w, "*")
  yw  <- y * w

  fit <- nnls::nnls(Xw, yw) # solves X*yw = b --> results are b
  beta <- as.numeric(coef(fit))
  yhat <- as.vector(X %*% beta)

  denom <- pmax(abs(y), eps)
  loss  <- sum(abs(y - yhat) / denom)
  loss <- rel_err(yhat, y)

  if (eval) {
    return(list(
      loss  = loss,
      fitted = yhat,
      beta   = beta,
      X      = X,
      keep   = keep,
      species = list(h = h[keep], g = g[keep], hg = hg[keep], hgg = hgg[keep])
    ))
  } else {
    return(loss)
  }
}
# --- example driver -----------------------------------------------------
Kg1  <- 1e6
Kg2  <- 2e5

Kg1  <- 1e9
Kg2  <- 2.5e3

Kg1 <- 1e3
Kg2 <- 1e9
df <- simulate(Kg1, Kg2)
plot(Signal ~ G0, data = df)

env <- new.env(parent = emptyenv())
env$guest <- df[[1]]
env$signal <- df[[2]]
env$h0 <- 3e-4

run_grid <- function(loss_fun, env,
                     K1_range = c(1e2, 1e10),
                     K2_range = c(1e2, 1e10),
                     n = 250,
                     mc.cores = max(1L, detectCores() - 1L)) {
  logspace <- function(a, b, n) 10^seq(log10(a), log10(b), length.out = n)
  k1 <- logspace(K1_range[1], K1_range[2], n)
  k2 <- logspace(K2_range[1], K2_range[2], n)
  grid <- expand.grid(Kg1 = k1, Kg2 = k2, KEEP.OUT.ATTRS = FALSE)
  errs <- mclapply(seq_len(nrow(grid)), function(i) {
    loss_fun(c(grid$Kg1[i], grid$Kg2[i]), env)
  }, mc.cores = mc.cores)
  grid$error <- unlist(errs)
  grid
}
res <- run_grid(lossFctHG2, env)
plot(error ~ Kg1, data = res)
plot(error ~ Kg2, data = res)

best_params <- as.numeric(res[which.min(res$error), 1:2])
ps(best_params)
is <- lossFctHG2(best_params, env, TRUE)
plot(Signal ~ G0, data = df)
points(df$G0, is$fitted, pch = 19)
sum(abs(df$Signal - is$fitted)) # < 1e-6

ggplot(res, aes(Kg1, Kg2, z = error)) +
  geom_contour_filled(bins = 30) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Kg1", y = "Kg2", fill = "error")

determine_parameter_range <- function(loss_fun, env, lb, ub, n = 31, delta = NULL,
                                      mc.cores = max(1L, parallel::detectCores() - 1L)) {
  stopifnot(is.numeric(lb), is.numeric(ub), length(lb) == 2L, length(ub) == 2L)
  if (any(lb <= 0) || any(ub <= 0)) stop("lb/ub must be > 0 for log spacing.")
  if (any(ub <= lb)) stop("Each ub must be greater than lb.")

  k1 <- 10 ^ seq(log10(lb[1]), log10(ub[1]), length.out = n)
  k2 <- 10 ^ seq(log10(lb[2]), log10(ub[2]), length.out = n)

  grid <- expand.grid(Kg1 = k1, Kg2 = k2, KEEP.OUT.ATTRS = FALSE)

  errs <- parallel::mclapply(seq_len(nrow(grid)), function(i) {
    loss_fun(c(grid$Kg1[i], grid$Kg2[i]), env)
  }, mc.cores = mc.cores)

  grid$error <- unlist(errs)

  if (!is.null(delta)) {
    best <- min(grid$error, na.rm = TRUE)
    thr  <- best * (1 + delta)
    grid$acceptable <- is.finite(grid$error) & grid$error <= thr
    attr(grid, "best") <- best
    attr(grid, "threshold") <- thr
  }
  grid
}
bp_lb <- best_params*0.25
bp_ub <- best_params*1.75
res <- determine_parameter_range(lossFctHG2, env, bp_lb, bp_ub, n = 31, delta = 0.05)
sum_resids <- apply(res, 1, function(row) {
  ps <- as.numeric(row[1:2])
  is <- lossFctHG2(ps, env, TRUE)
  model <- lm(df$Signal ~ is$fitted)
  sum(residuals(model))
})
summary(sum_resids)
res
