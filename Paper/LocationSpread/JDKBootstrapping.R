# Joint kernel density using SMR (significant model regions)
# SDR mathematically its a highest density region
# ========================================
calc_joint_density <- function(df) {
  mins <- apply(df, 2, min)
  maxs <- apply(df, 2, max)
  res <- ks::kde(df, xmin = mins, xmax = maxs)
  grid_points <- expand.grid(res$eval.points)
  joint_densities <- as.vector(res$estimate)
  density_data <- cbind(grid_points, joint_density = joint_densities)
  return(density_data)
}
# SMR highlights the most probable regions,
jdk_smr <- function(df, prob = 0.95) {
  df <- df[, 1:4]
  res <- ks::kde(df)
  level <- paste0(prob * 100, "%")
  density_threshold <- res$cont[level]
  grid_points <- expand.grid(res$eval.points)
  densities <- as.vector(res$estimate)
  significant_points <- grid_points[densities >= density_threshold, ]
  mode_index <- which.max(densities)
  mode <- grid_points[mode_index, ]
  mode <- ifelse(mode < 0, 0, mode) |> as.numeric()
  CIs <- apply(significant_points, 2, range)
  lc <- CIs[1, ]
  lc <- ifelse(lc < 0, 0, lc)
  uc <- CIs[2, ]
  uc <- ifelse(uc < 0, 0, uc)
  res <- calc_joint_density(df)
  df <- lapply(1:4, function(x) {
    i <- parent.frame()$i[]
    data.frame(x = res[, i], y = res[, 5])
  })
  return(list(
    mode = mode,
    lower_ci = lc,
    upper_ci = uc,
    df = df
  ))
}

# Joint kernel density with CIs calculated by bootstrapping
# ========================================
jkd_bootstrapping <- function(df, idx, n_iter = 1000) {
  # kde on data
  fit  <- ks::kde(df)
  dims <- vapply(fit$eval.points, length, integer(1))
  arr  <- array(fit$estimate, dim = dims)
  # marginal over all axes except idx (sum integrates the others on the evaluation grid)
  marg <- apply(arr, idx, sum)
  xvec <- fit$eval.points[[idx]]
  mode_idx <- which.max(marg)
  mode_val <- xvec[mode_idx]
  # bootstrap modes
  modes <- numeric(n_iter)
  for (i in seq_len(n_iter)) {
    print(paste0("Iter: ", i))
    boot <- df[sample.int(nrow(df), replace = TRUE), , drop = FALSE]
    fitb  <- ks::kde(boot)
    dimsB <- vapply(fitb$eval.points, length, integer(1))
    arrB  <- array(fitb$estimate, dim = dimsB)
    margB <- apply(arrB, idx, sum)
    modes[i] <- fitb$eval.points[[idx]][ which.max(margB) ]
  }
  ci <- stats::quantile(modes, c(0.025, 0.975), names = FALSE)
  df_out <- data.frame(x = xvec, y = marg / max(marg))
  list(
    res      = df_out,
    mode     = mode_val,
    lower_ci = ci[1],
    upper_ci = ci[2]
  )
}

jdk <- function(df, n_boot) {
  df <- df[, 1:4, drop = FALSE]
  out <- lapply(seq_len(4), function(i) jkd_bootstrapping(df, i, n_boot))
  list(
    mode     = vapply(out, function(x) x$mode,     numeric(1)),
    lower_ci = vapply(out, function(x) x$lower_ci, numeric(1)),
    upper_ci = vapply(out, function(x) x$upper_ci, numeric(1)),
    df       = lapply(out, function(x) x$res)
  )
}
