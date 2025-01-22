df <- read.csv("Tests/KernelDensi/joint_kernel_density.csv")
head(df)
res <- tsf:::kde4d_with_smr(df[, 1:4], 0.95)
str(res)
calc_values <- function(df) {
  res <- kde4d_with_smr(df)
  res$mode
  res$lower_ci
  res$upper_ci
  res <- lapply(1:4, function(idx) {
    mode <- res$mode[idx]
    l <- res$lower_ci[idx]
    u <- res$upper_ci[idx]
    df_temp <- data.frame(
      values = c(mode, l, u),
      type = c("mode", "lower", "upper")
    )
    names(df_temp)[1] <- names(df)[idx]
    return(df_temp)
  })
  res <- lapply(res, function(x) {
    x[, 1]
  })
  res <- Reduce(rbind, res) |> as.data.frame()
  res <- cbind(names(df)[1:4], res)
  names(res) <- c("Parameter", "mode", "lower", "upper")
  row.names(res) <- NULL
  return(res)
}
calc_values(df)
