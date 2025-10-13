# Calculate results
# ========================================
calc_values <- function(df) {
  res <- jdk_smr(df)
  res$mode
  res$lower_ci
  res$upper_ci
  res <- lapply(1:4, function(idx) {
    max <- max(df[, idx])
    min <- min(df[, idx])
    mode <- res$mode[[idx]]
    l <- res$lower_ci[[idx]]
    u <- res$upper_ci[[idx]]
    df_temp <- data.frame(
      values = sprintf("%.3e", c(mode, l, u)),
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
dba_res <- calc_values(p_dba)
print("DBA")
print(dba_res)
ida_res <- calc_values(p_ida)
print("IDA")
print(ida_res)
gda_res <- calc_values(p_gda)
print("GDA")
print(gda_res)

