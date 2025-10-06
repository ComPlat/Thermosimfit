R2 <- function(y_true, y_pred) {
  model <- lm(y_true ~ y_pred)
  summary(model)$r.squared
}

R2Adjusted <- function(y_true, y_pred) {
  model <- lm(y_true ~ y_pred)
  summary(model)$adj.r.squared
}

metrices <- function(df, error_fct, nsigs) {
  dfs <- lapply(1:nsigs, function(i) {
    temp <- data.frame(
      df[[i + 1]],
      df[[i + nsigs + 1]]
    )
    names(temp) <- c(
      "Signal measured",
      "Signal simulated"
    )
    temp
  })
  metrices_internally <- function(y_true, y_pred, error_fct, i) {
    mse <- mean((y_true - y_pred)^2) # Mean Squared Error (MSE)
    rmse <- sqrt(mse) # Root Mean Squared Error (RMSE)
    r2 <- R2(y_true, y_pred) # R-squared (R2)
    r2adjusted <- R2Adjusted(y_true, y_pred)
    res <- data.frame(
      MeanSquareError = mse, RootMeanSquareError = rmse,
      R2 = r2, "R2 adjusted" = r2adjusted,
      "Error fct" = error_fct,
      Signal = i
    )
    return(res)
  }
  ms <- lapply(dfs, function(elem) {
    metrices_internally(elem[["Signal measured"]], elem[["Signal simulated"]], error_fct, parent.frame()$i[])
  })
  Reduce(rbind, ms)
}
