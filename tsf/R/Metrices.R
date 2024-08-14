R2 <- function(y_true, y_pred) {
  model <- lm(y_true ~ y_pred)
  summary(model)$r.squared
}

R2Adjusted <- function(y_true, y_pred) {
  model <- lm(y_true ~ y_pred)
  summary(model)$adj.r.squared
}

metrices <- function(y_true, y_pred) {
  mse <- mean((y_true - y_pred)^2) # Mean Squared Error (MSE)
  rmse <- sqrt(mse) # Root Mean Squared Error (RMSE)
  mae <- mean(abs(y_true - y_pred)) # Mean Absolute Error (MAE)
  r2 <- R2(y_true, y_pred) # R-squared (R2)
  r2adjusted <- R2Adjusted(y_true, y_pred)
  res <- data.frame(
    mse = mse, rmse = rmse,
    mae = mae, r2 = r2, r2adjusted = r2adjusted
  )
  names(res) <- c(
    "MeanSquareError",
    "RootMeanSquareError", "MeanAbsoluteError", "R2", "R2 adjusted"
  )
  return(res)
}
