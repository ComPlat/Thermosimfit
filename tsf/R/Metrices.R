R2 <- function(y_true, y_pred) {
  ybar <- mean(y_true)
  residual <- sum((y_true - y_pred)^2) # sum of squared differences
  total <- sum((y_true - ybar)^2) # total sum of squares
  r2 <- 1 - (residual / total)
  return(r2)
}

R2Adjusted <- function(y_true, y_pred, k) {
  n <- length(y_true)
  r2 <- R2(y_true, y_pred)
  adjusted_r2 <- 1 - ((1 - r2) * (n - 1) / (n - k - 1))
  return(adjusted_r2)
}

metrices <- function(y_true, y_pred, number_of_independent_vars) {
  mse <- mean((y_true - y_pred)^2) # Mean Squared Error (MSE)
  rmse <- sqrt(mse) # Root Mean Squared Error (RMSE)
  mae <- mean(abs(y_true - y_pred)) # Mean Absolute Error (MAE)
  r2 <- R2(y_true, y_pred) # R-squared (R2)
  r2adjusted <- R2Adjusted(y_true, y_pred, number_of_independent_vars)
  return(list(
    mse = mse, rmse = rmse,
    mae = mae, r2 = r2, r2adjusted = r2adjusted
  ))
}