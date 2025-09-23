R2 <- function(y_true, y_pred) {
  model <- lm(y_true ~ y_pred)
  summary(model)$r.squared
}

R2Adjusted <- function(y_true, y_pred) {
  model <- lm(y_true ~ y_pred)
  summary(model)$adj.r.squared
}

metrices <- function(y_true, y_pred, error_fct) {
  r2 <- R2(y_true, y_pred) # R-squared (R2)
  r2adjusted <- R2Adjusted(y_true, y_pred)
  res <- data.frame(
    R2 = r2,  "R2 adjusted" = r2adjusted,
    "Error fct" = error_fct
  )
  return(res)
}
