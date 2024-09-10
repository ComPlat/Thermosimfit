library(tinytest)
library(tsf)
# test sensitivity
test_sensitivity_valid_input <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
  optimP <- data.frame(80699337.884, 0.000, 1251.928, 0.000)
  result <- sensitivity("ida", optimP, path, c(4.3, 6.0, 7079458), 20)
  expect_true("gg" %in% class(result))
}
test_sensitivity_valid_input()

test_sensitivity_invalid_case <- function() {
  parameters <- c(1, 2, 3, 4)
  additionalParameters <- c(5, 6, 7)
  path <- "invalid_path.txt"
  result <- sensitivity("invalid_case", parameters, path, additionalParameters, percentage = 10)
  expect_true("ErrorClass" %in% class(result))
}
test_sensitivity_invalid_case()

