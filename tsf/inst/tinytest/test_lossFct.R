library(tinytest)
library(tsf)
# test loss functions
test_lossFctHG_valid_input <- function() {
  parameter <- c(0.5, 1, 2, 3)
  env <- new.env()
  env$h0 <- 5
  env$dye <- c(0, 0.2, 0.3)
  env$signal <- c(1, 2, 3)
  result <- tsf:::lossFctHG(parameter, env)
  expect_true(is.numeric(result))
}
test_lossFctHG_valid_input()

test_lossFctIDA_valid_input <- function() {
  parameter <- c(0.5, 1, 2, 3)
  env <- new.env()
  env$h0 <- 5
  env$d0 <- 2
  env$ga <- c(0.1, 0.2, 0.3)
  env$signal <- c(1, 2, 3)
  env$kd <- 0.01
  result <- tsf:::lossFctIDA(parameter, env)
  expect_true(is.numeric(result))
}
test_lossFctIDA_valid_input()

test_lossFctGDA_valid_input <- function() {
  parameter <- c(0.5, 1, 2, 3)
  env <- new.env()
  env$h0 <- 5
  env$ga0 <- 2
  env$dye <- c(0.1, 0.2, 0.3)
  env$signal <- c(1, 2, 3)
  env$kd <- 0.01
  result <- tsf:::lossFctGDA(parameter, env)
  expect_true(is.numeric(result))
}
test_lossFctGDA_valid_input()

