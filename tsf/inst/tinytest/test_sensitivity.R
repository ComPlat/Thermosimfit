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
  result <- try(
    sensitivity("invalid_case", parameters, path, additionalParameters, percentage = 10)
  )
  expect_true("try-error" %in% class(result))
}
test_sensitivity_invalid_case()

# run sensitivity for gda
test_sensitivity_gda <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/GDA.txt")
  optimP <- data.frame(1.932e+06, 2.324e+02, 3.341e+09, 4.438e+04)
  result <- sensitivity("gda", optimP, path, c(1.65e-06, 1.32e-06, 1.7e07), 15)
  expect_true("gg" %in% class(result))
}
test_sensitivity_gda()

# run sensitivity for dba const host
test_sensitivity_dba_const_host <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/dba_dye_const.txt")
  optimP <- data.frame(2.002e03, 2.047e02, 1.828e07, 1.64e05)
  result <- tsf::sensitivity(
    case = "dba_host_const", parameter = optimP,
    path = path,
    additionalParameters = c(dye = 0.000151),
    percentage = 15
  )
  expect_true("gg" %in% class(result))
}
test_sensitivity_dba_const_host()

# run sensitivity for dba const dye
test_sensitivity_dba_const_dye <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/dba_dye_const.txt")
  optimP <- data.frame(2.002e03, 2.047e02, 1.828e07, 1.64e05)
  result <- tsf::sensitivity(
    case = "dba_dye_const", parameter = optimP,
    path = path,
    additionalParameters = c(dye = 0.000151),
    percentage = 15
  )
  expect_true("gg" %in% class(result))
}
test_sensitivity_dba_const_dye()
