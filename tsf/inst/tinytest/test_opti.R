# test opti
test_hg <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
  df <- read.csv(path, header = FALSE, sep = "\t")
  parameter <- c(10^8, 0, 1000, 1)
  env <- new.env()
  env$h0 <- 5
  env$dye <- df[, 1]
  env$signal <- df[, 2]
  result <- tsf:::lossFctHG(parameter, env, TRUE)
  df[, 2] <- result$insilico
  file <- tempfile(fileext = ".txt")
  write.csv(df, file, quote = FALSE, row.names = FALSE)
  set.seed(1234)
  res <- tsf::opti(
    "dba_host_const", c(1, 0, 0, 0), c(10^9, 1, rep(10^5, 2)), file, env$h0,
    40, 100
  )
  expect_true(res[[4]]$R2 > 0.99)
  expect_true((res[[2]][3] - 1000) < 1)
}
test_hg()

test_ida <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
  df <- read.csv(path, header = FALSE, sep = "\t")
  parameter <- c(10^8, 0, 1000, 1)
  env <- new.env()
  env$h0 <- 5
  env$kd <- 700000
  env$d0 <- 6
  env$ga <- df[, 1]
  env$signal <- df[, 2]
  result <- tsf:::lossFctIDA(parameter, env, TRUE)
  df[, 2] <- result$insilico
  file <- tempfile(fileext = ".txt")
  write.csv(df, file, quote = FALSE, row.names = FALSE)
  set.seed(1234)
  res <- tsf::opti(
    "ida", c(1, 0, 0, 0), c(10^9, 1, rep(10^5, 2)), file, c(env$h0, env$d0, env$kd),
    40, 150
  )
  expect_true(res[[4]]$R2 > 0.99)
  expect_true((res[[2]][3] - 1000) < 10)
}
test_ida()

test_gda <- function() {
  env <- new.env()
  env$h0 <- 1.65
  env$kd <- 1.7 * 10^7
  env$ga0 <- 1.8
  parameter <- c(1857463, 0, 3456.443, 0)
  path <- paste0(system.file("examples", package = "tsf"), "/GDA.txt")
  df <- read.csv(path, header = TRUE, sep = ",")
  env$dye <- df[, 1]
  env$signal <- df[, 2]
  result <- tsf:::lossFctGDA(parameter, env, TRUE)
  df[, 2] <- result$insilico
  file <- tempfile(fileext = ".txt")
  write.csv(df, file, quote = FALSE, row.names = FALSE)
  set.seed(1234)
  res <- tsf::opti("gda", c(1, 0, 0, 0), c(10^9, 1, rep(10^6, 2)), file,
    additionalParameters = c(env$h0, env$ga0, env$kd),
    40, 175
  )
  expect_true(res[[4]]$R2 > 0.99)
  expect_true(abs(res[[2]][3] - 3456) < 100)
}
test_gda()

# test opti for DBA const dye (not hg)

# create for each model a test. First create random parameter.
# Use these parameters in the loss function to create trajectories.
# Use these trajectories to create a file. Use this file to run opti.
# Check if the R2 value is greater than 0.99 and the parameter is close to the
# original parameter.
