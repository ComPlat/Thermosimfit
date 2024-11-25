# test opti
library(tsf)
library(tinytest)

# HG is DBA with increasing dye
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
    npop = 40, ngen = 100
  )
  expect_true(res[[4]]$R2 >= 0.99)
}
test_hg()

# DBA is case DBA with const dye and increasing host
test_dba <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
  df <- read.csv(path, header = FALSE, sep = "\t")
  parameter <- c(10^8, 0, 1000, 1)
  env <- new.env()
  env$d0 <- 5
  env$host <- df[, 1]
  env$signal <- df[, 2]
  result <- tsf:::lossFctDBA(parameter, env, TRUE)
  df[, 2] <- result$insilico
  file <- tempfile(fileext = ".txt")
  write.csv(df, file, quote = FALSE, row.names = FALSE)
  set.seed(1234)
  res <- tsf::opti(
    "dba_dye_const", c(1, 0, 0, 0), c(10^9, 1, rep(10^5, 2)), file, env$d0,
    npop = 40, ngen = 100
  )
  expect_true(res[[4]]$R2 >= 0.99)
}
test_dba()

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
    npop = 40, ngen = 150
  )
  expect_true(res[[4]]$R2 >= 0.99)
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
    npop = 40, ngen = 175
  )
  expect_true(res[[4]]$R2 >= 0.99)
}
test_gda()
