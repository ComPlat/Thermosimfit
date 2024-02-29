library(tinytest)
library(tsf)
getError <- function(error) {
  error$message
}

f <- function(a, b, c) {
  for (i in 1:10) {
    print(i)
  }
}
b <- body(f)[[2]]
expect_equal( getError(tsf:::getAST(b)), paste0("Error: function ", "for" ," not allowed") )

path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
expect_equal( is.data.frame(tsf:::importData(path)), TRUE)
path <- paste0(system.file("examples", package = "tsf"), "/ImportFailsHere.txt")
expect_equal(getError(tsf:::importData(path)), "Could not identify seperator in file")



rosenbrock <- function(parameter, env, Ignore) {
  value <- 0
  for (i in 1:(length(parameter) - 1)) {
    value <- value + 
      100*(parameter[i + 1] - parameter[i]^2)^2 +
      (1 - parameter[i])^2
  }
  return(value)
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-10, 3), rep(10, 3), rosenbrock, 1000, 40,
                 0.00001, TRUE, FALSE)
expect_equal( sum(res[[2]] - rep(1, 3)) < 1e-9, TRUE) 

rastrigin <- function(x, env, Ignore) {
  A <- 10
  n <- length(x)
  sum_val <- sum(x^2 - A * cos(2 * pi * x))
  return(A * n + sum_val)
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-10, 3), rep(10, 3), rastrigin, 1000, 120,
                 10^-14, TRUE, FALSE)
expect_equal( sum(res[[2]]) < 1e-9, TRUE) 


sphere <- function(x, env, Ignore) { # x = (0, 0, ... 0)
  return(sum(x^2))
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-10, 3), rep(10, 3), sphere, 1000, 120,
                 10^-14, TRUE, FALSE)
expect_equal( sum(res[[2]]) < 1e-9, TRUE) 

ackley <- function(x, env, Ignore) { # x = (0, 0, ... 0)
  n <- length(x)
  sum1 <- sum(x^2)
  sum2 <- sum(cos(2 * pi * x))
  return(-20 * exp(-0.2 * sqrt(sum1/n)) - exp(sum2/n) + 20 + exp(1))
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-10, 3), rep(10, 3), ackley, 6500, 120,
                 10^-14, TRUE, FALSE) # here the random topology is worse than star but far better than for sphere, rastrigin or rosenbrock
expect_equal( sum(res[[2]]) < 1e-9, TRUE) 

michalewicz <- function(xx, env, Ignore) { 
  # 2D global min = -1.8013 at x(2.2, 1.57)
  m = 10
  ii <- c(1:length(xx))
  sum <- sum(sin(xx) * (sin(ii*xx^2/pi))^(2*m))
  y <- -sum
  return(y)
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(0, 2), rep(5, 2), michalewicz, 1200, 120,
                 -Inf, TRUE, FALSE) 
expect_equal( sum(res[[2]] - c(2.2, 1.57)) < 1e-2 , TRUE) 
set.seed(1234)
res <- tsf:::pso(new.env(), rep(0, 2), rep(5, 2), michalewicz, 1200, 120,
                 -Inf, FALSE, FALSE) 
expect_equal( sum(res[[2]] - c(2.2, 1.57)) < 1e-2 , TRUE) 

schwefel_222 <- function(x, env, Ignore) { # x = (0, 0, ... 0)
  return(max(abs(x)))
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-10, 2), rep(10, 2), schwefel_222, 2200, 80,
                 1e-14, TRUE, FALSE) 
expect_equal( sum(abs(res[[2]])) < 1e-12 , TRUE) 

griewank <- function(x, env, Ignore) { # x = (0, 0, ... 0)
  n <- length(x)
  sum1 <- sum(x^2)/4000
  prod1 <- prod(cos(x/sqrt(1:n)))
  return(sum1 - prod1 + 1)
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-10, 2), rep(10, 2), griewank, 2800, 120,
                 1e-32, TRUE, FALSE) 
expect_equal( sum(abs(res[[1]])) < 1e-12 , TRUE) 
expect_equal( sum(abs(res[[2]])) < 1e-6 , TRUE) 

easom <- function(x, env, Ignore) { # global min -1 with x at pi, pi
  return(-cos(x[1]) * cos(x[2]) * exp(-((x[1] - pi)^2 + (x[2] - pi)^2)))
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-10, 2), rep(10, 2), easom, 2800, 120,
                 -1, TRUE, FALSE) 
expect_equal( sum(res[[1]] + 1) < 1e-12 , TRUE) 
expect_equal( sum(abs(res[[2]]) - c(pi, pi)) < 1e-6 , TRUE) 

egg_holder <- function(xx, env, Ignore) {
  # Global Minimum: f(x)≈−959.6407f(x)≈−959.6407 at x=(512,404.2319)x=(512,404.2319)
  x1 <- xx[1]
  x2 <- xx[2]
  term1 <- -(x2 + 47) * sin(sqrt(abs(x2 + x1 / 2 + 47)))
  term2 <- -x1 * sin(sqrt(abs(x1 - (x2 + 47))))
  y <- term1 + term2
  return(y)
}
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-512, 2), rep(512, 2), egg_holder, 2800, 120,
                 -959, FALSE, FALSE) 
expect_equal( sum(res[[1]] + 1) < 1e-12 , TRUE) 
expect_equal( sum(abs(res[[2]]) - c(512.404, 404.2319)) < 1e-6 , TRUE) 
set.seed(1234)
res <- tsf:::pso(new.env(), rep(-512, 2), rep(512, 2), egg_holder, 2800, 120,
                 -959, TRUE, FALSE) 
expect_equal( sum(res[[1]] + 1) < 1e-12 , TRUE) 
expect_equal( sum(abs(res[[2]]) - c(512.404, 404.2319)) < 1e-6 , TRUE) 


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

test_createPolynom_valid_input <- function() {
  f <- function() {
    h + hd + -h0 = 0
    d + hd -d0 = 0   
    hd / (h*d) -kd = 0
  }
  elimVars <- c("h", "d")
  result <- createPolynom(f, elimVars)
  trueRes <- "(hd^2 + (-h0 - d0) * hd + d0 * h0) * kd - hd"
  trueRes <- gsub(" ", "", trueRes)
  result <- gsub(" ", "", deparse(result))
  expect_equal(trueRes, result)
}
test_createPolynom_valid_input()

test_createPolynom_invalid_function <- function() {
  f <- "not_a_function"
  elimVars <- c("h", "d")
  result <- createPolynom(f, elimVars)
  expect_true(inherits(result, "ErrorClass"))
}
test_createPolynom_invalid_function()

test_createPolynom_invalid_elimVars <- function() {
  f <- function() {
    h + hd + -h0 = 0
    d + hd - d0 = 0   
    hd / (h*d) - kd = 0
  }
  elimVars <- "not_a_character_vector"
  result <- createPolynom(f, elimVars)
  expect_true(inherits(result, "ErrorClass"))
}
test_createPolynom_invalid_elimVars()

test_createPolynom_another_valid_input <- function() {
  f <- function() { # x = 0 and y = 2
    3*y + 2*x - 6 = 0
    5 * y - 2*x - 10 = 0
  }
  elimVars <- c("y", "x")
  resultX <- createPolynom(f, elimVars)
  elimVars <- c("x", "y")
  resultY <- createPolynom(f, elimVars)
  expect_equal(c(resultX, resultY), c(0, 2))
}
test_createPolynom_another_valid_input()

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
  res <- tsf::opti("hg", c(1, 0, 0, 0), c(10^9, 1, rep(10^5, 2)), file, env$h0,
                   40, 100)
  expect_true(res[[4]]$r2 > 0.99)
  expect_true( (res[[2]]$IHD - 1000) < 1)
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
  res <- tsf::opti("ida", c(1, 0, 0, 0), c(10^9, 1, rep(10^5, 2)), file, c(env$h0, env$d0, env$kd),
                   40, 150)
  expect_true(res[[4]]$r2 > 0.99)
  expect_true( (res[[2]]$IHD - 1000) < 10)
}
test_ida()

test_gda <- function() {
  env <- new.env()
  env$h0 <- 5
  env$kd <- 700000
  env$ga0 <- 6
  path <- paste0(system.file("examples", package = "tsf"), "/GDA.txt")
  res <- tsf::opti("gda", c(1, 0, 0, 0), c(10^9, 1, rep(10^5, 2)), path, 
                   additionalParameters = c(env$h0, env$ga0, env$kd),
                   40, 150)
  
  df <- read.csv(path, header = FALSE, sep = "\t")
  df[,1] <- 1000 * df[, 1]
  write.csv(df, path, quote = FALSE, row.names = FALSE)
  parameter <- c(10^8, 0, 1000, 1)
  env <- new.env()
  env$h0 <- 5
  env$kd <- 700000
  env$ga0 <- 6
  env$dye <- df[, 1]
  env$signal <- df[, 2]
  result <- tsf:::lossFctGDA(parameter, env, TRUE)
  df[, 2] <- result$insilico
  file <- tempfile(fileext = ".txt")
  write.csv(df, file, quote = FALSE, row.names = FALSE)
  set.seed(1234)
  res <- tsf::opti("gda", c(1, 0, 0, 0), c(10^9, 1, rep(10^5, 2)), file, 
                   additionalParameters = c(env$h0, env$ga0, env$kd),
                   40, 150)
  expect_true(res[[4]]$r2 > 0.99)
  expect_true( (res[[2]]$IHD - 1000) < 10)
}
test_gda()
