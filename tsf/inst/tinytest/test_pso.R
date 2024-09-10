library(tinytest)
library(tsf)

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
