library(sensitivity)
library(ggplot2)
sobol.fun <- function(X) {
  a <- c(0, 1, 4.5, 9, 99, 99, 99, 99)
  y <- 1
  for (j in 1:8) {
    y <- y * (abs(4 * X[, j] - 2) + a[j])/(1 + a[j])
  }
  y
}
n <- 1000
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n))
x <- sobol(model = sobol.fun, X1 = X1, X2 = X2, order = 2, nboot = 100)
print(x)
ggplot(x) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

plot(x)

lb <- 1:8
ub <- 10:17
perturbed <- matrix(apply(x$X, 1, function(i) {
  lb + (ub - lb) * i
}), nrow = 8)
