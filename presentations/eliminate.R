library(tsf)

f <- function() { # x = 0 and y = 2
  3*y + 2*x - 6 = 0
  5 * y - 2*x - 10 = 0
}
elimVars <- c("y")
resultX <- createPolynom(f, elimVars)
resultX
elimVars <- c("x")
resultY <- createPolynom(f, elimVars)
resultY