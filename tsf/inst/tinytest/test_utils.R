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
expect_equal(getError(tsf:::getAST(b)), paste0("Error: function ", "for", " not allowed"))

path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
expect_equal(is.data.frame(tsf:::importData(path)), TRUE)
path <- paste0(system.file("examples", package = "tsf"), "/ImportFailsHere.txt")
expect_equal(getError(tsf:::importData(path)), "Could not identify seperator in file")
