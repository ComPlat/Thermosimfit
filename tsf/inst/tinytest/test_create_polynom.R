# library(tinytest)
# library(tsf)
#
# # test create polynom
# test_createPolynom_valid_input <- function() {
#   f <- function() {
#     h + hd + -h0 <- 0
#     d + hd - d0 <- 0
#     hd / (h * d) - kd <- 0
#   }
#   elimVars <- c("h", "d")
#   result <- createPolynom(f, elimVars)
#   trueRes <- "(hd^2 + (-h0 - d0) * hd + d0 * h0) * kd - hd"
#   trueRes <- gsub(" ", "", trueRes)
#   result <- gsub(" ", "", deparse(result))
#   expect_equal(trueRes, result)
# }
# test_createPolynom_valid_input()
#
# test_createPolynom_invalid_function <- function() {
#   f <- "not_a_function"
#   elimVars <- c("h", "d")
#   result <- try(createPolynom(f, elimVars))
#   expect_true(inherits(result, "try-error"))
# }
# test_createPolynom_invalid_function()
#
# test_createPolynom_invalid_elimVars <- function() {
#   f <- function() {
#     h + hd + -h0 <- 0
#     d + hd - d0 <- 0
#     hd / (h * d) - kd <- 0
#   }
#   elimVars <- "not_a_character_vector"
#   result <- try(createPolynom(f, elimVars))
#   expect_true(inherits(result, "try-error"))
# }
# test_createPolynom_invalid_elimVars()
#
# test_createPolynom_another_valid_input <- function() {
#   f <- function() { # x = 0 and y = 2
#     3 * y + 2 * x - 6 <- 0
#     5 * y - 2 * x - 10 <- 0
#   }
#   elimVars <- c("y", "x")
#   resultX <- createPolynom(f, elimVars)
#   elimVars <- c("x", "y")
#   resultY <- createPolynom(f, elimVars)
#   expect_equal(c(resultX, resultY), c(0, 2))
# }
# test_createPolynom_another_valid_input()
