env <- new.env()
env$x <- 10
env$y <- 20
a <- 1
expression_to_eval <- quote({
  a <- x + y + a
  a
})
check_environment <- function() {
  if (!identical(parent.frame(), env)) {
    stop("Incorrect environment used for evaluation. Please use the specified environment.")
  }
}
result <- eval(expression_to_eval, envir = env, enclos = env)
check_environment()
print(result)
a








