parse_eq <- function(code, env) {
  if(!is.call(code)) {
    return(code)
  }
  lapply(code, \(x) {
    parse_eq(x, env)
  })
}

eliminate_variable <- function(eq, variable) {
  env <- new.env()
  env$coefficient_list <- c()
  parse_eq(eq, env) |> str()
  str(env$coefficient_list)
}

eq <- quote(2*x + 3*y + 1*x - 6)
eq <- eliminate_variable(eq, "x")

res <-
  list(
    fct = "-",
    lhs = list(
      fct = "+",
      lhs = list(
        fct = "+",
        lhs = list(
          fct = "*",
          lhs = 2,
          rhs = "x"
        ),
        rhs = list(
          fct = "*",
          lhs = 3,
          rhs = "y"
        )
      ),
      rhs = list(
        fct = "*",
        lhs = 1,
        rhs = "x")
    ),
    rhs = 6
  )

str(res)
