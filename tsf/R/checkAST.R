getAST <- function(inp) {
  if (!is.call(inp)) {
    return(inp)
  }

  inp <- as.list(inp)

  # check if is function
  fct <- inp[[1]]

  allowed_fcts <- c(
    "-", "+", "*", "/",
    "log", "log10", "sqrt", "exp", "^",
    "sin", "cos", "tan", "tanh", "sinh", "cosh", "acos", "asin", "atan",
    "<-", "="
  )

  check <- deparse(fct)

  if ((check %in% allowed_fcts) == FALSE) {
    return(ErrorClass$new(paste0("Error: function ", check ,"not allowed")))
  }

  lapply(inp, getAST)
}
