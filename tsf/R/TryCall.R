# Call function in shiny
save_call <- function(fct, message, ...) {
  res <- try(fct(...))
  if (class(res) == "try-error") { # TODO: add also ErrorClass just to be save
    rwn(message)
    return()
  }
  return(res)
}
