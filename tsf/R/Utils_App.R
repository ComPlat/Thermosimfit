#' Conversion of expression to numbers
#' @description converts an expression to a number
#' @export
#' @param l is an expression which should be evaluated. If the expression can
#'        be evaluated to a number the number is returned. Otherwise an error is returned.
convertToNum <- function(l) {
  res <- sapply(l, function(x) {
    ast <- try(getAST(str2lang(x)))
    if (is(e, "ErrorClass")) {
      showNotification(e$message)
      return("Error")
    } else if(inherits(e, "try-error")) {
      showNotification(e)
      return("Error")
    } else {
      return(eval(parse(text = x)))
    }
  })
  return(res)
}


