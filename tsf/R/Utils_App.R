#' Conversion of expression to numbers
#' @description converts an expression to a number
#' @export
#' @param l is an expression which should be evaluated. If the expression can
#'        be evaluated to a number the number is returned. Otherwise an error is returned.
convertToNum <- function(l) {
  res <- sapply(l, function(x) {
    e <- try(tsf:::getAST(str2lang(x)))
    if (is(e, "ErrorClass")) {
      showNotification(e$message)
      return("Error")
    } else if(inherits(e, "try-error")) {
      showNotification(e)
      return("Error")
    } else {
      return(x)  
    }
  })

  res <- sapply(l, function(x) {
    res <- try(eval(parse(text = x)))
    if(inherits(res, "try-error")) {
      showNotification(res)
      return("Error")
    } 
    return(res) 
  })
  return(res)
}


