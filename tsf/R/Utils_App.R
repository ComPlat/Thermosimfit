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


