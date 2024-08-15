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
    } else if (inherits(e, "try-error")) {
      showNotification(e)
      return("Error")
    } else {
      return(x)
    }
  })

  res <- sapply(l, function(x) {
    res <- try(eval(parse(text = x)))
    if (inherits(res, "try-error")) {
      showNotification(res)
      return("Error")
    }
    return(res)
  })
  return(res)
}

format_scientific <- function(x) {
  formatC(x, format = "e", digits = 3)
}

flush <- function(f) {
  file_con <- file(f, open = "w")
  close(file_con)
}

extract_iter <- function(s) {
  if (!is.character(s)) {
    return()
  }
  if (length(s) == 0) {
    return()
  }
  a <- strsplit(s, ";")[[1]]
  a <- strsplit(a, "/")[[1]]
  as.numeric(a[[1]])
}

# print notification
print_noti <- function(message, type = "warning", duration = 10) {
  showNotification(
    message,
    duration = duration,
    type = type
  )
}

# require with notificiation
rwn <- function(expr, message, type = "warning", duration = 10) {
  if (!expr) {
    print_noti(
      message,
      duration = duration,
      type = type
    )
  }
  req(expr)
}

# helper
convert_all_to_num <- function(what, ...) {
  v <- c(...)
  v <- convertToNum(v)
  if (any("Error" %in% v)) {
    rwn(FALSE, paste0("The ", what, " cannot be converted into a numeric value"))
  }
  return(v)
}
