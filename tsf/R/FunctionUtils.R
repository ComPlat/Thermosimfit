# TODO: do not export it anymores

#' ErrorClass class for handling errors
#' @description a class for handling error messages
#'
#' @export
#' @import R6
ErrorClass <- R6::R6Class(
  "ErrorClass",
  public = list(
    #' @field message the error message
    message = NULL,
    #' @field object an R object which can be stored in the class instance if an error and a result should be returned together.
    object = NULL,

    #' @description
    #' create a new ErrorClass Object
    #' @param message the string describing the error
    #' @param object is optional if something besides the message should be stored
    initialize = function(message, object = NULL) {
      if (is.null(message)) {
        stop("No message object is passed. Undefined case")
      } else {
        self$message <- message
        if (!is.null(object)) {
          self$object <- object
        }
      }
    }
  )
)

addArguments <- function(existingFunction, newArgumentNames) {
  existingArgs <- as.list(formals(existingFunction))
  newArgs <- setNames(rep(list(NULL), length(newArgumentNames)), newArgumentNames)
  updatedArgs <- c(existingArgs, newArgs)
  formals(existingFunction) <- updatedArgs
  existingFunction
}

addCode <- function(existingFunction, codeVector) {
  existingBody <- as.list(body(existingFunction))
  newBody <- as.list(parse(text = codeVector))
  updatedBody <- c(existingBody, newBody)
  body(existingFunction) <- as.call(updatedBody)
  existingFunction
}
