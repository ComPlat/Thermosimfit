ErrorClass <- R6::R6Class(
  "ErrorClass",
  public = list(
    message = NULL, 
    object = NULL,

    initialize = function(message, object = NULL) {
      if(is.null(message)) {
        stop("No message object is passed. Undefined case")
      } else {
        self$message <- message
        if(!is.null(object)) {
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