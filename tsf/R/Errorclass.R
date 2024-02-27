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


