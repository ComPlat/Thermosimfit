#' Communicator class 
#' @description a class for communicating via a temporary file
#'
#' @export
#' @import R6
Communicator <- R6::R6Class("Communicator",
  public = list(
    #' @field file is a file which contains the current status
    file = NULL,

    #' @field result is a file in which data can be written or read information.
    result = NULL,
    
    #' @description 
    #' create a new Communicator Object
    initialize = function() {
      self$file = tempfile()
      self$result = tempfile()
      write("Ready", self$file)
      write("", self$result)
    },

    #' @description 
    #' get the current status
    getStatus = function() {
      scan(self$file, what = "character", sep = "\n")  
    },

    #' @description 
    #' write a status to the status file
    #' @param msg is the message which should be set in the file
    setStatus = function(msg){
      write(msg, self$file)
    },

    #' @description 
    #' write data to the result file
    #' @param data is a string which should be written to the result file
    setData = function(data) {
      write(data, self$result)
    },

    #' @description 
    #' get the current data from the result file
    getData = function() {
      scan(self$result, what = "character", sep = "\n")  
    },

    #' @description 
    #' set the status to "interrupt"
    interrupt = function() {
      self$setStatus("interrupt")
    },

    #' @description 
    #' set the status to "ready"
    ready = function() {
      self$setStatus("ready")
    },

    #' @description 
    #' write a status to the status file. 
    #' @param percComplete is the message which should be set in the file.
    #'        If percComplete is not passed than the message is set to "Running..."
    running = function(percComplete) {
      msg <- "Running..."
      if (!missing(percComplete)) {
        msg <- paste0("Running... ", percComplete, "% Complete")  
      }
      self$setStatus(msg)  
    },

    #' @description 
    #' Checks if the current status is "interrupt"
    isInterrupted = function() {
      self$getStatus() == "interrupt"
    },

    #' @description 
    #' removes the temporary files. 
    #' \strong{This method has to be called at the end of the lifetime of the object!}
    destroy = function() {
      if (file.exists(self$file)) unlink(self$file)
      if (file.exists(self$result)) unlink(self$result)
    }
  )                            
)
