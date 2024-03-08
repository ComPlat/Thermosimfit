Communicator <- R6::R6Class("Communicator",
  public = list(
    file = NULL,
    result = NULL,
    initialize = function() {
      self$file = tempfile()
      self$result = tempfile()
      write("Ready", self$file)
      write("", self$result)
    },
    getStatus = function() {
      scan(self$file, what = "character", sep = "\n")  
    },
    setStatus = function(msg){
      write(msg, self$file)
    },
    setData = function(data) {
      write(data, self$result)
    },
    getData = function() {
      scan(self$result, what = "character", sep = "\n")  
    },
    interrupt = function() {
      self$setStatus("interrupt")
    },
    ready = function() {
      self$setStatus("ready")
    },
    running = function(percComplete) {
      msg <- "Running..."
      if (!missing(percComplete)) {
        msg <- paste0("Running... ", percComplete, "% Complete")  
      }
      self$setStatus(msg)  
    },
    isInterrupted = function() {
      self$getStatus() == "interrupt"
    },
    destroy = function() {
      if (file.exists(self$file)) unlink(self$file)
      if (file.exists(self$result)) unlink(self$result)
    }
  )                            
)
