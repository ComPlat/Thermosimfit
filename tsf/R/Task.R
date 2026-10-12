library(callr)

# Reuses one persistent r_session across every call, instead of
# callr::r_bg()'s fresh process (and fresh ast2ast/Rcpp compile) per call.
Task <- R6::R6Class(
  "Task",
  public = list(
    r_session = NULL,
    args = NULL,
    fct = NULL,
    result = NULL,
    error = NULL,
    stdout = NULL,
    stderr = NULL,
    running_status = "Idle",

    initialize = function() {
      self$r_session <- callr::r_session$new()
    },

    start = function(fct, args = list()) {
      if (self$r_session$get_state() == "busy" && self$is_done()) {
        self$collect()
      }
      stopifnot(
        "Task is still busy with a previous call" = self$r_session$get_state() == "idle"
      )
      self$fct <- fct
      self$args <- args
      self$result <- NULL
      self$error <- NULL
      self$stdout <- NULL
      self$stderr <- NULL
      self$running_status <- "Running"
      self$r_session$call(self$fct, args = self$args)
      invisible(self)
    },

    is_done = function() {
      self$r_session$poll_process(0) == "ready"
    },

    # Only after is_done(): read() is what returns the session to "idle"
    # (able to start() again), so don't call it before that.
    collect = function() {
      res <- self$r_session$read()
      self$result <- res$result
      self$error <- res$error
      self$stdout <- res$stdout
      self$stderr <- res$stderr
      self$running_status <- if (is.null(self$error)) "Idle" else "Error"
      invisible(self)
    },

    # For non-Shiny (CLI) callers with no invalidateLater() loop available.
    wait = function(interval = 0.2) {
      while (!self$is_done()) {
        Sys.sleep(interval)
      }
      self$collect()
      invisible(self)
    },

    # interrupt(), not kill_tree(): aborts the call but keeps the process
    # alive for the next start().
    cancel = function() {
      if (self$r_session$get_state() != "busy") {
        return(invisible(self))
      }
      self$r_session$interrupt()
      self$r_session$poll_process(-1)
      self$collect()
      self$running_status <- "Cancelled"
      invisible(self)
    },

    get_result = function() {
      self$result
    },

    close = function() {
      if (!is.null(self$r_session) && self$r_session$is_alive()) {
        self$r_session$close()
      }
      invisible(self)
    },

    destructor = function() {
      self$close()
    }
  )
)
