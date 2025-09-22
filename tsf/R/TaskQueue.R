DfRepCombi <- R6::R6Class(
  "DfRepCombi",
  public = list(
    df = NULL,
    rep = NULL,
    initialize = function(message) {
      m <- strsplit(message, ";")[[1]]
      df_m <- gsub("Dataset = ", "", m[[1]]) |> as.numeric()
      rep_m <- gsub("Replicate = ", "", m[[2]]) |> as.numeric()
      self$df <- df_m
      self$rep <- rep_m
    }
  )
)

Process <- R6::R6Class(
  "Process",
  public = list(
    process = NULL,
    idx = 0,
    initialize = function(process, idx) {
      self$process <- process
      self$idx <- idx
    },
    get_status = function() {
      self$process$get_status()
    },
    is_alive = function() {
      self$process$is_alive()
    },
    get_result = function() {
      self$process$get_result()
    },
    read_output = function() {
      self$process$read_output()
    },
    interrupt = function() {
      self$process$interrupt()
    },
    wait = function() {
      self$process$wait()
    },
    kill = function() {
      self$process$kill()
    }
  )
)

# TODO: test edge cases so that no indxe error occur anymore
TaskQueue <- R6::R6Class(
  "TaskQueue",
  public = list(
    dfs = NULL,
    seeds = NULL,
    messages = NULL,
    assigned = NULL,
    sz = NULL,
    df_reps = NULL,
    case = NULL,
    lb = NULL,
    ub = NULL,
    ap = NULL,
    npop = NULL,
    ngen = NULL,
    topo = NULL,
    ecf = NULL,
    et = NULL,
    num_cores = NULL,

    processes = list(),
    results = NULL,
    filled = FALSE,
    interrupted_at = NULL,
    current_opti = 0,
    progress = 0,

    initialize = function(case, lb, ub, dfs,
                          ap, seeds, npop, ngen,
                          topo, ecf, et,
                          messages, num_cores) {
      self$case <- case
      self$lb <- lb
      self$ub <- ub
      self$dfs <- dfs
      self$ap <- ap
      self$seeds <- seeds
      self$npop <- npop
      self$ngen <- ngen
      self$topo <- topo
      self$ecf <- ecf
      self$et <- et
      self$messages <- messages
      self$sz <- length(dfs)
      self$results <- vector("list", self$sz)
      self$assigned <- logical(self$sz)
      self$df_reps <- lapply(seq_len(self$sz), function(i) {
        DfRepCombi$new(messages[[i]])
      })
      self$num_cores <- num_cores
      self$processes <- vector("list", num_cores)
      self$filled <- TRUE
    },

    set_results = function(process) {
      results <- process$get_result()
      idx_res <- length(self$results) + 1
      self$results[[idx_res]] <- results
      idx <- process$idx
      self$results[[idx_res]]$data$repetition <- self$df_reps[[idx]]$rep
      self$results[[idx_res]]$data$dataset <- self$df_reps[[idx]]$df
      self$results[[idx_res]]$parameter$repetition <- self$df_reps[[idx]]$rep
      self$results[[idx_res]]$parameter$dataset <- self$df_reps[[idx]]$df
      self$results[[idx_res]]$metrices$repetition <- self$df_reps[[idx]]$rep
      self$results[[idx_res]]$metrices$dataset <- self$df_reps[[idx]]$df
      self$progress <- self$progress + 1
    },

    assign_task = function() {
      idx <- which(!self$assigned)[1]
      if (is.na(idx)) return(NULL)
      df <- self$dfs[[idx]]
      seed <- self$seeds[idx]
      m <- self$messages[idx]
      process <- callr::r_bg(
        function(case, lb, ub, df, ap,
                 seed, npop,
                 ngen, topology, error_threshold, ecf, messages) {
          res <- tsf::opti(
            case, lb, ub, df, ap, seed,
            npop, ngen, topology, error_threshold, ecf, messages
          )
          return(res)
        },
        args = list(
          self$case, self$lb, self$ub, df, self$ap,
          seed, self$npop, self$ngen, self$topo, self$et, self$ecf, m
        )
      )
      self$assigned[idx] <- TRUE
      self$current_opti <- self$current_opti + 1
      return(Process$new(process, self$current_opti))
    },

    # TODO: RAM usge is high
    # TODO: if a process is finished a new task
    # is first assigned after all processes are finished
    assign = function() {
      if (any(!self$assigned)) {
        size <- ifelse(self$num_cores > length(which(!self$assigned)),
                       length(which(!self$assigned)), self$num_cores)
        for (i in seq_len(size)) {
          if (inherits(self$processes[[i]], "Process")) {
            self$set_results(self$processes[[i]])
            if (self$processes[[i]]$is_alive()) {
              self$processes[[i]]$kill()
            }
          }
          if (self$queue_empty()) {
            next
          }
          process <- self$assign_task()
          self$processes[[i]] <- process
        }
        self$processes <- Filter(Negate(is.null), self$processes)
      }
    },

    check = function() {
      for (i in seq_len(length(self$processes))) {
        if (self$processes[[i]]$is_alive()) {
          return(FALSE)
        }
      }
      return(TRUE)
    },

    get_results = function() {
      for (i in seq_len(length(self$processes))) {
        if (!self$processes[[i]]$is_alive()) {
          self$set_results(self$processes[[i]])
        }
      }
    },

    interrupt = function() {
      for (i in seq_len(length(self$processes))) {
        if (self$processes[[i]]$is_alive()) {
          status <- self$processes[[i]]$get_status()
          self$processes[[i]]$interrupt()
          self$processes[[i]]$wait()
          if (status == "running" && self$processes[[i]]$is_alive()) {
            self$set_results(self$processes[[i]]$get_result())
          }
          self$processes[[i]]$kill()
        }
      }
      self$interrupted_at <- which(!self$assigned)[1]
      self$assigned <- TRUE
    },

    get_status = function(stdout) {
      status <- character(length(self$processes))
      for (i in seq_len(length(self$processes))) {
        if (self$processes[[i]]$is_alive()) { # TODO: can be removed. But maybe slows down code?
          status[i] <-
            print_status(self$processes[[i]]$read_output(), self$case)
        }
      }
      return(format_batch_status(stdout, status))
    },

    kill = function() {
      for (i in seq_len(length(self$processes))) {
        if (self$processes[[i]]$is_alive()) {
          self$processes[[i]]$kill()
        }
      }
    },

    seperate_results = function() {
      self$get_results()
      self$results <- Filter(Negate(is.null), self$results)
      self$results <- seperate_batch_results(self$results)
      return(self$results)
    },

    queue_empty = function() {
      all(self$assigned) &&
        all(sapply(self$processes, function(p) !p$is_alive()))
    },

    rep_own = function(elem, num) {
      if (num <= 0) return("")
      rep(elem, num)
    },

    get_progress_bar = function() {
      size <- length(self$assigned)
      if (self$progress == 0) {
        pb <- Reduce(paste0, self$rep_own(" ", size))
        return(paste0("[", pb, "]"))
      }
      done <- self$rep_own("=", self$progress)
      if (length(done) == size) {
        return(paste0("[", done, "]"))
      }
      todo <- self$rep_own(" ", size - length(done))
      paste0("[", paste0(c(done, todo), collapse = ""), "]")
    }
  )
)
