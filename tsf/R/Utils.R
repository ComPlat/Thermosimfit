`%||%` <- function(x, y) if (is.null(x)) y else x

getAST <- function(inp) {
  if (!is.call(inp)) {
    return(inp)
  }
  inp <- as.list(inp)
  # check if is function
  fct <- inp[[1]]
  allowed_fcts <- c( "-", "+", "*", "/","^")
  check <- deparse(fct)
  if ((check %in% allowed_fcts) == FALSE) {
    return(ErrorClass$new(paste0("Error: function ", check ," not allowed")))
  }
  lapply(inp, getAST)
}

importData <- function(path) {
  if (!is.character(path)) {
    return(ErrorClass$new("path is not of type character"))
  }
  df <- try(as.data.frame(read_excel(path, col_names = TRUE)), silent = TRUE)
  if (class(df) != "try-error") {
    return(df)
  }
  line <- readLines(path, n = 1)
  semicolon <- grepl(";", line)
  comma <- grepl(",", line)
  tab <- grepl("\t", line)
  seperator <- NULL
  if (semicolon == TRUE) {
    seperator <- ";"
  } else if (comma == TRUE) {
    seperator <- ","
  } else if (tab == TRUE) {
    seperator <- "\t"
  } else {
    return(ErrorClass$new("Could not identify seperator in file"))
  }
  header <- FALSE
  firstLine <- try(readLines(path, n = 1L))
  if (class(firstLine) == "try-error") {
    return(ErrorClass$new("Could not read first line of file"))
  }
  firstLine <- strsplit(firstLine, split = seperator)[[1]]
  firstLine <- as.numeric(firstLine)
  if (all(is.na(firstLine))) header <- TRUE
  df <- try(read.csv(path, header = header, sep = seperator))
  if (class(df) == "try-error") {
    return(ErrorClass$new("Could not import data"))
  }
  if (ncol(df) < 2) {
    return(ErrorClass$new("Data has wrong dimensions, at least two columns were expected"))
  }
  if (nrow(df) == 0) {
    return(ErrorClass$new("Data has 0 rows"))
  }
  if (nrow(df) > 10000) {
    return(ErrorClass$new("Data has more than 10000 rows"))
  }
  if (any(is.na(df))) {
    return(ErrorClass$new("Data contains missing values"))
  }
  if (!all(sapply(df, is.numeric))) {
    return(ErrorClass$new("Data contains non-numeric values"))
  }
  return(df)
}

ErrorClass <- R6::R6Class(
  "ErrorClass",
  public = list(
    message = NULL,
    object = NULL,
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

Communicator <- R6::R6Class("Communicator",
  public = list(
    file = NULL,
    result = NULL,
    initialize = function() {
      self$file <- tempfile()
      self$result <- tempfile()
      write("Ready", self$file)
      write("", self$result)
    },
    getStatus = function() {
      scan(self$file, what = "character", sep = "\n")
    },
    setStatus = function(msg) {
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

in_shiny_app <- function() {
  requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()
}
in_reactive_context <- function() {
  requireNamespace("shiny", quietly = TRUE) && !is.null(shiny::getDefaultReactiveDomain())
}
in_batch <- function() {
  !in_shiny_app() && !in_reactive_context()
}

rel_err   <- function(yhat, y) {
  eps <- 1e-12
  sum(abs(y - yhat) / pmax(abs(y), eps))
}
rmse <- function(yhat, y) {
  r <- y - yhat
  sqrt(mean(r^2))
}
sse <- function(yhat, y) {
  r <- y - yhat
  sum(r^2)
}
huber <- function(yhat, y) {
  delta <- 0.5
  r <- abs(y - yhat)
  mean(ifelse(r <= delta, 0.5*r^2, delta*(r - 0.5*delta)))
}
get_error_calc_fct <- function(name) {
  list("rel. Error" = rel_err, "Rel. Error" = rel_err, RMSE = rmse, SSE = sse, Huber = huber)[[name]]
}

check_error_calc_function <- function(f) {
  stopifnot("error calc function has to be a function" = is.function(f))
  n_args <- formals(f)
  stopifnot("The error calc function should accept two arguments" = length(n_args) == 2)
}

# additional parameters utilities
correct_names_additional_param <- function(df, case) {
  if (case == "dba_host_const") {
    names(df) <- c("Host [M]")
  } else if (case == "dba_dye_const") {
    names(df) <- c("Dye [M]")
  } else if (case == "ida") {
    names(df) <- c("Host [M]", "Dye [M]", "Ka(HD) [1/M]")
  } else if (case == "gda") {
    names(df) <- c("Host [M]", "Guest [M]", "Ka(HD) [1/M]")
  }
  return(df)
}

# Parameter utilities
correct_names_params <- function(df, case, n_sigs) {
  if (case == "dba_host_const") {
    names(df)[1] <- "Ka(HD) [1/M]"
  } else if (case == "dba_dye_const") {
    names(df)[1] <- "Ka(HD) [1/M]"
  } else if (case == "ida") {
    names(df)[1] <- "Ka(HG) [1/M]"
  } else if (case == "gda") {
    names(df)[1] <- "Ka(HG) [1/M]"
  }
  names <- vapply(1:n_sigs,
    function(i) {
      paste0("Sig Nr.", i, " ", c("I(0)", "I(HD) [1/M]", "I(D) [1/M]"))
    }, character(3))
  names(df)[-1] <- names
  return(df)
}

create_params_df <- function(res, case, n_sigs) {
  df <- data.frame(t(res[[2]])) |>
    correct_names_params(case, n_sigs)
  return(df)
}

# data utilities
correct_names_data <- function(df, case) {
  nc <- ncol(df)
  last2 <- (nc-1):(nc)
  if (case == "dba_host_const") {
    names(df)[1] <- "total Dye measured [M]"
    names(df)[last2] <- c("free Dye simulated [M]", "Host-Dye simulated [M]")
  } else if (case == "dba_dye_const") {
    names(df)[1] <- "total Host measured [M]"
    names(df)[last2] <- c("free Dye simulated [M]", "Host-Dye simulated [M]")
  } else if (case == "ida") {
    names(df)[1] <- "total Guest measured [M]"
    names(df)[last2] <- c("free Dye simulated [M]", "Host-Dye simulated [M]")
  } else if (case == "gda") {
    names(df)[1] <- "total Dye measured [M]"
    names(df)[last2] <- c("free Dye simulated [M]", "Host-Dye simulated [M]")
  }
  return(df)
}
create_data_df <- function(df, res, case, n_sigs) {
  for (i in 2:(n_sigs + 1)) {
    names(df)[i] <- paste0("Sig. Nr. ", i - 1, " measured")
  }
  for (i in seq_len(n_sigs)) {
    df[[paste0("Sig. Nr. ", i, " simulated")]] <- res[[1]][, i]
  }
  df$d <- res[[1]][, n_sigs + 1]
  df$hd <- res[[1]][, n_sigs + 2]
  return(correct_names_data(df, case))
}

# plotting utilities
add_axis_labels <- function(p, case, ylabel) {
  case_df <- data.frame(
    dba_host_const = "total Dye measured [M]",
    dba_dye_const = "total Host measured [M]",
    ida = "total Guest measured [M]",
    gda = "total Dye measured [M]"
  )
  x_col <- case_df[case] |> as.character()
  p + xlab(x_col) + ylab(ylabel)
}

plot_signals <- function(df, case, nsigs) {
  case_df <- data.frame(
    dba_host_const = "total Dye measured [M]",
    dba_dye_const = "total Host measured [M]",
    ida = "total Guest measured [M]",
    gda = "total Dye measured [M]"
  )
  x_col <- case_df[case] |> as.character()

  dfs <- lapply(1:nsigs, function(i) {
    temp <- data.frame(
      df[[x_col]],
      df[[i + 1]],
      df[[i + nsigs + 1]]
    )
    names(temp) <- c(
      x_col,
      "Signal measured",
      "Signal simulated"
    )
    temp
  })
  base_size <- 10
  
  lapply(dfs, function(df) {
    sig_n <- parent.frame()$i[]
    p <- ggplot(data = df) +
      geom_point(data = df, aes(x = .data[[x_col]], .data[["Signal measured"]], colour = "Signal measured")) +
      geom_point(data = df, aes(x = .data[[x_col]], .data[["Signal simulated"]], colour = "Signal simulated")) +
      theme(
        legend.position = "bottom",
        axis.title = element_text(size = base_size * 1.2),
        axis.text = element_text(size = base_size),
        legend.text = element_text(size = base_size),
        legend.title = element_text(size = base_size),
        strip.text.x = element_text(size = base_size)
      ) +
      guides(colour = guide_legend(title = NULL)) +
      labs(title = paste0("Sig. Nr.", sig_n))
    add_axis_labels(p, case, "Signal [a.u]")
  })
}


plot_d_hd <- function(df, case, nsigs) {
  case_df <- data.frame(
    dba_host_const = "total Dye measured [M]",
    dba_dye_const = "total Host measured [M]",
    ida = "total Guest measured [M]",
    gda = "total Dye measured [M]"
  )
  x_col <- case_df[case] |> as.character()

  df_d <- data.frame(
    x = df[, x_col],
    y = df[, "free Dye simulated [M]"]
  )
  df_hd <- data.frame(
    x = df[, x_col],
    y = df[, "Host-Dye simulated [M]"]
  )
  base_size <- 10

  p1 <- ggplot(
    data = df_d,
    aes(x = x, y = y)
  ) +
    geom_point() +
    theme(
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size * 0.8),
      strip.text.x = element_text(size = base_size)
    )
  p1 <- add_axis_labels(p1, case, "Dye [M]")

  p2 <- ggplot(
    data = df_hd,
    aes(x = x, y = y)
  ) +
    geom_point() +
    theme(
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size * 0.8),
      strip.text.x = element_text(size = base_size)
    )
  p2 <- add_axis_labels(p2, case, "Host-Dye [M]")
  p1 + p2
}
