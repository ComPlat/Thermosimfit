# Send information to golang
# ========================================================================================
send_and_read_info <- function(message) {
  # NOTE: In case it is run on a server the environment variable
  # SERVER_ENV should be defined (Sys.setenv(TEST_ENV = "TRUE"))
  on_server <- Sys.getenv("SERVER_ENV", unset = "FALSE") == "TRUE"
  if (!on_server) {
    return()
  }
  if (length(message) == 0) {
    return()
  }
  con <- socketConnection(
    host = "localhost",
    port = 8080, blocking = TRUE,
    server = FALSE,
    open = "w+b"
  )
  bind <- function(a, b) {
    paste(a, b, collapse = " , ")
  }
  m <- Reduce(bind, as.character(message))
  writeLines(m, con)
  response <- readLines(con, warn = FALSE)
  close(con)
  return(response)
}

# Formating stuff
# ========================================================================================
format_scientific <- function(x) {
  formatC(x, format = "e", digits = 3)
}

extract_iter <- function(s) { # TODO: still needed?
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

# print intermediate results
# ========================================================================================

print_status <- function(stdcout, model) {
  temp <- strsplit(stdcout, "\n")[[1]]
  if (length(temp) >= 4) {
    temp <- lapply(temp, function(x) {
      x <- gsub('"', "", x)
      x <- gsub("\\[.*?\\] ", "", x)
    })
    temp[[3]] <- strsplit(temp[[3]], " ")[[1]]
    if (temp[[1]] != "") {
      temp <- c(temp[[1]], temp[[2]], c(temp[[3]]), c(temp[[4]]))
    } else {
      temp <- c(temp[[2]], c(temp[[3]]), c(temp[[4]]))
    }
    if (length(temp) == 6) {
      if (model == "ida" || model == "gda") {
        names(temp) <- c("Generation", "Ka(HG)", "I(0)", "I(HD)", "I(D)", "Error")
      } else if (model == "dba_host_const" || model == "dba_dye_const") {
        names(temp) <- c("Generation", "Ka(HD)", "I(0)", "I(HD)", "I(D)", "Error")
      }
      temp <- paste(paste0(names(temp), " = ", temp), collapse = "; ")
    }
    if (length(temp) == 7) {
      if (model == "ida" || model == "gda") {
        names(temp) <- c("", "Generation", "Ka(HG)", "I(0)", "I(HD)", "I(D)", "Error")
      } else if (model == "dba_host_const" || model == "dba_dye_const") {
        names(temp) <- c("", "Generation", "Ka(HD)", "I(0)", "I(HD)", "I(D)", "Error")
      }
      temp <- ifelse(names(temp) != "", paste0(names(temp), " = ", temp), temp)
      temp <- paste(temp, collapse = "; ")
    }
    return(temp)
  } else {
    return("")
  }
}

format_batch_status <- function(stdout, temp) {
  if (length(stdout) != length(temp)) {
    return("")
  }
  for (i in seq_along(stdout)) {
    if (stdout[i] == "") {
      stdout[i] <- temp[i]
    } else if ((stdout[i] != temp[i]) && (temp[i] != "")) {
      stdout[i] <- temp[i]
    }
  }
  return(stdout)
}

# print notification
# ========================================================================================
print_noti <- function(message, type = "warning", duration = 15) {
  if (in_batch()) {
    print(message)
  } else {
    showNotification(
      message,
      duration = duration,
      type = type
    )
  }
}

# require with notificiation
# ========================================================================================
rwn <- function(expr, message, type = "warning", duration = 15) {
  if (!expr) {
    print_noti(
      message,
      duration = duration,
      type = type
    )
  }
  req(expr)
}

# print errors
# ========================================================================================
format_error <- function(e) {
  if (length(e) == 0) {
    return()
  }
  if (nchar(e) == 0) {
    return()
  }
  e <- strsplit(e, "\n")[[1]]
  if (length(e) == 1) {
    return(e)
  }
  e_rest <- lapply(e[2:length(e)], function(x) {
    paste("<br>", x, "</br>")
  })
  e_rest <- Reduce(paste0, e_rest)
  HTML(c(e[[1]], e_rest))
}


print_error <- function(e) {
  if (length(e) == 0) {
    return()
  }
  if (nchar(e) == 0) {
    return()
  }
  showNotification(format_error(e),
    type = "error", duration = 20
  )
}

# helper
# ========================================================================================
convertToNum <- function(expr) {
  res <- sapply(expr, function(x) {
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

  res <- sapply(expr, function(x) {
    res <- try(eval(parse(text = x)))
    if (inherits(res, "try-error")) {
      showNotification(res)
      return("Error")
    }
    return(res)
  })
  return(res)
}

convert_all_to_num <- function(what, ...) {
  v <- c(...)
  v <- convertToNum(v)
  if (any("Error" %in% v)) {
    rwn(FALSE, paste0("The ", what, " cannot be converted into a numeric value"))
  }
  return(v)
}

request_cores <- function(n_cores, token) {
  # NOTE: In case it is run on a server the environment variable
  # SERVER_ENV should be defined (Sys.setenv(TEST_ENV = "TRUE"))
  on_server <- Sys.getenv("SERVER_ENV", unset = "FALSE") == "TRUE"
  if (!on_server) {
    return()
  }
  if (!is.null(globalenv()$server)) {
    if (!globalenv()$server) {
      return()
    }
  }
  status <- send_and_read_info(paste0("request: ", token, " :", n_cores))
  if (status == "Exceeded core limit") {
    rwn(
      FALSE,
      "Exceed core limit.
           Please try again later."
    )
  }
  rwn(
    status == "Cores allocated",
    "Could not allocate cores. Please try again later"
  )
}

# Warms Rcpp's compile cache for every case in this process; return value unused.
warm_ast2ast_cache <- function() {
  for (case in c("dba_dye_const", "dba_host_const", "ida", "gda")) {
    pso_spec <- tsf:::pso_a2a_spec(case)
    ast2ast::translate(pso_spec$loss_fct, types_f = pso_spec$types_f)

    vapro_spec <- tsf:::vapro_a2a_spec(case)
    ast2ast::translate(vapro_spec$loss_fct, types_f = vapro_spec$types_f)
    ast2ast::translate(vapro_spec$grid_fct, types_f = vapro_spec$types_f)
  }
  invisible(TRUE)
}

determine_seed_case <- function(seed, num_rep) {
  if (num_rep > 1 && !is.na(seed)) {
    print_noti("Found number of replications > 1 and a seed was defined.
          Only for the first analysis of each dataset respectivly,
          the seed which will be used.")
  }
  if (is.na(seed)) {
    seed_case <- 1
  } else {
    if (num_rep == 1) {
      seed_case <- 2
    } else if (num_rep > 1) {
      seed_case <- 3
    }
  }
  return(seed_case)
}

# integer stuff
# ========================================================================================
convert_num_to_int <- function(number) {
  if (!is.numeric(number)) {
    return(0L) # default value
  }
  return(as.integer(round(number)))
}

is_integer <- function(x) {
  return(is.numeric(x) && x == round(x))
}

# download file
# ========================================================================================
download_file <- function(model, file, result_val) {
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "Results")
  writeData(wb, "Results",
    paste0("Model: ", model),
    startCol = 1,
    startRow = 1
  )

  curr_row <- 3
  data_trajectories <- result_val$data
  writeData(wb, "Results", data_trajectories, startRow = curr_row)
  curr_row <- curr_row + dim(data_trajectories)[1] + 5

  parameter <- result_val$parameter
  lb <- result_val$lowerBounds
  ub <- result_val$upperBounds
  parameter <- rbind(parameter, lb)
  parameter <- rbind(parameter, ub)
  parameter <- cbind(
    info = c("Opti. results", "lower boundaries", "upper boundaries"),
    parameter
  )
  writeData(wb, "Results", parameter, startRow = curr_row)
  curr_row <- curr_row + dim(parameter)[1] + 5

  metrices <- result_val$metrices
  writeData(wb, "Results", metrices, startRow = curr_row)
  curr_row <- curr_row + dim(metrices)[1] + 5

  tempfile_plots <- list()

  d_hd_plot_file <- tempfile(fileext = ".png")
  ggsave(d_hd_plot_file,
    plot = result_val$d_hd_plot, width = 15, height = 15, limitsize = FALSE
  )
  tempfile_plots[[1]] <- d_hd_plot_file
  insertImage(wb, "Results", d_hd_plot_file, startRow = curr_row)
  curr_row <- curr_row + 15

  ps <- result_val$signal_plots
  for (i in seq_len(length(ps))) {
    plot_file <- tempfile(fileext = ".png")
    ggsave(plot_file, plot = ps[[i]], width = 15, height = 15, limitsize = FALSE)
    tempfile_plots[[length(tempfile_plots) + 1]] <- plot_file
    insertImage(wb, "Results", plot_file, startRow = curr_row)
    curr_row <- curr_row + 15
  }

  add_info <- data.frame(
    as.data.frame(t(result_val$additionalParameters)),
    npop = result_val$npop,
    ngen = result_val$ngen,
    topology = result_val$Topology,
    seed = result_val$seed
  )
  writeData(
    wb, "Results",
    add_info,
    startRow = curr_row
  )
  curr_row <- curr_row + 5

  writeData(wb, "Results",
    as.data.frame(R.Version()),
    startRow = curr_row
  )
  curr_row <- curr_row + 5

  writeData(wb, "Results",
    paste0("tsf version: ", packageVersion("tsf")),
    startRow = curr_row
  )

  openxlsx::saveWorkbook(wb, file)
  lapply(tempfile_plots, unlink)
}

download_csv <- function(model, file, result_val) {
  # csv file
  write.table(paste0("Model: ", model), file)
  data_trajectories <- result_val$data
  write.table(data_trajectories, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )

  parameter <- result_val$parameter
  lb <- result_val$lowerBounds
  ub <- result_val$upperBounds
  parameter <- rbind(parameter, lb)
  parameter <- rbind(parameter, ub)
  parameter <- cbind(
    info = c("Opti. results", "lower boundaries", "upper boundaries"),
    parameter
  )
  write.table(parameter, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )

  metrices <- result_val$metrices
  write.table(metrices, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )

  add_info <- data.frame(
    as.data.frame(t(result_val$additionalParameters)),
    npop = result_val$npop,
    ngen = result_val$ngen,
    topology = result_val$Topology,
    seed = result_val$seed
  )
  write.table(add_info, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )
  write.table(as.data.frame(R.Version()), file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )
  write.table(
    as.data.frame(paste0("tsf version: ", packageVersion("tsf"))),
    file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )
}

# VAPRO's lowerBounds/upperBounds are a single-column data.frame (only the
# nonlinear binding constant is searched; I0/IHD/ID are profiled via NNLS),
# unlike PSO's, which has one column per parameter matching `parameter` - so
# they can't be rbind-ed onto `parameter` the way download_file/download_csv
# do, and nGrid replaces npop/ngen/Topology/seed in the run-info block.
download_file_vapro <- function(model, file, result_val) {
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "Results")
  writeData(wb, "Results",
    paste0("Model: ", model, " (VAPRO)"),
    startCol = 1,
    startRow = 1
  )

  curr_row <- 3
  data_trajectories <- result_val$data
  writeData(wb, "Results", data_trajectories, startRow = curr_row)
  curr_row <- curr_row + dim(data_trajectories)[1] + 5

  parameter <- result_val$parameter
  writeData(wb, "Results", parameter, startRow = curr_row)
  curr_row <- curr_row + dim(parameter)[1] + 3

  bounds <- data.frame(
    info = c("lower boundary", "upper boundary"),
    value = c(result_val$lowerBounds[[1]], result_val$upperBounds[[1]])
  )
  names(bounds)[2] <- names(result_val$lowerBounds)[1]
  writeData(wb, "Results", bounds, startRow = curr_row)
  curr_row <- curr_row + dim(bounds)[1] + 5

  metrices <- result_val$metrices
  writeData(wb, "Results", metrices, startRow = curr_row)
  curr_row <- curr_row + dim(metrices)[1] + 5

  tempfile_plots <- list()

  d_hd_plot_file <- tempfile(fileext = ".png")
  ggsave(d_hd_plot_file,
    plot = result_val$d_hd_plot, width = 15, height = 15, limitsize = FALSE
  )
  tempfile_plots[[1]] <- d_hd_plot_file
  insertImage(wb, "Results", d_hd_plot_file, startRow = curr_row)
  curr_row <- curr_row + 15

  ps <- result_val$signal_plots
  for (i in seq_len(length(ps))) {
    plot_file <- tempfile(fileext = ".png")
    ggsave(plot_file, plot = ps[[i]], width = 15, height = 15, limitsize = FALSE)
    tempfile_plots[[length(tempfile_plots) + 1]] <- plot_file
    insertImage(wb, "Results", plot_file, startRow = curr_row)
    curr_row <- curr_row + 15
  }

  add_info <- data.frame(
    as.data.frame(t(result_val$additionalParameters)),
    nGrid = result_val$nGrid
  )
  writeData(
    wb, "Results",
    add_info,
    startRow = curr_row
  )
  curr_row <- curr_row + 5

  writeData(wb, "Results",
    as.data.frame(R.Version()),
    startRow = curr_row
  )
  curr_row <- curr_row + 5

  writeData(wb, "Results",
    paste0("tsf version: ", packageVersion("tsf")),
    startRow = curr_row
  )

  openxlsx::saveWorkbook(wb, file)
  lapply(tempfile_plots, unlink)
}

download_csv_vapro <- function(model, file, result_val) {
  write.table(paste0("Model: ", model, " (VAPRO)"), file)
  data_trajectories <- result_val$data
  write.table(data_trajectories, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )

  parameter <- result_val$parameter
  write.table(parameter, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )

  bounds <- data.frame(
    info = c("lower boundary", "upper boundary"),
    value = c(result_val$lowerBounds[[1]], result_val$upperBounds[[1]])
  )
  names(bounds)[2] <- names(result_val$lowerBounds)[1]
  write.table(bounds, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )

  metrices <- result_val$metrices
  write.table(metrices, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )

  add_info <- data.frame(
    as.data.frame(t(result_val$additionalParameters)),
    nGrid = result_val$nGrid
  )
  write.table(add_info, file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )
  write.table(as.data.frame(R.Version()), file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )
  write.table(
    as.data.frame(paste0("tsf version: ", packageVersion("tsf"))),
    file,
    append = TRUE,
    sep = ",", row.names = FALSE
  )
}


# download batch file
# ========================================================================================
create_df_for_batch <- function(list, what) {
  list <- list[[what]]
  df <- Reduce(rbind, list)
  return(df)
}

insert_plot_batch <- function(wb, p, start_row, env_files) {
  f <- tempfile(fileext = ".png")
  ggsave(f, plot = p, dpi = 600)
  insertImage(wb, "Results", f, startRow = start_row)
  env_files$l <- c(env_files$l, f)
  start_row + 20
}

download_batch_file <- function(model, file, list) {
  env_files <- new.env(); env_files$l <- list()
  result_val <- list[[1]]
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "Results")
  writeData(wb, "Results",
    paste0("Model: ", model),
    startCol = 1,
    startRow = 1
  )

  curr_row <- 3
  data_trajectories <- create_df_for_batch(result_val, "states")
  writeData(wb, "Results", data_trajectories, startRow = curr_row)
  curr_row <- curr_row + dim(data_trajectories)[1] + 5

  parameter <- create_df_for_batch(result_val, "params")
  writeData(wb, "Results", parameter, startRow = curr_row)
  curr_row <- curr_row + dim(parameter)[1] + 5

  lb <- as.data.frame(t(result_val$lowerBounds))
  lb$info <- "Lower bounds"
  ub <- as.data.frame(t(result_val$upperBounds))
  ub$info <- "Upper bounds"
  boundaries <- rbind(lb, ub)
  writeData(wb, "Results", boundaries, startRow = curr_row)
  curr_row <- curr_row + dim(boundaries)[1] + 5

  metrices <- create_df_for_batch(result_val, "metrices")
  writeData(wb, "Results", metrices, startRow = curr_row)
  curr_row <- curr_row + dim(metrices)[1] + 5

  # Ka plots
  ka_plots <- list$ka_plots
  curr_row <- insert_plot_batch(wb, ka_plots[[1]], curr_row, env_files)
  for (i in seq_len(length(ka_plots[[2]]))) {
    curr_row <- insert_plot_batch(wb, ka_plots[[2]][[i]], curr_row, env_files)
  }

  hd_d_dataset_plots <- list$hd_d_plots
  for (i in seq_len(length(hd_d_dataset_plots))) {
    curr_row <- insert_plot_batch(wb, hd_d_dataset_plots[[i]], curr_row, env_files)
  }

  I_plots <- list$i_param_plots
  for (i in seq_len(length(I_plots))) {
    ps_per_signal <- I_plots[[i]]
    for (j in seq_len(length(ps_per_signal))) {
      curr_row <- insert_plot_batch(wb, ps_per_signal[[j]], curr_row, env_files)
    }
  }

  Sig_plots <- list$state_plots
  for (i in seq_len(length(Sig_plots))) {
    ps_per_signal <- Sig_plots[[i]]
    for (j in seq_len(length(ps_per_signal))) {
      curr_row <- insert_plot_batch(wb, ps_per_signal[[j]], curr_row, env_files)
    }
  }

  add_info <- result_val$additionalParameters |>
    t() |>
    as.data.frame()
  add_info <- cbind(
    add_info,
    data.frame(
      npop = result_val$npop,
      ngen = result_val$ngen,
      Topology = result_val$Topology
    )
  )
  writeData(
    wb, "Results",
    add_info,
    startRow = curr_row
  )
  curr_row <- curr_row + 5

  seeds <- result_val$seeds |>
    unlist() |>
    as.data.frame()
  names(seeds) <- "Seeds"
  writeData(
    wb, "Results",
    seeds,
    startRow = curr_row
  )
  curr_row <- curr_row + 5

  writeData(wb, "Results",
    as.data.frame(R.Version()),
    startRow = curr_row
  )
  curr_row <- curr_row + 5

  writeData(wb, "Results",
    paste0("tsf version: ", packageVersion("tsf")),
    startRow = curr_row
  )

  openxlsx::saveWorkbook(wb, file)
  lapply(env_files$l, unlink)
}
