# Send information to golang
# ========================================================================================
send_and_read_info <- function(message) {
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
# ========================================================================================
print_noti <- function(message, type = "warning", duration = 10) {
  showNotification(
    message,
    duration = duration,
    type = type
  )
}

# require with notificiation
# ========================================================================================
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
# ========================================================================================
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

convert_all_to_num <- function(what, ...) {
  v <- c(...)
  v <- convertToNum(v)
  if (any("Error" %in% v)) {
    rwn(FALSE, paste0("The ", what, " cannot be converted into a numeric value"))
  }
  return(v)
}

# integer stuff
# ========================================================================================
convert_num_to_int <- function(number) {
  if (!is.numeric(number)) {
    return(0) # default value
  }
  return(round(number))
}

is_integer <- function(x) {
  return(is.numeric(x) && identical(round(x), x))
}

# download file
# ========================================================================================
download_file <- function(file, result_val) {
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "Results")
  writeData(wb, "Results",
    "Model: IDA", # TODO: add model name as argument
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

  p <- result_val$plot
  tempfile_plot <- tempfile(fileext = ".png")
  ggsave(tempfile_plot,
    plot = p, width = 15, height = 15, limitsize = FALSE
  )
  insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
  curr_row <- curr_row + 15

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
  unlink(tempfile_plot)
}


download_csv <- function(file, result_val) {
  # csv file
  write.table("Model: IDA", file) # TODO: pass model anem as argument
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


# download batch file
# ========================================================================================
create_df_for_batch <- function(list, what, num_rep) { # TODO: use this fct also in the plotting fcts of Batch.R
  list <- list[[what]]
  num_data_sets <- length(list) / num_rep
  repetitions <- (seq_len(length(list)) - 1) %% num_rep + 1
  data_sets <- rep(1:num_data_sets, each = num_rep)
  for (i in seq_along(list)) {
    list[[i]]$dataset <- data_sets[i]
    list[[i]]$repetition <- repetitions[i]
  }
  df <- Reduce(rbind, list)
  return(df)
}

download_batch_file <- function(file, result_val, num_rep) {
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "Results")
  writeData(wb, "Results",
    "Model: IDA",
    startCol = 1,
    startRow = 1
  )

  curr_row <- 3
  data_trajectories <- create_df_for_batch(result_val, "states", num_rep)
  writeData(wb, "Results", data_trajectories, startRow = curr_row)
  curr_row <- curr_row + dim(data_trajectories)[1] + 5

  parameter <- create_df_for_batch(result_val, "params", num_rep)
  writeData(wb, "Results", parameter, startRow = curr_row)
  curr_row <- curr_row + dim(parameter)[1] + 5

  lb <- as.data.frame(t(result_val$lowerBounds))
  lb$info <- "Lower bounds"
  ub <- as.data.frame(t(result_val$upperBounds))
  ub$info <- "Upper bounds"
  boundaries <- rbind(lb, ub)
  writeData(wb, "Results", boundaries, startRow = curr_row)
  curr_row <- curr_row + dim(boundaries)[1] + 5

  metrices <- create_df_for_batch(result_val, "metrices", num_rep)
  writeData(wb, "Results", metrices, startRow = curr_row)
  curr_row <- curr_row + dim(metrices)[1] + 5

  p1 <- plotStates(result_val, num_rep)
  p2 <- plotParams(result_val, num_rep)
  p3 <- plotMetrices(result_val, num_rep)

  # TODO: All plots are super ugly. Fix this
  tempfile_plot1 <- tempfile(fileext = ".png")
  ggsave(tempfile_plot1,
    plot = p1, width = 10, height = 10, limitsize = FALSE
  )
  insertImage(wb, "Results", tempfile_plot1,
    startRow = curr_row
  )
  curr_row <- curr_row + 20

  tempfile_plot2 <- tempfile(fileext = ".png")
  ggsave(tempfile_plot2,
    plot = p2, width = 10, height = 10, limitsize = FALSE
  )
  insertImage(wb, "Results", tempfile_plot2, startRow = curr_row)
  curr_row <- curr_row + 20

  tempfile_plot3 <- tempfile(fileext = ".png")
  ggsave(tempfile_plot3,
    plot = p3, width = 10, height = 10, limitsize = FALSE
  )
  insertImage(wb, "Results", tempfile_plot3, startRow = curr_row)
  curr_row <- curr_row + 20


  add_info <- data.frame(
    as.data.frame(t(result_val$additionalParameters)),
    npop = result_val$npop,
    ngen = result_val$ngen,
    topology = result_val$Topology
  )
  writeData(
    wb, "Results",
    add_info,
    startRow = curr_row
  )
  curr_row <- curr_row + 5

  seeds <- result_val$seeds
  seeds <- as.data.frame(seeds)
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
  unlink(tempfile_plot1)
  unlink(tempfile_plot2)
  unlink(tempfile_plot3)
}
