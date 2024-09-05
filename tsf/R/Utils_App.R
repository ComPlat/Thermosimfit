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

flush <- function(f) { # TODO: still needed?
  file_con <- file(f, open = "w")
  close(file_con)
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
      } else if (model == "hg" || model == "dba") {
        names(temp) <- c("Generation", "Ka(HD)", "I(0)", "I(HD)", "I(D)", "Error")
      }
      temp <- paste(paste0(names(temp), " = ", temp), collapse = "; ")
    }
    if (length(temp) == 7) {
      if (model == "ida" || model == "gda") {
        names(temp) <- c("", "Generation", "Ka(HG)", "I(0)", "I(HD)", "I(D)", "Error")
      } else if (model == "hg" || model == "dba") {
        names(temp) <- c("", "Generation", "Ka(HD)", "I(0)", "I(HD)", "I(D)", "Error")
      }
      temp <- ifelse(names(temp) != "", paste0(names(temp), " = ",temp), temp)
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
  showNotification(
    message,
    duration = duration,
    type = type
  )
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
  if(length(e) == 1) {
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
    type = "error", duration = 20)
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

call_opti_in_bg <- function(case, lb, ub,
                            df, ap, seed,
                            npop, ngen, topo,
                            et) {
  callr::r_bg(
    function(case, lb, ub, df, ap,
             seed, npop, ngen, Topology, errorThreshold) {
      res <- tsf::opti(
        case, lb, ub, df, ap, seed, npop, ngen, Topology, errorThreshold
      )
      return(res)
    },
    args = list(
      case, lb, ub, df,
      ap, seed, npop, ngen, topo, et
    )
  )
}

call_sensi_in_bg <- function(case, optim_params, df, ap, sense_bounds) {
  callr::r_bg(
    function(case, optim_params, df, ap, sense_bounds) {
      res <- tsf::sensitivity(
        case, optim_params, df, ap, sense_bounds
      )
      return(res)
    },
    args = list(case, optim_params, df, ap, sense_bounds)
  )
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
    return(0) # default value
  }
  return(round(number))
}

is_integer <- function(x) {
  return(is.numeric(x) && identical(round(x), x))
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


# download batch file
# ========================================================================================
create_df_for_batch <- function(list, what) {
  list <- list[[what]]
  df <- Reduce(rbind, list)
  return(df)
}

adjust_theme <- function(p) {
  base_size <- 4
  p <- p + theme(
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(10, 10, 10, 10),
    title = element_text(size = base_size, face = "bold"),
    axis.title = element_text(size = base_size, face = "bold"),
    axis.text = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    legend.key.size = unit(0.25, "cm"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5),
    strip.text.x = element_text(size = base_size, face = "bold"),
    strip.text.y = element_text(size = base_size, face = "bold")
  )
  return(p)
}

download_batch_file <- function(model, file, result_val) {
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

  p1 <- plotStates(result_val)
  temp_files_p1 <- lapply(seq_len(length(p1)), function(x) {
    tempfile(fileext = ".png")
  })

  for (i in seq_len(length(p1))) {
    p <- p1[[i]]
    f <- temp_files_p1[[i]]
    ggsave(f, plot = p,
      dpi = 600
    )
    insertImage(wb, "Results", f,
      startRow = curr_row
    )
    curr_row <- curr_row + 20
  }

  p2 <- plotParams(result_val)
  p3 <- plotMetrices(result_val)

  tempfile_plot2 <- tempfile(fileext = ".png")
  ggsave(tempfile_plot2,
    plot = p2
  )
  insertImage(wb, "Results", tempfile_plot2, startRow = curr_row)
  curr_row <- curr_row + 20

  tempfile_plot3 <- tempfile(fileext = ".png")
  ggsave(tempfile_plot3,
    plot = p3
  )
  insertImage(wb, "Results", tempfile_plot3, startRow = curr_row)
  curr_row <- curr_row + 20

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
  lapply(temp_files_p1, unlink)
  unlink(tempfile_plot2)
  unlink(tempfile_plot3)
}
