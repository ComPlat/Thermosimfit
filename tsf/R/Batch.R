importDataBatchRaw <- function(path) {
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
  if (!header) {
    ErrorClass$new("Header have to be supplied in batch mode")
  }
  df <- try(read.csv(path, header = FALSE, sep = seperator))
  if (class(df) == "try-error") {
    return(ErrorClass$new("Could not import data"))
  }
  return(df)
}

parseData <- function(path) {
  df <- importDataBatchRaw(path)
  if (!is.data.frame(df)) {
    return(df)
  }
  header <- df[1, ]
  start_rows <- apply(df, MARGIN = 1, function(x) {
    all(x == header)
  }) |>
    which()
  end_rows <- numeric(length(start_rows))
  for (i in seq_along(start_rows)) {
    if (i == length(start_rows)) {
      end_rows[i] <- nrow(df)
    } else {
      end_rows[i] <- start_rows[i + 1] - 1
    }
  }
  Map(function(start, end) {
    df[start:end, ]
  }, start_rows, end_rows)
}

convertDataToNum <- function(list) {
  Map(function(x) {
    temp <- as.data.frame(x[2:nrow(x), ])
    names(temp) <- x[1, ]
    temp <- apply(temp, MARGIN = 2, function(x) {
      as.numeric(x)
    })
    as.data.frame(temp)
  }, list)
}

importDataBatch <- function(path) {
  list <- parseData(path)
  convertDataToNum(list)
}

seperate_batch_results <- function(list) {
  # TODO: dont use indices but use names
  states <- lapply(list, function(x) {
    x[[1]]
  })
  params <- lapply(list, function(x) {
    x[[2]]
  })
  metrices <- lapply(list, function(x) {
    x[[5]]
  })
  seeds <- lapply(list, function(x) {
    x$seed
  })
  list(
    states = states, params = params, metrices = metrices,
    lowerBounds = list[[1]]$lowerBounds,
    upperBounds = list[[1]]$upperBounds,
    additionalParameters = list[[1]]$additionalParameters,
    seeds = seeds, npop = list[[1]]$npop, ngen = list[[1]]$ngen,
    Topology = list[[1]]$Topology
  )
}

run_batch_sequential_pso <- function(case, lb, ub, dfs, ap, seeds,
                                      npop, ngen, topo, ecf, et, messages) {
  df_reps <- lapply(messages, DfRepCombi$new)
  results <- vector("list", length(dfs))
  for (i in seq_along(dfs)) {
    res <- tryCatch(
      tsf::opti(case, lb, ub, dfs[[i]], ap, seeds[i], npop, ngen, topo, et, ecf,
        add_info = messages[[i]]
      ),
      error = function(e) NULL
    )
    if (!is.null(res) && !inherits(res, "ErrorClass")) {
      res$data$repetition <- df_reps[[i]]$rep
      res$data$dataset <- df_reps[[i]]$df
      res$parameter$repetition <- df_reps[[i]]$rep
      res$parameter$dataset <- df_reps[[i]]$df
      res$metrices$repetition <- df_reps[[i]]$rep
      res$metrices$dataset <- df_reps[[i]]$df
    } else {
      res <- NULL
    }
    results[[i]] <- res
    cat(sprintf("BATCH_JOB_DONE %d/%d\n", i, length(dfs)))
    flush(stdout())
  }
  results
}
