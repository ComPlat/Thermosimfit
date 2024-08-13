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
  states <- lapply(list, function(x) {
    x[[1]]
  })
  params <- lapply(list, function(x) {
    x[[2]]
  })
  metrices <- lapply(list, function(x) {
    x[[4]]
  })
  list(states = states, params = params, metrices = metrices)
}

plotStates <- function(list) {
  list <- list[[1]]
  for (i in seq_along(list)) {
    list[[i]]$run <- i
  }
  df <- Reduce(rbind, list)
  var <- names(df)[1]
  df <- tidyr::pivot_longer(df, cols = -c("run", var))
  df <- as.data.frame(df)
  ggplot() +
    geom_boxplot(data = df, aes(x = df[, var], y = value, group = df[, var])) +
    facet_wrap(~name, scales = "free_y")
}

plotParams <- function(list) {
  list <- list[[2]]
  for (i in seq_along(list)) {
    list[[i]]$run <- i
  }
  df <- Reduce(rbind, list)
  df <- tidyr::pivot_longer(df, cols = -c("run"))
  df <- as.data.frame(df)
  ggplot() +
    geom_boxplot(data = df, aes(, y = value)) +
    facet_wrap(~name, scales = "free_y")
}

plotMetrices <- function(list) {
  list <- list[[3]]
  list <- lapply(list, unlist)
  df <- Reduce(rbind, list) |> as.data.frame()
  df$run <- 1:nrow(df)
  df <- tidyr::pivot_longer(df, cols = -c("run"))
  ggplot() +
    geom_boxplot(data = df, aes(, y = value)) +
    facet_wrap(~name, scales = "free_y")
}

batch <- function(case,
                  lowerBounds, upperBounds,
                  path,
                  additionalParameters,
                  seed = NULL, npop = 40, ngen = 200, Topology = "random",
                  errorThreshold = -Inf, runAsShiny = FALSE) {
  if (!is.list(path)) {
    list_df <- importDataBatch(path)
  } else {
    list_df <- path
  }
  if (!is.list(list_df)) {
    return(ErrorClass$new("Could not import data"))
  }
  for (i in seq_along(list_df)) {
    if (!is.data.frame(list_df[[i]])) {
      return(list_df[[i]])
    }
  }

  promises_list <- vector("list", length(list_df))

  communicator_list <- lapply(seq_along(list_df), function(x) {
    Communicator$new()
  })

  plan(multisession)
  for (i in seq_along(list_df)) {
    if (!is.null(seed)) seed <- as.numeric(Sys.time())
    promises_list[[i]] <- future({
      opti(
        case = case, lowerBounds = lowerBounds, upperBounds = upperBounds,
        list_df[[i]], additionalParameters,
        seed = seed, npop = npop, ngen = ngen,
        Topology = Topology,
        errorThreshold = errorThreshold, runAsShiny = communicator_list[[i]]
      )
    })
  }

  while (TRUE) {
    current_state <- lapply(communicator_list, function(x) {
      x$getData()
    })
    current_state <- do.call(rbind, current_state)

    runAsShiny$setData(current_state)
    all_resolved <- all(sapply(promises_list, resolved))
    if (all_resolved) break
    # Sys.sleep(2)
  }
  temp_list <- list()
  for (i in seq_along(promises_list)) {
    temp_list[[i]] <- value(promises_list[[i]])
  }

  lapply(communicator_list, function(x) {
    x$destroy()
  })

  list <- seperate_batch_results(temp_list)
  list(list, plotStates(list), plotParams(list), plotMetrices(list))
}
