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

batch <- function(case,
                  lowerBounds, upperBounds,
                  path,
                  additionalParameters,
                  seed = NULL, npop = 40, ngen = 200, Topology = "random",
                  errorThreshold = -Inf, num_rep = 1) {
  # import data
  list_df <- importDataBatch(path)
  if (!is.list(list_df)) {
    return(ErrorClass$new("Could not import data"))
  }
  for (i in seq_along(list_df)) {
    if (!is.data.frame(list_df[[i]])) {
      return(list_df[[i]])
    }
  }
  # seed case
  seed_original <- seed
  seed_from <- 1:1e6
  # size calculations
  num_data_sets <- length(list_df)
  result <- vector("list", length = num_data_sets * num_rep)
  seeds <- numeric(length = num_data_sets * num_rep)
  counter <- 1

  # run optimization
  for (i in seq_len(num_data_sets)) {
    if (is.null(seed)) {
      seed <- sample(seed_from, 1)
    } else {
      seed <- seed_original
    }
    # seeds[counter] <- seed
    result[[counter]] <- opti(
      case = case, lowerBounds = lowerBounds, upperBounds = upperBounds,
      list_df[[i]], additionalParameters, seed = seed, npop = npop, ngen = ngen,
      Topology = Topology, errorThreshold = errorThreshold
    )
    counter <- counter + 1
    if (num_rep > 1) {
      for (j in seq_len(num_rep - 1)) {
        seed <- sample(seed_from, 1)
        # seeds[counter] <- seed
        result[[counter]] <- opti(
          case = case, lowerBounds = lowerBounds, upperBounds = upperBounds,
          list_df[[i]], additionalParameters, seed = seed, npop = npop, ngen = ngen,
          Topology = Topology, errorThreshold = errorThreshold
        )
        counter <- counter + 1
      }
    }
  }

  list <- seperate_batch_results(result)
  list(
    list,
    plotStates(list, num_rep),
    plotParams(list, num_rep),
    plotMetrices(list, num_rep)
  )
}

call_several_opti <- function(case, lb, ub,
                                    df_list, ap, seed_list,
                                    npop, ngen, topo,
                                    et, messages) {
  res <- vector("list", length(df_list))
  for (i in seq_len(length(df_list))) {
    df <- df_list[[i]]
    seed <- seed_list[[i]]
    m <- messages[[i]]
    result <- tsf::opti(
      case, lb, ub, df, ap, seed, npop, ngen,
      topo, et, m
    )
    res[[i]] <- result
  }
  return(res)
}

call_several_opti_in_bg <- function(case, lb, ub, df_list, ap,
                                    seed_list, npop, ngen, topo,
                                    et, messages) {

  callr::r_bg(
    function(case, lb, ub, df_list, ap,
             seed_list, npop, ngen, Topology, errorThreshold, messages) {
      res <- tsf:::call_several_opti(
        case, lb, ub, df_list, ap, seed_list, npop, ngen,
        Topology, errorThreshold, messages
      )
      return(res)
    },
    args = list(
      case, lb, ub, df_list,
      ap, seed_list, npop, ngen, topo, et, messages
    )
  )
}
