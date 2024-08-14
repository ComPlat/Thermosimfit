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

plotStates <- function(list, num_rep = 1) {
  list <- list[[1]]
  num_data_sets <- length(list) / num_rep
  repetitions <- (seq_len(length(list)) - 1) %% num_rep + 1
  data_sets <- rep(1:num_data_sets, each = num_rep)
  for (i in seq_along(list)) {
    list[[i]]$dataset <- data_sets[i]
    list[[i]]$repetition <- repetitions[i]
  }
  df <- Reduce(rbind, list)
  data <- data.frame(
    x = rep(df[, 1], 3),
    y = c(df[, 3], df[, 4], df[, 5]),
    names = c(
      rep(names(df)[3], nrow(df)),
      rep(names(df)[4], nrow(df)),
      rep(names(df)[5], nrow(df))
    ),
    repetition = rep(df$repetition, 3),
    dataset = rep(df$dataset, 3)
  )
  data_signal_measured <- data.frame(
    x = df[, 1],
    y = df[, 2],
    dataset = df$dataset
  )
  p_signal <- ggplot(
    data = data_signal_measured,
    aes(x = x, y = y, fill = dataset)
  ) +
    geom_line() +
    xlab(names(df)[1]) +
    ylab("Signal measured")

  base_size <- 10
  if (num_rep > 1) {
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = x, y = y,
          fill = factor(dataset),
          group = interaction(x, dataset)
        ),
        width = 0.25,
        size = 0.25
      ) +
      facet_wrap(. ~ names,
        scales = "free_y"
      ) +
      guides(fill = guide_legend(title = "Datasets"))
  } else {
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = x,
          y = y,
          group = factor(x)
        )
      ) +
      facet_wrap(~names,
        scales = "free_y"
      )
  }
  p <- p + theme(
    legend.position = "bottom",
    axis.title = element_text(size = base_size * 1.2),
    axis.text = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    strip.text.x = element_text(size = base_size)
  )
  return(p / p_signal)
}

plotParams <- function(list, num_rep = 1) {
  list <- list[[2]]
  num_data_sets <- length(list) / num_rep
  repetitions <- (seq_len(length(list)) - 1) %% num_rep + 1
  data_sets <- rep(1:num_data_sets, each = num_rep)
  for (i in seq_along(list)) {
    list[[i]]$dataset <- data_sets[i]
    list[[i]]$repetition <- repetitions[i]
  }
  df <- Reduce(rbind, list)
  data <- data.frame(
    x = rep(df[, 5], 4),
    y = c(df[, 1], df[, 2], df[, 3], df[, 4]),
    names = c(
      rep(names(df)[1], nrow(df)),
      rep(names(df)[2], nrow(df)),
      rep(names(df)[3], nrow(df)),
      rep(names(df)[4], nrow(df))
    ),
    repetition = rep(df$repetition, 4)
  )
  base_size <- 10
  if (num_rep > 1) {
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = factor(x), y = y,
          group = factor(x),
          fill = factor(x)
        )
      ) +
      facet_wrap(. ~ names,
        scales = "free_y",
        strip.position = "left"
      ) +
      xlab(NULL) +
      ylab(NULL) +
      theme(
        strip.background = element_blank(),
        strip.placement = "outside"
      ) +
      guides(fill = guide_legend(title = "Datasets"))
  } else {
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = factor(x),
          y = y,
          group = names
        )
      ) +
      facet_wrap(~names,
        scales = "free_y",
        strip.position = "left"
      ) +
      xlab(NULL) +
      ylab(NULL) +
      theme(
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  p <- p + theme(
    legend.position = "bottom",
    axis.title = element_text(size = base_size * 1.2),
    axis.text = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    strip.text.x = element_text(size = base_size)
  )
  return(p)
}

plotMetrices <- function(list, num_rep = 1) {
  list <- list[[3]]
  num_data_sets <- length(list) / num_rep
  repetitions <- (seq_len(length(list)) - 1) %% num_rep + 1
  data_sets <- rep(1:num_data_sets, each = num_rep)
  for (i in seq_along(list)) {
    list[[i]]$dataset <- data_sets[i]
    list[[i]]$repetition <- repetitions[i]
  }
  df <- Reduce(rbind, list)
  data <- data.frame(
    x = rep(df[, 6], 5),
    y = c(df[, 1], df[, 2], df[, 3], df[, 4], df[, 5]),
    names = c(
      rep(names(df)[1], nrow(df)),
      rep(names(df)[2], nrow(df)),
      rep(names(df)[3], nrow(df)),
      rep(names(df)[4], nrow(df)),
      rep(names(df)[5], nrow(df))
    ),
    repetition = rep(df$repetition, 5)
  )
  base_size <- 10
  if (num_rep > 1) {
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = factor(x), y = y,
          group = factor(x),
          fill = factor(x)
        )
      ) +
      facet_wrap(. ~ names,
        scales = "free_y",
        strip.position = "left"
      ) +
      xlab(NULL) +
      ylab(NULL) +
      theme(
        strip.background = element_blank(),
        strip.placement = "outside"
      ) +
      guides(fill = guide_legend(title = "Datasets"))
  } else {
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = factor(x),
          y = y,
          group = names
        )
      ) +
      facet_wrap(~names,
        scales = "free_y",
        strip.position = "left"
      ) +
      xlab(NULL) +
      ylab(NULL) +
      theme(
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  p <- p + theme(
    legend.position = "bottom",
    axis.title = element_text(size = base_size * 1.2),
    axis.text = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    strip.text.x = element_text(size = base_size)
  )
  return(p)
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
      Topology = Topology, errorThreshold = errorThreshold, runAsShiny = FALSE
    )
    counter <- counter + 1
    if (num_rep > 1) {
      for (j in seq_len(num_rep - 1)) {
        seed <- sample(seed_from, 1)
        # seeds[counter] <- seed
        result[[counter]] <- opti(
          case = case, lowerBounds = lowerBounds, upperBounds = upperBounds,
          list_df[[i]], additionalParameters, seed = seed, npop = npop, ngen = ngen,
          Topology = Topology, errorThreshold = errorThreshold, runAsShiny = FALSE
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
