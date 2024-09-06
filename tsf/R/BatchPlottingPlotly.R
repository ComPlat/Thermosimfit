
dotSizePlotly <- function() {
    return(4)
}

lineSizePlotly <- function() {
    return(1)
}

plotSignalPlotly <- function(df, Dataset) {
  df_signal <- data.frame(
    x = rep(df[, 1], 2),
    y = c(df[, 2], df[, 3]),
    group = c(
      rep("Signal measured", nrow(df)),
      rep("Signal simulated", nrow(df))
    ),
    repetitions = df$repetition
  )
  smoothed <- spline(df[, 1], df[, 2], n = 100)
  smoothed_data <- data.frame(
    x = smoothed$x,
    y = smoothed$y
  )
  p <- plot_ly(data = df_signal, x = ~x, y = ~y, height = 4000)
  reps <- unique(df$repetition)
  legend_group <- paste0("Signal", 1:length(reps))
  sig_sim <- df_signal[df_signal$group != "Signal measured", ]
  for (i in seq_along(reps)) {
    temp <- sig_sim[sig_sim$repetitions == reps[i], ]
    p <- p %>% add_trace(
      data = temp,
      x = ~x, y = ~y,
      type = "scatter",
      mode = "markers",
      marker = list(size = dotSizePlotly()),
      color = ~factor(repetitions),
      name = ~paste0("signal simulated Rep.", factor(repetitions)),
      legendgroup = legend_group[i]
    ) %>% layout(margin = 0.015)
  }
  p <- p %>%
    add_trace(
      data = smoothed_data,
      x = ~x, y = ~y,
      type = "scatter",
      mode = "lines",
      line = list(color = "grey", width = lineSizePlotly()),
      name = "interpolated measured points",
      legendgroup = "MeasuredInterpolated"
    ) %>%
    add_trace(
      data = df_signal[df_signal$group == "Signal measured", ],
      x = ~x, y = ~y,
      type = "scatter",
      mode = "markers",
      marker = list(size = dotSizePlotly(), color = "grey"),
      name = "Signal measured",
      legendgroup = "Measured"
    ) %>% layout(margin = 0.015)
  return(p)
}

plotFreeDyePlotly <- function(df, Dataset) {
  df_dye <- data.frame(
    x = df[, 1],
    y = df[, 4],
    repetitions = df$repetition
  )
  p <- plot_ly()
  reps <- unique(df$repetition)
  legend_group <- paste0("Dye", 1:length(reps))
  for (i in seq_along(reps)) {
    temp <- df_dye[df_dye$repetitions == reps[i], ]
      p <- p %>% add_trace(
        data = temp,
        x = ~x,
        y = ~y,
        color = ~factor(repetitions),
        type = "scatter",
        mode = "markers",
        marker = list(size = dotSizePlotly()),
        name = ~paste0("Dye Rep.", factor(repetitions)),
        legendgroup = legend_group[i]
      ) %>% layout(margin = 0.015)
  }
  return(p)
}

plotHostDyePlotly <- function(df, Dataset) {
  df_host_dye <- data.frame(
    x = df[, 1],
    y = df[, 5],
    repetitions = df$repetition
  )
  p <- plot_ly()
  reps <- unique(df$repetition)
  legend_group <- paste0("Host-Dye", 1:length(reps))
  for (i in seq_along(reps)) {
    temp <- df_host_dye[df_host_dye$repetitions == reps[i], ]
    p <- p %>% add_trace(
      data = temp,
      x = ~x,
      y = ~y,
      color = ~factor(repetitions),
      type = "scatter",
      marker = list(size = dotSizePlotly()),
      mode = "markers",
      name = ~paste0("Host-Dye Rep.", factor(repetitions)),
      legendgroup = legend_group[i]
    ) %>% layout(margin = 0.015)
  }
  return(p)
}

plotStatesPlotly <- function(list) {
  list <- list[[1]]
  df <- Reduce(rbind, list)
  groups <- unique(df$dataset)
  plot_list <- lapply(groups, function(x) {
    temp_df <- df[df$dataset == x, ]
    p1 <- plotSignalPlotly(temp_df, x) %>%
      layout(
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = "Signal [a.u]"),
        margin = 0.015
      )
    p2 <- plotFreeDyePlotly(temp_df, x) %>%
      layout(
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = "Dye [M]"),
        margin = 0.015
      )
    p3 <- plotHostDyePlotly(temp_df, x) %>%
      layout(
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = "Host-Dye [M]"),
        margin = 0.015
      )
    if (x != 1) {
      p1 <- p1 %>% style(showlegend = FALSE)
      p2 <- p2 %>% style(showlegend = FALSE)
      p3 <- p3 %>% style(showlegend = FALSE)
    }
    list(p1, p2, p3)
  })
  # signal plot
  signal_list <- lapply(plot_list, function(x) x[[1]])
  signal_p <- subplot(signal_list, nrows = length(plot_list),
    shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
  # dye plot
  dye_list <- lapply(plot_list, function(x) x[[2]])
  dye_p <- subplot(dye_list, nrows = length(plot_list),
    shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)

  annotations <- lapply(1:length(plot_list), function(i) {
    list(
      x = 0.5,
      y = 1 - ((i - 1) / length(plot_list)),
      text = paste0("Dataset Nr.", i),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(size = 14)
    )
  })

  dye_p <- dye_p %>%
    layout(annotations = annotations)

  # host-dye plot
  host_dye_list <- lapply(plot_list, function(x) x[[3]])
  host_dye_p <- subplot(host_dye_list, nrows = length(plot_list),
    shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)

  p <- subplot(signal_p, dye_p, host_dye_p,
    shareX = FALSE, shareY = FALSE,
    margin = 0.07,
    titleX = TRUE, titleY = TRUE, which_layout = 0) %>% # TODO: find alternative this results in a warning
  return(p)
}

plotParamsPlotly <- function(list) {
  list <- list[[2]]
  df <- Reduce(rbind, list)
  data <- data.frame(
    x = rep(df$dataset, 4),
    y = c(df[, 1], df[, 2], df[, 3], df[, 4]),
    names = c(
      rep(names(df)[1], nrow(df)),
      rep(names(df)[2], nrow(df)),
      rep(names(df)[3], nrow(df)),
      rep(names(df)[4], nrow(df))
    ),
    repetition = rep(df$repetition, 4)
  )
  plot_list <- list()
  names <- unique(data$names)
  for (i in seq_along(names)) {
    temp <- data[data$names == names[i], ]
    plot_list[[i]] <- plot_ly() %>%
      add_trace(
        data = temp,
        y = ~y,
        x = ~factor("Entire data"),
        type = "box",
        name = paste0("Entire data: ", names[i])
      ) %>%
      add_trace(
      data = temp,
      y = ~y,
      x = ~factor(x),
      type = "box",
      name = names[i]
    ) %>%
      layout(
        yaxis = list(title = names[i]),
        xaxis = list(title = "Datasets")
      )
  }
  p <- subplot(plot_list[[1]], plot_list[[2]],
               plot_list[[3]], plot_list[[4]],
    nrows = 4,
    titleX = TRUE, titleY = TRUE, shareX = TRUE, shareY = FALSE,
      margin = 0.015)
  return(p)
}

plotMetricesPlotly <- function(list) {
  list <- list[[3]]
  df <- Reduce(rbind, list)
  data <- data.frame(
    x = rep(df$dataset, 5),
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
  plot_list <- list()
  names <- unique(data$names)
  for (i in seq_along(names)) {
    temp <- data[data$names == names[i], ]
    plot_list[[i]] <- plot_ly() %>%
      add_trace(
        data = temp,
        y = ~y,
        x = ~factor("Entire data"),
        type = "box",
        name = paste0("Entire data: ", names[i])
      ) %>%
      add_trace(
        data = temp,
        y = ~y,
        x = ~factor(x),
        type = "box",
        name = names[i]
      ) %>%
      layout(
        yaxis = list(title = names[i]),
        xaxis = list(title = "Datasets")
      )
  }
  p <- subplot(plot_list[[1]], plot_list[[2]],
    plot_list[[3]], plot_list[[4]], plot_list[[5]],
    nrows = 5,
    titleX = TRUE, titleY = TRUE, shareX = TRUE, shareY = FALSE,
  margin = 0.015)
  return(p)

}

entirePlotPlotly <- function(list) {
  states <- plotStatesPlotly(list)
  params <- plotParamsPlotly(list)
  metrices <- plotMetricesPlotly(list)
  subplot(states, params, metrices, nrows = 1,
          shareX = FALSE, titleX = TRUE, titleY = TRUE,
          widths = c(0.7, 0.15, 0.15),
          margin = 0.02)
}
