
dotSizePlotly <- function() {
    return(10)
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
  p <- plot_ly(data = df_signal, x = ~x, y = ~y)
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
      name = ~paste0("Repetition signal simulated", factor(repetitions)),
      legendgroup = legend_group[i]
    )
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
    )
  return(p)
}

plotFreeDyePlotly <- function(df, Dataset) {
  df_dye <- data.frame(
    x = df[, 1],
    y = df[, 4],
    repetitions = df$repetition
  )
  p <- plot_ly(width = 600, height = 600)
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
        name = ~paste0("Repetition (Dye)", factor(repetitions)),
        legendgroup = legend_group[i]
      )
  }
  return(p)
}

plotHostDyePlotly <- function(df, Dataset) {
  df_host_dye <- data.frame(
    x = df[, 1],
    y = df[, 5],
    repetitions = df$repetition
  )
  p <- plot_ly(width = 600, height = 600)
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
      mode = "markers",
      name = ~paste0("Repetition (Host-Dye)", factor(repetitions)),
      legendgroup = legend_group[i]
    )
  }
  return(p)
}

plotStatesPlotly <- function(list, num_rep = 1, ncols = 4) {
  list <- list[[1]]
  num_data_sets <- length(list) / num_rep
  repetitions <- (seq_len(length(list)) - 1) %% num_rep + 1
  data_sets <- rep(1:num_data_sets, each = num_rep)
  for (i in seq_along(list)) {
    list[[i]]$dataset <- data_sets[i]
    list[[i]]$repetition <- repetitions[i]
  }
  df <- Reduce(rbind, list)
  groups <- unique(df$dataset)
  plot_list <- lapply(groups, function(x) {
    temp_df <- df[df$dataset == x, ]
    p1 <- plotSignalPlotly(temp_df, x) %>%
      layout(
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = "Signal [a.u]")
      )
    p2 <- plotFreeDyePlotly(temp_df, x) %>%
      layout(
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = "Dye [M]")
      )
    p3 <- plotHostDyePlotly(temp_df, x) %>%
      layout(
        xaxis = list(title = names(df)[1]),
        yaxis = list(title = "Host-Dye [M]")
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
    titleX = TRUE, titleY = TRUE, which_layout = 0) %>% #TODO: find alternative this results in a warning
    layout(
      margin = list(l = 80, r = 40, b = 40, t = 10),
      width = 1200, height = 1000 # TODO: this is deprecated but the correct way to do it in plot_ly does not work
    )
  return(p)
}
