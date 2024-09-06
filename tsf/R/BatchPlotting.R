dotSize <- function() {
  return(0.5)
}

lineSize <- function() {
  return(0.5)
}

baseSize <- function() {
  return(8)
}

addTheme <- function(p, base_size = 6) {
  p <- p + theme(
    title = element_text(size = base_size, face = "bold"),
    axis.title = element_text(size = base_size, face = "bold"),
    axis.text = element_text(size = base_size),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5),
    strip.text.x = element_text(size = base_size, face = "bold"),
    strip.text.y = element_text(size = base_size, face = "bold")
  )
  return(p)
}

plotSignal <- function(df, Dataset) {
  df_signal <- data.frame(
    x = rep(df[, 1], 2),
    y = c(df[, 2], df[, 3]),
    group = c(
      rep("Signal measured", nrow(df)),
      rep("Signal simulated", nrow(df))
    ),
    repetitions = paste0("Run Nr. ", df$repetition)
  )
  ggplot() +
    geom_point(data = df_signal[df_signal$group != "Signal measured", ],
      aes(
        x = x,
        y = y,
        colour = factor(repetitions)
      ), size = dotSize()
    ) +
    geom_smooth(data = df_signal[df_signal$group == "Signal measured", ],
      aes(
        x = x,
        y = y,
        colour = group
      ),
      linewidth = lineSize(),
      method = "loess",
      formula = 'y ~ x',
      se = FALSE
    ) +
    geom_point(data = df_signal[df_signal$group == "Signal measured", ],
      aes(
        x = x,
        y = y,
        colour = group
      ),
      size = dotSize()
    ) +
    ylab("Signal [a.u]") +
    xlab(names(df)[1]) +
    scale_colour_brewer(name = "", palette = "Dark2")
}

plotFreeDye <- function(df) {
  df_dye <- data.frame(
    x = df[, 1],
    y = df[, 4],
    repetitions = df$repetition
  )
  ggplot() +
    geom_point(data = df_dye,
      aes(
        x = x,
        y = y,
        colour = factor(repetitions)
      ), size = dotSize()
    ) +
    ylab("Dye [M]") +
    xlab(names(df)[1]) +
    scale_colour_brewer(name = "", palette = "Dark2") +
    guides(colour = guide_legend(title = "Repetition"))
}

plotHostDye <- function(df) {
  df_host_dye <- data.frame(
    x = df[, 1],
    y = df[, 5],
    repetitions = df$repetition
  )
  ggplot() +
    geom_point(data = df_host_dye,
      aes(
        x = x,
        y = y,
        colour = factor(repetitions)
      ), size = dotSize()
    ) +
    ylab("Host-Dye [M]") +
    xlab(names(df)[1]) +
    scale_colour_brewer(name = "", palette = "Dark2") +
    guides(colour = guide_legend(title = "Repetition"))
}

combinePlots <- function(p1, p2, p3, index, base_size = 6) {
  p1 <- addTheme(p1, base_size)
  legend <- cowplot::get_legend(p1 + theme(
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    legend.key.size = unit(0.25, "cm")
  ))
  p1 <- addTheme(p1, base_size)
  p2 <- addTheme(p2, base_size)
  p3 <- addTheme(p3, base_size)
  p1 <- p1 + theme(legend.position = "none")
  p2 <- p2 + theme(legend.position = "none")
  p3 <- p3 + theme(legend.position = "none")
  p <- cowplot::plot_grid(p1, p2, p3, nrow = 1)
  p <- cowplot::plot_grid(p, legend, nrow = 1, rel_widths = c(0.8, 0.2))
  p <- cowplot::ggdraw() +
    cowplot::draw_plot(p) +
    cowplot::draw_label(
      paste0("Dataset Nr.", index),
      x = 0.5, y = 1.0, hjust = -1.5, vjust = 1.2,
      size = 12)
  return(p)
}

plotStates <- function(list) {
  base_size <- baseSize()
  list <- list[[1]]
  df <- Reduce(rbind, list)
  groups <- unique(df$dataset)
  plot_list <- lapply(groups, function(x) {
    temp_df <- df[df$dataset == x, ]
    p1 <- plotSignal(temp_df, x)
    p2 <- plotFreeDye(temp_df)
    p3 <- plotHostDye(temp_df)
    return(combinePlots(p1, p2, p3, parent.frame()$i[], base_size))
  })
  return(plot_list)
}

plotParams <- function(list) {
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
  p <- ggplot() +
    geom_boxplot(
      data = data,
      aes(
        y = y, fill = "Entire data", x = factor(0)
      )
    ) +
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
      panel.spacing = unit(2, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    guides(fill = guide_legend(title = "Datasets"))
  p <- addTheme(p)
  p <- p + theme(
    plot.background = element_rect(color = "grey", fill = NA, size = 2)
  )
  return(p)
}

plotMetrices <- function(list) {
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
  p <- ggplot() +
    geom_boxplot(
      data = data,
      aes(
        y = y, fill = "Entire data", x = factor(0)
      )
    ) +
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
      panel.spacing = unit(2, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    guides(fill = guide_legend(title = "Datasets"))
  p <- addTheme(p)
  p <- p + theme(
    plot.background = element_rect(color = "grey", fill = NA, size = 2)
  )
  return(p)
}
