dotSize <- function() 2
lineSize <- function() 1

addTheme <- function(p) {
  base_size <- 12
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
    legend.key.size = unit(0.05, "cm"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
  return(p)
}

plotSignal <- function(df, Dataset) {
  df_signal <- data.frame(
    x = rep(df[, 1], 2),
    y = c(df[, 2],df[, 3]),
    group = c(
      rep("Signal measured", nrow(df)),
      rep("Signal simulated", nrow(df))
    ),
    repetitions =df$repetition
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
        linetype = ""
      ),
      size = lineSize(),
      method = "loess",
      formula = 'y ~ x',
      se = FALSE,
      colour = "grey"
    ) +
    geom_point(data = df_signal[df_signal$group == "Signal measured", ],
      aes(
      x = x,
      y = y
      ), size = dotSize(),
      colour = "grey"
    ) +
    ylab("Signal [a.u]") +
    xlab(names(df)[1]) +
    ggtitle(paste0("Dataset Nr.", Dataset)) +
    scale_colour_brewer(name = "", palette = "Dark2") +
    guides(colour = guide_legend(title = "Repetition")) +
    guides(linetype = guide_legend(title = "Measured"))
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

combinePlots <- function(p1, p2, p3, keep_legend = FALSE) {
  p1 <- addTheme(p1)
  p2 <- addTheme(p2)
  p3 <- addTheme(p3)
  if (keep_legend) {
    p2 <- p2 + theme(legend.position = "none")
    p3 <- p3 + theme(legend.position = "none")
    p <- cowplot::plot_grid(p1, p2, p3, nrow = 3)
    p <- p + cowplot::panel_border(colour = "grey")
    return(p)
  } else {

    p1 <- p1 + theme(legend.position = "none")
    p2 <- p2 + theme(legend.position = "none")
    p3 <- p3 + theme(legend.position = "none")
    p <- cowplot::plot_grid(p1, p2, p3, nrow = 3)
    p <- p + cowplot::panel_border(colour = "grey")
    return(p)
  }
}

combineList <- function(plot_list) {
  cowplot::plot_grid(plotlist = c(plot_list, legend), ncol = 4)
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
  groups <- unique(df$dataset)
  plot_list <- lapply(groups, function(x) {
    temp_df <- df[df$dataset == x, ]
    p1 <- plotSignal(temp_df, x)
    p2 <- plotFreeDye(temp_df)
    p3 <- plotHostDye(temp_df)
    if (x == 1) {
      return(combinePlots(p1, p2, p3, keep_legend = TRUE))
    } else {
      return(combinePlots(p1, p2, p3))
    }
  })
  return(combineList(plot_list))
}
