dotSize <- function(download = FALSE) {
  if (download) {
    return(1)
  } else {
    return(2)
  }
}
lineSize <- function(download = FALSE) {
  if(download){
    return(0.5)}
  return(1)
}

addTheme <- function(p, base_size = 12) {
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

plotSignal <- function(df, Dataset, download = FALSE) {
  df_signal <- data.frame(
    x = rep(df[, 1], 2),
    y = c(df[, 2], df[, 3]),
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
      ), size = dotSize(download)
    ) +
    geom_smooth(data = df_signal[df_signal$group == "Signal measured", ],
      aes(
        x = x,
        y = y,
        linetype = ""
      ),
      size = lineSize(download),
      method = "loess",
      formula = 'y ~ x',
      se = FALSE,
      colour = "grey"
    ) +
    geom_point(data = df_signal[df_signal$group == "Signal measured", ],
      aes(
      x = x,
      y = y
      ), size = dotSize(download),
      colour = "grey"
    ) +
    ylab("Signal [a.u]") +
    xlab(names(df)[1]) +
    ggtitle(paste0("Dataset Nr.", Dataset)) +
    scale_colour_brewer(name = "", palette = "Dark2") +
    guides(colour = guide_legend(title = "Repetition")) +
    guides(linetype = guide_legend(title = "Measured"))
}

plotFreeDye <- function(df, download) {
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
      ), size = dotSize(download)
    ) +
    ylab("Dye [M]") +
    xlab(names(df)[1]) +
    scale_colour_brewer(name = "", palette = "Dark2") +
    guides(colour = guide_legend(title = "Repetition"))
}

plotHostDye <- function(df, download) {
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
      ), size = dotSize(download)
    ) +
    ylab("Host-Dye [M]") +
    xlab(names(df)[1]) +
    scale_colour_brewer(name = "", palette = "Dark2") +
    guides(colour = guide_legend(title = "Repetition"))
}

combinePlots <- function(p1, p2, p3, keep_legend = FALSE, base_size = 12) {
  p1 <- addTheme(p1, base_size)
  p2 <- addTheme(p2, base_size)
  p3 <- addTheme(p3, base_size)
  if (keep_legend) {
    p2 <- p2 + theme(legend.position = "none")
    p3 <- p3 + theme(legend.position = "none")
    p <- cowplot::plot_grid(p1, p2, p3, nrow = 3)
    p <- p + cowplot::panel_border(colour = "grey", size = 2)
    return(p)
  } else {
    p1 <- p1 + theme(legend.position = "none")
    p2 <- p2 + theme(legend.position = "none")
    p3 <- p3 + theme(legend.position = "none")
    p <- cowplot::plot_grid(p1, p2, p3, nrow = 3)
    p <- p + cowplot::panel_border(colour = "grey", size = 2)
    return(p)
  }
}

combineList <- function(plot_list, ncols = 4) {
  cowplot::plot_grid(plotlist = c(plot_list, legend), ncol = ncols)
}

plotStates <- function(list, num_rep = 1, base_size = 12, download = FALSE) {
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
    p1 <- plotSignal(temp_df, x, download)
    p2 <- plotFreeDye(temp_df, download)
    p3 <- plotHostDye(temp_df, download)
    if (x == 1) {
      return(combinePlots(p1, p2, p3, keep_legend = TRUE, base_size))
    } else {
      return(combinePlots(p1, p2, p3, keep_legend = FALSE, base_size))
    }
  })
  return(combineList(plot_list))
}



plotParams <- function(list, num_rep = 1, base_size = 12) {
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
  if (num_rep > 1) {
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
        panel.spacing = unit(2, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  p <- addTheme(p)
  p <- p + theme(
    plot.background = element_rect(color = "grey", fill = NA, size = 2)
  )
  return(p)
}

plotMetrices <- function(list, num_rep = 1, base_size = 12) {
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
  if (num_rep > 1) {
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
        panel.spacing = unit(2, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  p <- addTheme(p)
  p <- p + theme(
    plot.background = element_rect(color = "grey", fill = NA, size = 2)
  )
  return(p)
}
