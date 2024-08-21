library(tsf)
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/Batch/")

res_batch <- tsf:::batch(
  case = "ida",
  lowerBounds = c(kG = 1000, I0 = 0, IHD = 0, ID = 0),
  upperBounds = c(kG = 10^8, I0 = 100, IHD = 10^7, ID = 10^7),
  path = "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/idaBatch.csv",
  additionalParameters = c(host = 1.00E-06, dye = 1.00E-06, kHD = 3.00E+06),
  ngen = 20, num_rep = 2
)

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
  # dye and host dye plot
  data <- data.frame(
    x = rep(df[, 1], 2),
    y = c(df[, 4], df[, 5]),
    names = c(
      rep(names(df)[4], nrow(df)),
      rep(names(df)[5], nrow(df))
    ),
    repetition = rep(df$repetition, 2),
    dataset = rep(df$dataset, 2)
  )
  base_size <- 14
  if (num_rep > 1) {
    xlabels <- as.character(unique(rbind(df[, 1], "")))
    xlabels[(seq_along(xlabels) %% 10) != 1] <- ""
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = interaction(dataset, x), y = y,
          fill = factor(dataset),
          group = interaction(dataset, x)
        ),
        width = 0.25,
        size = 0.25
      ) +
      facet_wrap(. ~ names,
        strip.position = "left",
        scales = "free_y"
      ) +
      xlab(names(df)[1]) +
      ylab(NULL) +
      guides(fill = guide_legend(title = "Datasets")) +
      scale_x_discrete(
        labels = xlabels
      )
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
        strip.position = "left",
        scales = "free_y"
      ) +
      ylab(NULL) +
      xlab(names(df)[1])
  }
  p <- p + theme(
    legend.position = "bottom",
    axis.title = element_text(size = base_size * 1.2, face = "bold"),
    axis.text = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    strip.text.x = element_text(size = base_size * 1.2, face = "bold"),
    strip.text.y = element_text(size = base_size * 1.2, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
  # signal plot
  data_signal_measured <- data.frame(
    x = c(df[, 1], df[, 1]),
    y = c(df[, 2], df[, 3]),
    names = c(
      rep(names(df)[2], nrow(df)),
      rep(names(df)[3], nrow(df))
    ),
    repetition = rep(df$repetition, 2),
    dataset = rep(df$dataset, 2)
  )

  xlevels <- levels(factor(data_signal_measured$x))
  xlabels <- xlevels
  xlabels[(seq_along(xlevels) %% 3) != 1] <- ""
  p_signal <- ggplot(
    data = data_signal_measured,
    aes(
      x = factor(x),
      y = y,
      colour = factor(dataset),
      group = interaction(x, dataset, repetition)
    )
  ) +
    geom_point(
      data = subset(data_signal_measured, names != "Signal measured"),
      aes(shape = factor(repetition))
    ) +
    geom_smooth(
      data = subset(data_signal_measured, names == "Signal measured"),
      aes(x = factor(x), y = y, group = 1, colour = "Measured")
    ) +
    scale_x_discrete(labels = xlabels) +
    xlab(names(df)[1]) +
    ylab(NULL) +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = base_size * 1.2, face = "bold"),
      axis.text = element_text(size = base_size),
      legend.text = element_text(size = base_size),
      legend.title = element_text(size = base_size),
      strip.text.x = element_text(size = base_size, face = "bold"),
      strip.text.y = element_text(size = base_size * 1.2, face = "bold"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    guides(
      shape = guide_legend(title = "Repetitions"),
      colour = guide_legend(title = "Datasets")
    )
  return(list(p_signal, p))
}

library(ggplot2)

plotStates(res_batch[[1]], 2)[[1]]
plotStates(res_batch[[1]], 2)[[2]]
