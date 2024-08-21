l <- readRDS("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/Batch/res_batch.rds")

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
  base_size <- 14
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
    axis.title = element_text(size = base_size * 1.2, face = "bold"),
    axis.text = element_text(size = base_size, face = "bold"),
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    strip.text.x = element_text(size = base_size * 1.2, face = "bold"),
    strip.text.y = element_text(size = base_size * 1.2, face = "bold")
  )
  return(p)
}
library(ggplot2)
plotMetrices(l, 2)
