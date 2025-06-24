library(ggplot2)

extract_best_runs <- function(res) {
  states <- res$states
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  params <- Reduce(rbind, params)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50, ]
    params_subset[params_subset$repetition %in% errors_subset$repetition, ]
  })
  params <- Reduce(rbind, params)
  lapply(1:3, function(x) {
    print(dim(params[params$dataset == x, ]))
  })
  # params <- params[params[, 4] > 1, ] # Discarding small ID values as a check 
  lapply(1:3, function(x) {
    print(dim(params[params$dataset == x, ]))
  })
  params
}

load_data <- function(path) {
  load(path)
  params <- extract_best_runs(res[[1]])
  data.frame(
    y = c(params[, 1], params[, 2], params[, 3], params[, 4]),
    names = rep(names(params)[1:4], each = nrow(params)),
    dataset = rep(params[, "dataset"], 4)
  )
}
data <- load_data("ida_100.RData")


param_plot <- function(params) {
  ggplot() +
    geom_boxplot(
      data = params,
      aes(
        y = y, fill = "Entire data", x = factor(0)
      )
    ) +
    geom_boxplot(
      data = params,
      aes(
        x = factor(dataset), y = y,
        group = factor(dataset),
        fill = factor(dataset)
      )
    ) +
    facet_wrap(. ~ names,
      scales = "free_y",
      strip.position = "left"
    ) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    theme(
      panel.spacing = unit(2, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.text.x = element_blank()
    ) +
    guides(fill = guide_legend(title = "Datasets")) +
    scale_fill_brewer(palette = "Dark2")
}

p_ida_params <- param_plot(data)
p_ida_params 
