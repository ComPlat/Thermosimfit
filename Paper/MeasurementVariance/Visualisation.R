library(ggplot2)
library(cowplot)

empty_plot <- ggplot() +
  theme_void()
size_dashes <- 0.25

extract_best_runs <- function(res) {
  states <- res$states
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  states <- Reduce(rbind, states)
  params <- Reduce(rbind, params)
  states <- lapply(unique(errors$dataset), function(x) {
    states_subset <- states[states$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50,]
    states_subset[states_subset$repetition %in% errors_subset$repetition, ]
  })
  states <- Reduce(rbind, states)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50,]
    params_subset[params_subset$repetition %in% errors_subset$repetition, ]
  })
  params <- Reduce(rbind, params)
  return(list(states = states, params = params))
}

param_plot <- function(path) {
  load(path)
  params <- extract_best_runs(res[[1]])$params
  params <- data.frame(
    y = c(params[, 1], params[, 2], params[, 3], params[, 4]),
    names = rep(names(params)[1:4], each = nrow(params)),
    dataset = rep(params[, "dataset"], 4)
  )
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
    theme(
      panel.spacing = unit(2, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    guides(fill = guide_legend(title = "Datasets")) +
    scale_fill_brewer(palette = "Dark2")
}

plot_fct <- function(path) {
  load(path)
  states <- extract_best_runs(res[[1]])$states
  df <- data.frame(
    var = rep(states[, 1], 2),
    signal = c(states[, 2], states[, 3]),
    dataset = rep(states[, "dataset"], 2),
    repetition = rep(states[, "repetition"], 2),
    group = c(rep(
      "Measured",
      nrow(states)
    ), rep(
      "Simulated",
      nrow(states)
    ))
  )

  ggplot() +
    geom_boxplot(
      data = df,
      aes(
        x = var,
        y = signal,
        group = interaction(var, group),
        fill = interaction(group)
      ),
      outlier.size = 0.25
    ) +
    scale_fill_brewer(palette = "Dark2") +
    labs(
      fill = "",
      y = "Signal",
      x = names(states)[1]
    )
}

p_dba <- plot_fct("dba_100Runs.RData")
p_ida <- plot_fct("ida_100.RData")
p_gda <- plot_fct("gda_100.RData")
p_dba <- p_dba + theme(legend.position = "none")
p_ida <- p_ida + theme(legend.position = "none")
p1 <- plot_grid(p_dba, p_ida, p_gda,
  nrow = 1,
  labels = c("a", "b", "c")
)

p_dba <- param_plot("dba_100Runs.RData") + theme(legend.position = "none")
p_ida <- param_plot("ida_100.RData") + theme(legend.position = "none")
p_gda <- param_plot("gda_100.RData")

p2 <- plot_grid(p_dba, p_ida, p_gda,
  nrow = 1,
  labels = c("d", "e", "f")
)

p <- plot_grid(p1, p2, nrow = 2)

ggsave(p,
  bg = "white",
  file = "variance_50reps.png",
  width = 15,
  height = 8
)
