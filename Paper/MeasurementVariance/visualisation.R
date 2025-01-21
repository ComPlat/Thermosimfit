library(ggplot2)
library(cowplot)

dotsize <- 0.5
boxplot_size <- 0.5
outlier_size <- 0.5
strip <- element_text(size = 7, face = "bold")
axis <- element_text(size = 6)
axis_title <- element_text(size = 8)
legend_text <- element_text(size = 8)

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
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50, ]
    states_subset[states_subset$repetition %in% errors_subset$repetition, ]
  })
  states <- Reduce(rbind, states)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50, ]
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
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    labs(
      fill = "",
      y = "Signal",
      x = names(states)[1]
    )
}

# Paper plot
p_ida_signal <- plot_fct("ida_100.RData")
p_ida_params <- param_plot("ida_100.RData") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1.15, 0.8)
  )
p_ida_params <- plot_grid(
  p_ida_params, empty_plot,
  nrow = 1,
  rel_widths = c(1, 0.2)
)

p <- plot_grid(
  p_ida_signal, p_ida_params,
  nrow = 2,
  rel_widths = c(1, 0.3),
  labels = c("a", "b")
)
p
ggsave(p,
  bg = "white",
  file = "ida_variance_50reps.png",
  width = 8,
  height = 8
)

# Supplementary plot
p_dba <- plot_fct("dba_100Runs.RData")
p_gda <- plot_fct("gda_100.RData")
p_dba <- p_dba + theme(legend.position = "none")
p_gda <- p_gda + theme(
  legend.position = "inside",
  legend.position.inside = c(1.15, 0.8)
)
p1 <- plot_grid(p_dba, p_gda, empty_plot,
  nrow = 1,
  rel_widths = c(1, 1, 0.3),
  labels = c("a", "b")
)
p1

p_dba <- param_plot("dba_100Runs.RData") + theme(legend.position = "none")
p_gda <- param_plot("gda_100.RData")
p_gda <- p_gda + theme(
  legend.position = "inside",
  legend.position.inside = c(1.17, 0.8)
)
p2 <- plot_grid(p_dba, p_gda, empty_plot,
  nrow = 1,
  rel_widths = c(1, 1, 0.3),
  labels = c("c", "d")
)
p2

p <- plot_grid(p1, p2, nrow = 2)

ggsave(p,
  bg = "white",
  file = "variance_50reps.png",
  width = 15,
  height = 8
)
