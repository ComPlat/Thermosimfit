library(ggplot2)
library(cowplot)

empty_plot <- ggplot() +
  theme_void()
size_dashes <- 0.25

param_plot <- function(path) {
  load(path)
  res[[3]] + scale_fill_brewer(palette = "Dark2")
}

plot_fct <- function(path) {
  load(path)
  states <- res[[1]]$states
  states <- Reduce(rbind, states)

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

p_dba <- plot_fct("dba.RData")
p_ida <- plot_fct("ida.RData")
p_gda <- plot_fct("gda.RData")
p_dba <- p_dba + theme(legend.position = "none")
p_ida <- p_ida + theme(legend.position = "none")
p1 <- plot_grid(p_dba, p_ida, p_gda,
  nrow = 1,
  labels = c("a", "b", "c")
)

p_dba <- param_plot("dba.RData") + theme(legend.position = "none")
p_ida <- param_plot("ida.RData") + theme(legend.position = "none")
p_gda <- param_plot("gda.RData")

p2 <- plot_grid(p_dba, p_ida, p_gda,
  nrow = 1,
  labels = c("d", "e", "f")
)

p <- plot_grid(p1, p2, nrow = 2)

ggsave(p,
  bg = "white",
  file = "variance.png",
  width = 15,
  height = 8
)
