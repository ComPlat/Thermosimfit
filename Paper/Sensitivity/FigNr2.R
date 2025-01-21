library(ggplot2)
library(cowplot)
load("Sensitivity.RData")
load("ErrorsVsImportantParams.RData")
dotsize <- 0.5
boxplot_size <- 0.5
outlier_size <- 0.5
axis <- element_text(size = 8)
axis_title <- element_text(size = 12)
legend_text <- element_text(size = 8)
theme <- theme(
  axis.text = axis,
  axis.title = axis_title,
  legend.text = legend_text
)

# Add correct labels to sensitivity plots
add_labels <- function(pl, names) {
  xBreaks <- layer_scales(pl)$x$break_positions()
  pl + theme(
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) +
    scale_x_continuous(
      breaks = xBreaks,
      labels = names
    ) +
    ylab("Explained fraction of variance")
}

p_dba <- add_labels(p_dba, c(
  "Ka(HD)", "I(0)", "I(HD)", "I(D)",
  "Ka(HD)*I(0)", "Ka(HD)*I(HD)", "Ka(HD)*I(D)",
  "I(0)*I(HD)", "I(0)*I(D)", "I(HD)*I(D)"
))
p_ida <- add_labels(p_ida, c(
  "Ka(HG)", "I(0)", "I(HD)", "I(D)",
  "Ka(HG)*I(0)", "Ka(HG)*I(HD)", "Ka(HG)*I(D)",
  "I(0)*I(HD)", "I(0)*I(D)", "I(HD)*I(D)"
))
p_gda <- add_labels(p_gda, c(
  "Ka(HG)", "I(0)", "I(HD)", "I(D)",
  "Ka(HG)*I(0)", "Ka(HG)*I(HD)", "Ka(HG)*I(D)",
  "I(0)*I(HD)", "I(0)*I(D)", "I(HD)*I(D)"
))

p_ida <- p_ida + theme
p_dba <- p_dba + theme
p_gda <- p_gda + theme

ggsave(p_ida,
  bg = "white",
  width = 5,
  height = 6,
  dpi = 900,
  file = "FigNr2.png"
)

empty_plot <- ggplot() +
  theme_void()
size_dashes <- 0.25

sensi_plots <- plot_grid(
  p_dba, empty_plot, p_gda,
  labels = c("a", "", "b"),
  ncol = 3,
  rel_widths = c(1, 0.05, 1)
)

ggsave(sensi_plots,
  bg = "white",
  width = 10,
  height = 6,
  dpi = 900,
  file = "SuplFigNr1.png"
)
