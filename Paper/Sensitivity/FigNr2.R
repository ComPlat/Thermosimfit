library(ggplot2)
library(cowplot)
load("Sensitivity.RData")
load("ErrorsVsImportantParams.RData")

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

empty_plot <- ggplot() +
  theme_void()
size_dashes <- 0.25

sensi_plots <- plot_grid(
  p_dba, empty_plot, p_ida, empty_plot, p_gda,
  labels = c("a", "", "b", "", "c"),
  ncol = 5,
  rel_widths = c(1, 0.05, 1, 0.05, 1)
)
sensi_plots <- ggdraw(sensi_plots) +
  draw_line(
    x = c(0.33, 0.33), y = c(0, 1),
    color = "black",
    linetype = "dashed",
    size = size_dashes
  ) +
  draw_line(
    x = c(0.66, 0.66),
    y = c(0, 1),
    color = "black",
    linetype = "dashed",
    size = size_dashes
  )

ip_plots <- plot_grid(
  p_dba_ip, empty_plot, p_ida_ip, empty_plot, p_gda_ip,
  labels = c("d", "", "e", "", "f"),
  ncol = 5,
  rel_widths = c(1, 0.05, 1, 0.05, 1)
)

ip_plots <- ggdraw(ip_plots) +
  draw_line(
    x = c(0.33, 0.33), y = c(0, 1),
    color = "black",
    linetype = "dashed",
    size = size_dashes
  ) +
  draw_line(
    x = c(0.66, 0.66),
    y = c(0, 1),
    color = "black",
    linetype = "dashed",
    size = size_dashes
  )

p <- plot_grid(
  sensi_plots,
  ip_plots,
  nrow = 2
)

ggsave(p,
  bg = "white",
  width = 20,
  height = 10,
  dpi = 900,
  file = "FigNr2.png"
)
