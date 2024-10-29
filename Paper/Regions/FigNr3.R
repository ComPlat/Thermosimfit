library(ggplot2)
library(cowplot)
load("ContourPlots.RData")

p1 <- plot_grid(
  p_dba_cp$plot, p_ida_cp$plot,
  labels = c("a", "b"),
  nrow = 2
)
p2 <- plot_grid(p_gda_cp$plot, labels = "c")

ggsave(p1,
  width = 10,
  height = 10,
  dpi = 600,
  bg = "white",
  file = "FigNr3Part1.png"
)
ggsave(p2,
  width = 7.5,
  height = 7.5,
  dpi = 600,
  bg = "white",
  file = "FigNr3Part2.png"
)
