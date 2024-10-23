library(magick)
library(cowplot)
library(ggplot2)

p_dba <- image_read_svg("DBA.svg") |> image_ggplot()
p_ida <- image_read_svg("IDA.svg") |> image_ggplot()
p_gda <- image_read_svg("GDA.svg") |> image_ggplot()

p <- plot_grid(
  p_dba, p_ida, p_gda,
  ncols = 3,
  labels = c("a", "b", "c")
)

# library(patchwork)
# p <- p_dba + p_ida + p_gda

ggsave(p,
  file = "entire_plot.svg",
  bg = "white",
  width = 15, height = 5, dpi = 900
)
