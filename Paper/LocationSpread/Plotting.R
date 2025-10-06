# Plotting
# ========================================
plot_raw_data <- function(data, bins) {
  ggplot() +
    geom_histogram(
      data = data.frame(x = data),
      binwidth = bins[2] - bins[1],
      aes(x = x, y = ..density..)
    ) +
    geom_rug(
      data = data.frame(x = data),
      aes(x = data)
    ) +
    labs(x = names(data), y = "Density")
}
add_labels <- function(p, fd) {
  df <- data.frame(
    x = max(fd$df$x),
    y = mean(fd$df$y),
    label = fd$params
  )
  p + geom_label(
    data = df,
    aes(
      x = x,
      y = y,
      label = label
    ), size = 6)
}
add_density <- function(p, df) {
  p + geom_line(
    data = df,
    aes(
      x = x, y = y,
      linetype = linetype
    )
  )
}
add_joint_kernel_density <- function(p, density_data) {
  p + stat_summary_bin(
    data = density_data, aes(x = x, y = y, linetype = "Joint kernel density"),
    bins = 120,
    fun.min = function(z) 0,
    fun.max = max,
    geom = "linerange",
    linewidth = 2.0,
    lineend = "round"
)
}
add_metrices <- function(p, location_error) {
  colors <- RColorBrewer::brewer.pal(6, "Dark2")
  p +
    geom_errorbarh(
      data = location_error,
      aes(xmin = xmin, xmax = xmax, y = y, color = type),
      height = 0, size = 0.8
    ) +
    geom_point(
      data = location_error,
      aes(x = x, y = y, color = type),
      size = 1
    ) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Mean" = colors[1],
        "Median" = colors[2],
        "Mode (KD)" = colors[3],
        "Mode (JDK)" = colors[4],
        "Mode (JDK boot.)" = colors[5],
        "Median IQR" = colors[6]
      )
    ) +
    scale_linetype_manual(
      name = NULL,
      values = setNames(
        c("solid", "dashed", "dotted"),
        c("Fitted distribution", "Kernel density", "Joint kernel density")
      )
    ) +
    scale_shape_manual(
      name = NULL,
      values = c("Joint kernel density" = 10)
    ) +
    theme(
      legend.position = "right",
      legend.box      = "horizontal",
      legend.direction = "horizontal",
      legend.text  = element_text(size = 28),
      axis.text.x = element_text(size = 28),
      axis.text.y = element_text(size = 28),
      axis.title.x = element_text(size = 28),
      axis.title.y = element_text(size = 28)
    ) +
    guides(
      linetype = guide_legend(order = 1, keywidth = 2, keyheight = 1, nrow = 2),
      color    = guide_legend(order = 2, keywidth = 2, keyheight = 1, nrow = 2)
    )
}
