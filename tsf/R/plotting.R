plotTSF <- function(df, xCol) {
  df_com <- data.frame(
    x = rep(df[, xCol], 2),
    y = c(df[, "signal"], df[, "signal_insilico"]),
    group = c(
      rep("measured", length(df[, xCol])),
      rep("predicted", length(df[, xCol]))
    )
  )
  df_d <- data.frame(
    x = df[, xCol],
    y = df[, "d"]
  )
  df_hd <- data.frame(
    x = df[, xCol],
    y = df[, "hd"]
  )
  base_size <- 10
  p1 <- ggplot(df_com, aes(x = x, y = y, colour = group)) +
    geom_point() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size),
      legend.text = element_text(size = base_size),
      legend.title = element_text(size = base_size),
      strip.text.x = element_text(size = base_size)
    ) +
    labs(y = "Signal", x = paste(xCol, "[M]"))
  p2 <- ggplot(data = df_d, aes(x = x, y = y)) +
    geom_point() +
    labs(x = paste(xCol, "[M]"), y = "dye [M]") +
    theme(
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size * 0.8),
      strip.text.x = element_text(size = base_size)
    )

  p3 <- ggplot(data = df_hd, aes(x = x, y = y)) +
    geom_point() +
    labs(x = paste(xCol, "[M]"), y = "host-dye [M]") +
    theme(
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size * 0.8),
      strip.text.x = element_text(size = base_size)
    )
  p1 / (p2 + p3)
}
