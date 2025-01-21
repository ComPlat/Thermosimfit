library(ggplot2)
library(cowplot)
dotsize <- 0.5
boxplot_size <- 0.5
outlier_size <- 0.5
strip <- element_text(size = 7, face = "bold")
axis <- element_text(size = 6)
axis_title <- element_text(size = 8)
legend_text <- element_text(size = 8)

calc_errors <- function(df) {
  df <- split(df, df$seed)
  lapply(df, function(x) {
    signal <- x[, 2]
    signalInsilico <- x[, 3]
    return(sum(abs(signal - signalInsilico) / signal))
  }) |> unlist()
}

sig_plot <- function(case, path, legend = FALSE) {
  load(path)
  seeds <- lapply(result, function(x) x$seed)
  df <- lapply(seq_len(length(result)), function(idx) {
    res <- result[[idx]][[1]]
    res$seed <- seeds[[idx]]
    return(res)
  })
  df <- Reduce(rbind, df)

  errors <- calc_errors(df)
  errors_df <- data.frame(errors = errors)
  p_errors <- ggplot() +
    geom_boxplot(
      data = errors_df,
      aes(x = "", y = errors),
      outlier.size = outlier_size
    ) +
    labs(x = "", y = "MANE") +
    theme(
      axis.text = axis,
      axis.title = axis_title
    ) +
    coord_flip()

  df_forward_sim <- lapply(seq_len(length(result)), function(idx) {
    res <- result[[idx]][[1]]
    params <- result[[idx]][[2]]
    ap <- result[[idx]]$additionalParameters
    res <- tsf:::forward_simulation(
      case = case,
      df = res,
      additionalParameters = ap,
      parameter = params,
      n = 100
    )
    res$Signal <- res$Signal + params[, 2] # TODO: add I0 not here but in forward_simulation
    res$seed <- seeds[[idx]]
    return(res)
  })
  df_forward_sim <- Reduce(rbind, df_forward_sim)

  p_signal <- ggplot() +
    geom_smooth(
      data = df_forward_sim,
      aes(
        x = df_forward_sim[, 1],
        y = `Signal`,
        group = `seed`,
        colour = "forward sim."
      ),
      linewidth = dotsize * 0.4
    ) +
    geom_point(
      data = df,
      aes(
        x = df[, 1],
        y = `Signal measured`,
        colour = "measured"
      ),
      size = dotsize
    ) +
    labs(x = names(df)[1]) +
    scale_colour_manual(values = c("grey", "#76acb0", "darkred")) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    theme(
      legend.title = element_blank(),
      axis.text = axis,
      axis.title = axis_title,
      legend.text = legend_text,
      legend.position = "bottom",
      legend.key.size = unit(0.6, "cm"),
      legend.key = element_rect(fill = "white")
    ) +
    guides(
      colour = guide_legend(override.aes = list(fill = NA))
    )

  p_signal <- plot_grid(
    p_signal, p_errors,
    nrow = 2,
    rel_heights = c(1, 0.275)
  )

  if (!legend) {
    p_signal <- p_signal + theme(legend.position = "none")
  }
  return(p_signal)
}

param_plot <- function(path) {
  load(path)
  seeds <- lapply(result, function(x) x$seed)
  parameter <- lapply(seq_len(length(result)), function(idx) {
    res <- result[[idx]][[2]]
    res$seed <- seeds[[idx]]
    return(res)
  })
  parameter <- Reduce(rbind, parameter)
  parameter <- data.frame(
    seeds = rep(parameter$seed, 4),
    names = stack(parameter[, 1:4])[, 2],
    values = stack(parameter[, 1:4])[, 1]
  )
  p_parameter <- ggplot(
    data = parameter,
    aes(
      x = "",
      y = values
    )
  ) +
    geom_boxplot(
      size = boxplot_size,
      outlier.size = outlier_size
    ) +
    geom_point(size = dotsize) +
    labs(x = NULL) +
    # labs(y = "Values [1/M] or a.u.") +
    labs(y = "") +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    facet_wrap(. ~ names, scales = "free") +
    theme(
      axis.text = axis,
      axis.title = axis_title,
      legend.text = legend_text,
      strip.text = strip
    )
}

empty_plot <- ggplot() +
  theme_void()
size_dashes <- 0.25

p_ida <- sig_plot("ida", "IDA_10_different_seeds.RData")
p_gda <- sig_plot("gda", "GDA_10_different_seeds.RData")
p_dba <- sig_plot("dba_dye_const", "DBA_10_different_seeds.RData", legend = TRUE)
legend <- ggpubr::get_legend(p_dba)
p_dba <- p_dba + theme(legend.position = "none")
p <- plot_grid(
  p_dba, empty_plot, p_ida, empty_plot, p_gda,
  ncol = 5,
  labels = c("a", "", "b", "", "c"),
  rel_widths = c(1, 0.05, 1, 0.05, 1)
)
p_signal <- plot_grid(p, legend, ncol = 1, rel_heights = c(1, 0.1))

p_signal <- ggdraw(p_signal) +
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

p_ida <- param_plot("IDA_10_different_seeds.RData")
p_gda <- param_plot("GDA_10_different_seeds.RData")
p_dba <- param_plot("DBA_10_different_seeds.RData")
p_param <- plot_grid(
  p_dba, empty_plot, p_ida, empty_plot, p_gda,
  ncol = 5,
  rel_widths = c(1, 0.05, 1, 0.05, 1)
)
p_param <- ggdraw(p_param) +
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
    size = 0.25
  )

p <- plot_grid(p_signal, p_param, nrow = 2)
ggsave("FigNr1.pdf", p,
  width = 8, height = 8 * 2 / 3
)
ggsave("FigNr1.png", p,
  width = 8, height = 8 * 2 / 3,
  bg = "white", dpi = 800
)
