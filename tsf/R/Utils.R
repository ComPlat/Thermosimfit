# additional parameters utilities
correct_names_additional_param <- function(df, case) {
  if (case == "dba_host_const") {
    names(df) <- c("Host [M]")
  } else if (case == "dba_dye_const") {
    names(df) <- c("Dye [M]")
  } else if (case == "ida") {
    names(df) <- c("Host [M]", "Dye [M]", "Ka(HD) [1/M]")
  } else if (case == "gda") {
    names(df) <- c("Host [M]", "Guest [M]", "Ka(HD) [1/M]")
  }
  return(df)
}

# Parameter utilities
correct_names_params <- function(df, case) {
  if (case == "dba_host_const") {
    names(df) <- c("Ka(HD) [1/M]", "I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  } else if (case == "dba_dye_const") {
    names(df) <- c("Ka(HD) [1/M]", "I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  } else if (case == "ida") {
    names(df) <- c("Ka(HG) [1/M]", "I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  } else if (case == "gda") {
    names(df) <- c("Ka(HG) [1/M]", "I(0)", "I(HD) [1/M]", "I(D) [1/M]")
  }
  return(df)
}

create_params_df <- function(res, case) {
  df <- data.frame(
    khd = res[[2]][1], I0 = res[[2]][2],
    IHD = res[[2]][3], ID = res[[2]][4]
  ) |> correct_names_params(case)
  return(df)
}

# data utilities
correct_names_data <- function(df, case) {
  if (case == "dba_host_const") {
    names(df) <- c(
      "total Dye measured [M]", "Signal measured",
      "Signal simulated",
      "free Dye simulated [M]", "Host-Dye simulated [M]"
    )
  } else if (case == "dba_dye_const") {
    names(df) <- c(
      "total Host measured [M]", "Signal measured",
      "Signal simulated",
      "free Dye simulated [M]", "Host-Dye simulated [M]"
    )
  } else if (case == "ida") {
    names(df) <- c(
      "total Guest measured [M]", "Signal measured",
      "Signal simulated",
      "free Dye simulated [M]", "Host-Dye simulated [M]"
    )
  } else if (case == "gda") {
    names(df) <- c(
      "total Dye measured [M]", "Signal measured",
      "Signal simulated",
      "free Dye simulated [M]", "Host-Dye simulated [M]"
    )
  }
  return(df)
}
create_data_df <- function(df, res, case) {
  df$signal_insilico <- res[[1]][, 1]
  df$d <- res[[1]][, 2]
  df$hd <- res[[1]][, 3]
  return(correct_names_data(df, case))
}

# plotting utilities
add_axis_labels <- function(p, case, ylabel) {
  case_df <- data.frame(
    dba_host_const = "total Dye measured [M]",
    dba_dye_const = "total Host measured [M]",
    ida = "total Guest measured [M]",
    gda = "total Dye measured [M]"
  )
  x_col <- case_df[case] |> as.character()
  p + xlab(x_col) + ylab(ylabel)
}

plot_results <- function(df, case) {
  case_df <- data.frame(
    dba_host_const = "total Dye measured [M]",
    dba_dye_const = "total Host measured [M]",
    ida = "total Guest measured [M]",
    gda = "total Dye measured [M]"
  )
  x_col <- case_df[case] |> as.character()
  df_com <- data.frame(
    x = rep(df[, x_col], 2),
    y = c(df[, "Signal measured"], df[, "Signal simulated"]),
    group = c(
      rep("Measured", length(df[, x_col])),
      rep("Predicted", length(df[, x_col]))
    )
  )
  df_d <- data.frame(
    x = df[, x_col],
    y = df[, "free Dye simulated [M]"]
  )
  df_hd <- data.frame(
    x = df[, x_col],
    y = df[, "Host-Dye simulated [M]"]
  )
  base_size <- 10

  p1 <- ggplot() +
    geom_point(data = df_com[df_com$group == "Measured", ],
      aes(
        x = x,
        y = y,
        colour = "Measured"
      ), size = 2, alpha = 0.5
    ) +
    geom_smooth(data = df_com[df_com$group == "Measured", ],
      aes(
        x = x,
        y = y,
        colour = "Measured"
      ),
      colour = "grey", formula = "y ~ x",
      size = 1,
      se = FALSE, method = "loess"
    ) +
    geom_point(data = df_com[df_com$group == "Predicted", ],
      aes(
        x = x,
        y = y,
        colour = "Predicted"
      ), size = 2, alpha = 0.5
    ) +
    scale_colour_manual(
      values = c(
        "Measured" = "grey",
        "Predicted" = RColorBrewer::brewer.pal(8, "Dark2")[1]
      )
    ) +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size),
      legend.text = element_text(size = base_size),
      legend.title = element_text(size = base_size),
      strip.text.x = element_text(size = base_size)
    ) +
    guides(colour = guide_legend(title = ""))
  p1 <- add_axis_labels(p1, case, "Signal [a.u]")

  p2 <- ggplot(
    data = df_d,
    aes(x = x, y = y)
  ) +
    geom_point() +
    theme(
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size * 0.8),
      strip.text.x = element_text(size = base_size)
    )
  p2 <- add_axis_labels(p2, case, "Dye [M]")

  p3 <- ggplot(
    data = df_hd,
    aes(x = x, y = y)
  ) +
    geom_point() +
    theme(
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size * 0.8),
      strip.text.x = element_text(size = base_size)
    )
  p3 <- add_axis_labels(p3, case, "Host-Dye [M]")
  p1 + p2 + p3
}

plot_results_plotly <- function(df, case) {
  case_df <- data.frame(
    dba_host_const = "total Dye measured [M]",
    dba_dye_const = "total Host measured [M]",
    ida = "total Guest measured [M]",
    gda = "total Dye measured [M]"
  )
  x_col <- case_df[case] |> as.character()
  df_com <- data.frame(
    x = rep(df[, x_col], 2),
    y = c(df[, "Signal measured"], df[, "Signal simulated"]),
    group = c(
      rep("Measured", length(df[, x_col])),
      rep("Predicted", length(df[, x_col]))
    )
  )
  df_d <- data.frame(
    x = df[, x_col],
    y = df[, "free Dye simulated [M]"]
  )
  df_hd <- data.frame(
    x = df[, x_col],
    y = df[, "Host-Dye simulated [M]"]
  )
  base_size <- 10

  colors <- c(
    "Measured" = "grey",
    "Predicted" = RColorBrewer::brewer.pal(8, "Dark2")[1]
  )

  p1 <- plot_ly() %>%
    # Measured points
    add_trace(
      data = df_com[df_com$group == "Measured", ],
      x = ~x, y = ~y,
      type = "scatter",
      mode = "markers",
      marker = list(color = colors["Measured"], size = 10, opacity = 0.5),
      name = "Measured"
    ) %>%
    # Measured smoothed line
    add_trace(
      data = df_com[df_com$group == "Measured", ],
      x = ~x, y = ~y,
      type = "scatter",
      mode = "lines",
      line = list(color = "grey", width = 2),
      name = "Measured Loess",
      showlegend = FALSE
    ) %>%
    # Predicted points
    add_trace(
      data = df_com[df_com$group == "Predicted", ],
      x = ~x, y = ~y,
      type = "scatter",
      mode = "markers",
      marker = list(color = colors["Predicted"], size = 10, opacity = 0.5),
      name = "Predicted"
    )

  p2 <- plot_ly() %>%
    add_trace(
      data = df_d,
      x = ~x, y = ~y,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE
    )

  p3 <- plot_ly() %>%
    add_trace(
      data = df_hd,
      x = ~x, y = ~y,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE
    )

  case_df <- data.frame(
    dba_host_const = "total Dye measured [M]",
    dba_dye_const = "total Host measured [M]",
    ida = "total Guest measured [M]",
    gda = "total Dye measured [M]"
  )
  x_col <- case_df[case] |> as.character()

  subplot(
    p1, p2, p3,
    nrows = 2,
    margin = 0.05,
    shareX = FALSE,
    shareY = FALSE
  ) %>%
    layout(
      xaxis = list(title = x_col),
      xaxis2 = list(title = x_col),
      xaxis3 = list(title = x_col),
      yaxis = list(title = "Signal [a.u]"),
      yaxis2 = list(title = "Dye [M]"),
      yaxis3 = list(title = "Host-Dye [M]")
    )
}
