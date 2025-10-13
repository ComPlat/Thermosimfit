dotSize <- function() {
  return(0.5)
}

lineSize <- function() {
  return(0.5)
}

baseSize <- function() {
  return(8)
}

addTheme <- function(p, base_size = 6) {
  p <- p + theme(
    title = element_text(size = base_size, face = "bold"),
    axis.title = element_text(size = base_size, face = "bold"),
    axis.text = element_text(size = base_size),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5),
    strip.text.x = element_text(size = base_size, face = "bold"),
    strip.text.y = element_text(size = base_size, face = "bold")
  )
  return(p)
}

plotStatesBatch <- function(list, case) {
  case_df <- data.frame(
    dba_host_const = "total Dye measured [M]",
    dba_dye_const = "total Host measured [M]",
    ida = "total Guest measured [M]",
    gda = "total Dye measured [M]"
  )
  x_col <- case_df[case] |> as.character()
  base_size <- baseSize()

  list <- list[["states"]]
  df <- Reduce(rbind, list)
  nsigs <- (ncol(df) - 5L) / 2L
  dataset_col <- ncol(df)
  repetion_col <- ncol(df) - 1L
  lapply(unique(df$dataset), function(i) {
    sub <- df[df$dataset == i, ]
    lapply(2:(nsigs + 1), function(s) {
      subsub <- sub[, c(1, s, s + nsigs, repetion_col, dataset_col)]
      ym_col <- names(subsub)[2]
      yis_col <- names(subsub)[3]
      p <- ggplot(data = subsub) +
        geom_boxplot(data = subsub,
          aes(x = .data[[x_col]], y = .data[[yis_col]], group = .data[[x_col]], fill = "Signal in silico")) +
        geom_point(data = subsub,
          aes(x = .data[[x_col]], .data[[ym_col]], colour = "Signal measured")) +
        scale_colour_manual(values = c("Signal measured" = "black"), name = NULL) +
        theme(
          legend.position = "bottom",
          axis.title = element_text(size = base_size * 1.2),
          axis.text = element_text(size = base_size),
          legend.text = element_text(size = base_size),
          legend.title = element_text(size = base_size),
          strip.text.x = element_text(size = base_size)
        ) +
        guides(colour = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +
        labs(title = sprintf("Dataset Nr.%s, Sig. Nr.%s", i, s - 1))
      add_axis_labels(p, case, "Signal [a.u]")
    })
  })
}

plotIParamsBatch <- function(list, num_rep = 1) {
  base_size <- baseSize()
  list <- list[["params"]]
  df <- Reduce(rbind, list)
  nsigs <- (ncol(df) - 3L) / 3L
  i_inds <- 2:(1 + nsigs*3)
  lapply(unique(df$dataset), function(i) {
    sub <- df[df$dataset == i, ]
    sub <- sub[, i_inds]
    inds <- split(seq_len(ncol(sub)), ceiling(seq_along(sub) / 3))
    subs <- lapply(inds, function(i) sub[, i, drop = FALSE])
    lapply(subs, function(s) {
      sig <- names(s)[[1]]
      sig <- gsub(".*(Nr\\.?[0-9]+).*", "\\1", sig)
      title <- paste0("Dataset Nr.", i, " Signal ", sig)
      names(s) <- c("I(0)", "I(HD) [1/M]", "I(D) [1/M]")
      s <- stack(s)
      ggplot(data = s) +
        geom_boxplot(aes(y = values, group = 1L)) +
        labs(title = title, x = NULL, y = NULL) +
        facet_wrap(~ ind, scales = "free") +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    })
  })
}

plotKaBatch <- function(list, num_rep = 1) {
  base_size <- baseSize()
  list <- list[["params"]]
  df <- Reduce(rbind, list)
  df <- df[, c(1, ncol(df) - 1, ncol(df))]
  x_col <- names(df)[1]
  ps <- list()
  ps[[1]] <- ggplot() +
    geom_boxplot(data = df, aes(y = .data[[x_col]]))
  ps[[2]] <- lapply(unique(df$dataset), function(i) {
    sub <- df[df$dataset == i, ]
    ggplot() +
      geom_boxplot(data = sub, aes(y = .data[[x_col]])) +
      labs(title = sprintf("Dataset Nr.%s", i))
  })
  ps
}

plotMetricesBatch <- function(list, num_rep = 1) {
  base_size <- baseSize()
  list <- list[["metrices"]]
  df <- Reduce(rbind, list)
  nsigs <- length(unique(df[["Signal"]]))
  lapply(unique(df$dataset), function(d) {
    sub <- df[df$dataset == d, ]
    lapply(seq_len(nsigs), function(s) {
      subsub <- sub[sub$Signal == s,]
      title <- paste0("Sig. Nr.", s)
      subsub <- stack(subsub[, 1:4])
      ggplot(data = subsub, aes(y = values)) +
        geom_boxplot() +
        facet_wrap(~ ind, scales = "free") +
        labs(title = title)
    })
  })
}

plotDAndHDBatch <- function(list, num_rep = 1) {
  base_size <- baseSize()
  list <- list[["states"]]
  df <- Reduce(rbind, list)
  df <- df[, c(1, rev(ncol(df):(ncol(df) - 3)))]
  x_col <- names(df)[1]
  lapply(unique(df$dataset), function(i) {
    sub <- df[df$dataset == i, ]
    pdye <- ggplot() +
      geom_boxplot(data = sub, aes(x = .data[[x_col]],
        y = .data[["free Dye simulated [M]"]],
        group = .data[[x_col]])) +
      labs(title = sprintf("Dataset Nr.%s", i))
    phostdye <- ggplot() +
      geom_boxplot(data = sub, aes(x = .data[[x_col]],
        y = .data[["Host-Dye simulated [M]"]],
        group = .data[[x_col]])) +
      labs(title = sprintf("Dataset Nr.%s", i))
    pdye + phostdye
  })
}
