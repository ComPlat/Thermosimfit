# Quantity	Value
# conc(H)	1.00E-06
# conc(D)	1.00E-06
# conc(G)	1/200000
# Kequ(HD)	3.00E+06
# Kequ(HG)	2.00E+07
# Signal-0	0
# Signal-HD	1.00E+06
# Signal-Dye	2.00E+05
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA")
library(tsf)
df <- read.csv("forKonrad-conc-vs-signal.csv",
  sep = ";",
  dec = ".",
  header = FALSE
)

res <- tsf:::batch(
  case = "ida",
  lowerBounds = c(
    kG = 1000,
    I0 = 0,
    IHD = 0,
    ID = 0
  ),
  upperBounds = c(
    kG = 10^8,
    I0 = 100, # started at 10^7 but it ended always at 0...
    IHD = 10^7,
    ID = 10^7
  ),
  "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/idaBatch.csv",
  additionalParameters = c(
    host = 1.00E-06,
    dye = 1.00E-06,
    kHD = 3.00E+06
  ),
  npop = 40,
  ngen = 20,
  Topology = "random",
  errorThreshold = 0.7,
  num_rep = 2
)

plotStates <- function(list, num_rep = 1) {
  list <- list[[1]]
  num_data_sets <- length(list) / num_rep
  repetitions <- (seq_len(length(list)) - 1) %% num_rep + 1
  data_sets <- rep(1:num_data_sets, each = num_rep)
  for (i in seq_along(list)) {
    list[[i]]$dataset <- data_sets[i]
    list[[i]]$repetition <- repetitions[i]
  }
  df <- Reduce(rbind, list)
  data <- data.frame(
    x = rep(df[, 1], 3),
    y = c(df[, 3], df[, 4], df[, 5]),
    names = c(
      rep(names(df)[3], nrow(df)),
      rep(names(df)[4], nrow(df)),
      rep(names(df)[5], nrow(df))
    ),
    repetition = rep(df$repetition, 3),
    dataset = rep(df$dataset, 3)
  )
  data_signal_measured <- data.frame(
    x = df[, 1],
    y = df[, 2],
    dataset = df$dataset
  )
  p_signal <- ggplot(
    data = data_signal_measured,
    aes(x = x, y = y, fill = dataset)
  ) +
    geom_line() +
    xlab(names(df)[1]) +
    ylab("Signal measured")

  base_size <- 10
  if (num_rep > 1) {
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = x, y = y,
          fill = factor(dataset),
          group = interaction(x, dataset)
        ),
        width = 0.25,
        size = 0.25
      ) +
      facet_wrap(. ~ names,
        scales = "free_y"
      ) +
      guides(fill = guide_legend(title = "Datasets"))
  } else {
    p <- ggplot() +
      geom_boxplot(
        data = data,
        aes(
          x = x,
          y = y,
          group = factor(x)
        )
      ) +
      facet_wrap(~names,
        scales = "free_y"
      )
  }
  p <- p + theme(
    legend.position = "bottom",
    axis.title = element_text(size = base_size * 1.2),
    axis.text = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    legend.title = element_text(size = base_size),
    strip.text.x = element_text(size = base_size)
  )
  return(p / p_signal)
}

library(ggplot2)
plotStates(res[[1]], 2)
