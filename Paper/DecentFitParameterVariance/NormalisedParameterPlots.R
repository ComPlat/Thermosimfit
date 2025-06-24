read_data <- function(path) {
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

  parameter <- tapply(
    parameter, parameter$names, function(x) {
      x$values <- x$values / mean(x$values)
      x
    }
  )
  parameter <- Reduce(rbind, parameter)
  # NOTE: otherwise I0 remains as factor level, which is annoying
  parameter$names <- parameter$names |> as.character()
  parameter[parameter$names != "I(0)", ]
}

df <- read_data("IDA_10_different_seeds.RData")
library(ggplot2)
ggplot(data = df, aes(x = names, y = values)) +
  geom_boxplot() +
  labs(y = "Normalised parameter values", x = "") +
  theme(
    axis.text = element_text(size = 24),
    axis.title.y = element_text(size = 26)
  )
