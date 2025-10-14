load("./Paper/GlobalAnalysis/10Runs.RData")
names(res[[1]])

errors <- vapply(seq_len(length(res)), \(i) {
  sum(res[[i]][["metrices"]]$MeanSquareError)
}, double(1))
boxplot(c(runif(10), 100), plot = FALSE)

# Signal plots
# ====================================================================
states <- lapply(seq_len(length(res)), \(i) {
  elem <- res[[i]]
  temp <- elem[["data"]]
  temp <- temp[, 1:13] # Remove free dye and host-dye
  temp$run <- i
  temp
})
states <- Reduce(rbind, states)

wls <- c(470, 486, 512, 524, 532, 550)
names(states)[2:7] <- paste("Measured", wls)
names(states)[8:13] <- paste("Simulated", wls)

states <- data.frame(
  stack(states[, 2:13]),
  run = rep(states$run, 12),
  "Guest" = rep(states[[1]], 12)
)
states$group <- gsub("[0-9]| ", "", states$ind)
states$signal_wavelengths <- as.numeric(gsub("[A-z]| ", "", states$ind))
states$signal_names <- sprintf("Signal at %s nm", states$signal_wavelengths)

library(ggplot2)

p <- ggplot(data = states) +
  geom_boxplot(
    data = states[states$group == "Simulated", ],
    aes(x = Guest, y = values, group = Guest)
  ) +
  geom_point(
    data = states[states$group == "Measured", ],
    aes(x = Guest, y = values, colour = "Measured")
  ) +
  facet_wrap(~ signal_names, scales = "free") +
  labs(x = "Total Guest Measured [M]", y = NULL, colour = NULL)
p

ggsave(p,
  file = "./Paper/GlobalAnalysis/SignalPlot.png",
  width = 10,
  height = 12.5
)

# Parameter plots
# ====================================================================
params <- lapply(seq_len(length(res)), \(i) {
  res[[i]][["parameter"]]
})
params <- Reduce(rbind, params)

p_Ka <- ggplot(data = params[, 1, drop = FALSE],
  aes(y = .data[["Ka(HG) [1/M]"]])) +
  geom_boxplot() +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    legend.position = "bottom",
    legend.key.size = unit(0.6, "cm"),
    legend.key = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

params <- stack(params[, 2:19])
params$signal <- gsub(".*(Nr\\.[0-9]+).*", "\\1", params$ind)
params$signal <- gsub("Nr.", "", params$signal) |> as.numeric()
wls <- c(470, 486, 512, 524, 532, 550)
params$signal <- vapply(params$signal, function(i) {
  wls[i]
}, numeric(1))
params$parameter <- sub("^.*?(I)", "\\1", params$ind)
head(params)
tail(params)
params

p <- ggplot(data = params, aes(x = signal, y = values, group = signal)) +
  geom_boxplot() +
  facet_wrap(~ parameter, scales = "free") +
  labs(x = "Signal [nm]", y = NULL) +
  theme(
    legend.title = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    legend.position = "bottom",
    legend.key.size = unit(0.6, "cm"),
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(size = 16)
  )

ggsave(p,
  file = "./Paper/GlobalAnalysis/ParameterPlots.png",
  width = 10,
  height = 12.5
)
