install.packages("tsf", repos = NULL, type = "source")
detach("package:tsf", unload = TRUE)

library(tsf)
# Kg1 = 1e6      # first‐step binding constant
# Kg2 = 2e5      # second‐step binding constant
# I0 = 0       # baseline signal
# IH   = 0       # signal coefficient for free H (set to 0: no signal from free H)
# IG   = 1e6     # signal coefficient for free G
# IHG = 1e7      # signal coefficient for HG
# IHG2 = 1e5     # signal coefficient for HG2

library(rootSolve)
library(ggplot2)
files <- list.files("./tsf/R", full.names = TRUE)
trash <- lapply(files, source)

combine <- function(results, seeds, df) {
  parameters <- lapply(seq_len(length(results)), function(i) {
    temp <- results[[i]][[2]] |> t() |> as.data.frame()
    temp$seed <- seeds[i]
    names(temp) <- c("Kg", "Kgg", "I0", "IH", "IG", "IHG", "IHGG", "seed")
    temp
  })
  data <- lapply(seq_len(length(results)), function(i) {
    temp <- results[[i]][[1]]
    temp$seed <- seeds[i]
    temp$run <- i
    temp$Guest <- df[[1]]
    temp
  })
  errors <- lapply(data, function(d) {
    sqrt(sum( (d[[1]] - d[[4]])^2 ) / nrow(d))
  })
  es <- unlist(errors) |> order()
  decent_runs <- es[1:10]

  data <- data[decent_runs]
  parameters <- parameters[decent_runs]
  errors <- errors[decent_runs]
  parameters <- do.call(rbind, parameters)
  data <- do.call(rbind, data)
  errors <- do.call(rbind, errors)
  list(data, parameters, errors)
}

case <- "dba_host_const2"
lb <- c(kg = 10^2, kgg = 10^1, I0 = 0,    IH = 0,    IG = 10^2, IHG = 10^3, IHGG = 10^3)
ub <- c(kg = 10^8, kgg = 10^8, I0 = 10^6, IH = 10^6, IG = 10^9, IHG = 10^9, IHGG = 10^8)

df <- read.csv("./HG2/signal.csv", sep = ",")
names(df) <- c("Guest", "Signal")
ap <- c(H0 = 3e-4)
env <- new.env(parent = emptyenv())
env$guest <- df[, 1]
env$signal <- df[, 2]
env$h0 <- 3e-4

seeds <- sample(1:10^5, 25)
results <- lapply(seeds, function(seed) {
  opti(case, lb, ub, df, ap, seed, 40, 6000, "random", add_info = paste0("Current seed: ", which(seed == seeds)))
})

# saveRDS(results, "25Runs.RDS")

res <- combine(results, seeds, df)
parameters <- stack(res[[2]][, 1:7])
parameters$seed <- res[[2]][[8]]
p <- ggplot() +
  geom_line(data = df, aes(x = Guest, y = Signal)) +
  geom_point(data = res[[1]], aes(x = Guest, y = insilico, colour = run))
ggsave("Trajectories.png", p)
p <- ggplot(data = parameters, aes(x = seed, y = values, group = 1)) +
  geom_boxplot() +
  facet_wrap(.~ ind, scales = "free")
ggsave("Parameters.png", p)
