# INFO:
# 3 parameter sets were tested in the Mathematica software
# The files are named ida_seed_NUMBER.csv
# The measured value are in IDA.txt
# Furthermore:
# conc_host (CB7): 4.3 µM
# conc_dye(TNS): 6 µM
# Ka(HD) = 1.70E+07
setwd("./CrossValidation")

read_mathematica_output <- function(path, seed) {
  inp <- readLines(path, n = 103)
  inp <- inp[3:103]
  inp <- lapply(inp, function(x) {
    x <- strsplit(x, ",")[[1]]
    x[1:2]
  })
  names(inp) <- NULL
  df <- do.call(rbind, inp)
  df <- data.frame(Guest = df[, 1], Signal = df[, 2])
  df$Guest <- as.numeric(df$Guest)
  df$Signal <- as.numeric(df$Signal)
  df$Group <- paste0("Mathematica_", seed)
  df
}

# setwd("./Paper/CrossValidation")
ida <- read.table("IDA.txt", header = FALSE)
names(ida) <- c("Guest", "Signal")
ida$Group <- "measured"
path1 <- "ida_seed_267931.csv"
path2 <- "ida_seed_347140.csv"
path3 <- "ida_seed_424221.csv"

# 264409330.959622			21.82678196017			1302253548.71118			14630633.4035409			267931
df1 <- read_mathematica_output(path1, 267931)
# 241411574.51663			16.6112221126833			1261394467.79288			12962919.598762			347140
df2 <- read_mathematica_output(path2, 347140)
# 241157908.922041			6.45848159957172			1275650016.66358			13397450.7311171			424221
df3 <- read_mathematica_output(path3, 424221)

# Thermosimfit
env <- new.env()
env$h0 <- 4.3 * 10^-6
env$d0 <- 6 * 10^-6
env$kd <- 1.7E07
env$ga <- ida[, 1]
env$signal <- ida[, 2]
params1 <- c(264409330.959622, 21.82678196017, 1302253548.71118, 14630633.403540)
params2 <- c(241411574.51663, 16.6112221126833, 1261394467.79288, 12962919.598762)
params3 <- c(241157908.922041, 6.45848159957172, 1275650016.66358, 13397450.731117)
res1 <- tsf:::lossFctIDA(params1, env, TRUE)
res2 <- tsf:::lossFctIDA(params2, env, TRUE)
res3 <- tsf:::lossFctIDA(params3, env, TRUE)
res1 <- data.frame(Guest = ida[, 1], Signal = res1[, 1], Group = "Thermosimfit_267931")
res2 <- data.frame(Guest = ida[, 1], Signal = res2[, 1], Group = "Thermosimfit_347140")
res3 <- data.frame(Guest = ida[, 1], Signal = res3[, 1], Group = "Thermosimfit_424221")

# Comparison
df <- rbind(ida, df1, df2, df3, res1, res2, res3)
library(ggplot2)
ggplot(data = df, aes(x = Guest, y = Signal, color = Group)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Guest (µM)", y = "Signal (a.u.)", color = "") +
  ggtitle("Comparison of measured and simulated ida values")


summary(aov(Signal ~ Group, data = df))
nrmse <- function(x, y) {
  sqrt(mean((x - y)^2)) / mean(x) / sqrt(length(x))
}
grid <- expand.grid(unique(df$Group), unique(df$Group))
grid <- grid[grid$Var1 != grid$Var2, ]
grid <- grid[1:6, ]
grid$error <- NA
for (i in seq_len(nrow(grid))) {
  x <- df[df$Group == grid[i, 1], ]
  y <- df[df$Group == grid[i, 2], ]
  x <- spline(x[, 1], x[, 2], n = 1000)$y
  y <- spline(y[, 1], y[, 2], n = 1000)$y
  grid$error[i] <- nrmse(x, y)
}

grid
write.csv(grid, "ida_comparison.csv", row.names = FALSE)

mean_mathematica <- mean(grid[1:3, 3])
mean_thermosimfit <- mean(grid[4:6, 3])
sd_mathematica <- sd(grid[1:3, 3])
sd_thermosimfit <- sd(grid[4:6, 3])
res <- data.frame(
  Mathematica = c(mean_mathematica, sd_mathematica),
  Thermosimfit = c(mean_thermosimfit, sd_thermosimfit)
)
write.csv(res, "ida_comparison_summary.csv", row.names = FALSE)
