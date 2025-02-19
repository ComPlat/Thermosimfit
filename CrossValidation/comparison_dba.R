# INFO:
# 3 parameter sets were tested in the Mathematica software
# The files are named dba_seed_NUMBER.csv
# The measured value are in DBA.txt
# Furthermore, 151 µM host

read_mathematica_output <- function(path, seed) {
  inp <- readLines(path, n = 53)
  inp <- inp[3:53]
  inp <- lapply(inp, function(x) {
    x <- strsplit(x, ",")[[1]]
    x[1:2]
  })
  names(inp) <- NULL
  df <- do.call(rbind, inp)
  df <- data.frame(Host = df[, 1], Signal = df[, 2])
  df$Host <- as.numeric(df$Host)
  df$Signal <- as.numeric(df$Signal)
  df$Group <- paste0("Mathematica_", seed)
  df
}

# setwd("./Paper/CrossValidation")
# Read measured data
dba <- read.table("DBA.txt", header = FALSE)
names(dba) <- c("Host", "Signal")
dba$Group <- "measured"

# Read mathematica results
# 1950.48119355866			524.822717270241			74483163.3340886			1553144.2928931			423544
df1 <- read_mathematica_output("dba_seed_423544.csv", 423544)
# 2678.69168274464			654.640148212479			63570259.9221406			1e-15			513193
df2 <- read_mathematica_output("dba_seed_513193.csv", 513193)
# 2014.56304965819			778.146392247941			69626378.8465047			1e-15			572413
df3 <- read_mathematica_output("dba_seed_572413.csv", 572413)

# Call Thermosimfit
env <- new.env()
env$d0 <- 151 * 10^-6
env$host <- dba[, 1]
env$signal <- dba[, 2]
params1 <- c(khd = 1950.48, I0 = 524.83, IHD = 74483163.33, ID = 1553144.29)
params2 <- c(khd = 2678.69, I0 = 654.64, IHD = 63570259.92, ID = 1e-15)
params3 <- c(khd = 2014.56, I0 = 778.15, IHD = 69626378.85, ID = 1e-15)
res1 <- tsf:::lossFctDBA(params1, env, TRUE)
res2 <- tsf:::lossFctDBA(params2, env, TRUE)
res3 <- tsf:::lossFctDBA(params3, env, TRUE)
res1 <- data.frame(Host = dba[, 1], Signal = res1[, 1], Group = "Thermosimfit_423544")
res2 <- data.frame(Host = dba[, 1], Signal = res2[, 1], Group = "Thermosimfit_513193")
res3 <- data.frame(Host = dba[, 1], Signal = res3[, 1], Group = "Thermosimfit_572413")

# Combine data and evaluate differences
df <- rbind(dba, df1, df2, df3, res1, res2, res3)
df$Group <- factor(df$Group)
library(ggplot2)
ggplot(data = df, aes(x = Host, y = Signal, color = Group)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Host (µM)", y = "Signal (a.u.)", color = "") +
  ggtitle("Comparison of measured and simulated DBA values")

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
write.csv(grid, "dba_comparison.csv", row.names = FALSE)
