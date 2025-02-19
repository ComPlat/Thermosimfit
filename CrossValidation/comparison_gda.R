# INFO:
# 3 parameter sets were tested in the Mathematica software
# The files are named gda_seed_NUMBER.csv
# The measured value are in gda.txt
# Furthermore:
# conc_host (CB7): 50 µM
# conc_guest(nButanol): 292 µM
# I(0) = 29
# I(D) = 3.52E+06
# Ka(HD) = 33000
# Dye (TNS)

read_mathematica_output <- function(path, seed) {
  inp <- readLines(path, n = 103)
  inp <- inp[3:103]
  inp <- lapply(inp, function(x) {
    x <- strsplit(x, ",")[[1]]
    x[1:2]
  })
  names(inp) <- NULL
  df <- do.call(rbind, inp)
  df <- data.frame(Dye = df[, 1], Signal = df[, 2])
  df$Dye <- as.numeric(df$Dye)
  df$Signal <- as.numeric(df$Signal)
  df$Group <- paste0("Mathematica_", seed)
  df
}

# setwd("./Paper/CrossValgdation")
gda <- read.table("GDA_system_3.txt", header = FALSE)
names(gda) <- c("Dye", "Signal")
gda$Group <- "measured"
path1 <- "gda_seed_322088.csv"
path2 <- "gda_seed_295846.csv"
path3 <- "gda_seed_501115.csv"

# 83133.1200428989			405.433781253007			423487633.666695			100			322088
df1 <- read_mathematica_output(path1, 322088)
# 86202.9195886173			486.496382197214			414794390.888579			544010.069248712			295846
df2 <- read_mathematica_output(path2, 295846)
# 77100.6572423567			404.297428696769			381948765.202612			1e+06			501115
df3 <- read_mathematica_output(path3, 501115)

# Call Thermosimfit
env <- new.env()
env$h0 <- 50 * 10^-6
env$ga0 <- 292 * 10^-6
env$kd <- 33000
env$dye <- gda[, 1]
env$signal <- gda[, 2]
params1 <- c(khg = 83133.12, I0 = 405.44, IHD = 423487633.66, ID = 100)
params2 <- c(khg = 86202.919, I0 = 486.496, IHD = 414794390.88, ID = 544010.069)
params3 <- c(khg = 77100.657, I0 = 404.297, IHD = 381948765.202, ID = 1e06)
res1 <- tsf:::lossFctGDA(params1, env, TRUE)
res2 <- tsf:::lossFctGDA(params2, env, TRUE)
res3 <- tsf:::lossFctGDA(params3, env, TRUE)
res1 <- data.frame(Dye = gda[, 1], Signal = res1[, 1], Group = "Thermosimfit_322088")
res2 <- data.frame(Dye = gda[, 1], Signal = res2[, 1], Group = "Thermosimfit_295846")
res3 <- data.frame(Dye = gda[, 1], Signal = res3[, 1], Group = "Thermosimfit_501115")


# Comparison
df <- rbind(gda, df1, df2, df3, res1, res2, res3)
library(ggplot2)
ggplot(data = df, aes(x = Dye, y = Signal, color = Group)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Dye (µM)", y = "Signal (a.u.)", color = "") +
  ggtitle("Comparison of measured and simulated gda values")

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
write.csv(grid, "gda_comparison.csv", row.names = FALSE)
