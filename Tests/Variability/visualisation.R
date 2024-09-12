library(scatterplot3d)
df <- readRDS("ParameterError.rds")
df <- df[df$errors < 20, ]

unique_hd <- unique(df$IHD)
n_colors <- length(unique(df$IHD))
colors <- RColorBrewer::brewer.pal(n_colors, "Set1")
colors <- colors[match(df$IHD, unique_hd)]
s3d <- scatterplot3d(
  df[, c(1, 3, 5)],
  angle = 240,
  color = colors,
  pch = 16,
  col.grid = "grey",
  lty.grid = par("lty")
)

# create new values
best_params <- df[which.min(df$errors), ]
best_params
best_params <- best_params[1:4]

lb <- best_params * 0.8
ub <- best_params * 1.2

new_params <- lapply(1:4, function(x) {
  r <- runif(30000)
  lb[[x]] + (ub[[x]] - lb[[x]]) * r
})
new_params <- Reduce(cbind, new_params)
new_params <- as.data.frame(new_params)
names(new_params) <- c("kG", "I0", "ID", "IHD")
library(tsf)
df <- read.csv("IDA.csv",
               sep = ";",
               dec = ".",
               header = TRUE
)

env <- new.env()
env$h0 <- 1e-6
env$kd <- 3e6
env$d0 <- 1e-6
env$ga <- df[, 1]
env$signal <- df[, 2]
new_params$errors <- apply(new_params, 1, function(parameter) {
  idx <- parent.frame()$i[]
  if ((idx %% 100) == 0) {
    print((100 / nrow(new_params)) * idx)
  }
  tsf:::lossFctIDA(parameter, env, FALSE)
})

head(new_params)
df <- new_params[new_params$errors < 2, ]
library(plotly)
plot_ly(data = df,
        x = ~kG,
        y = ~ID,
        z = ~errors,
        size = 1,
        type = "scatter3d")
