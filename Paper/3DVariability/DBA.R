library(tsf)
library(plotly)
# Conduct one optimization
# ===============================
lowerBounds <- c(
  kHD = 0,
  I0 = 0,
  IHD = 0,
  ID = 0
)
upperBounds <- c(
  kHD = 10^7,
  I0 = 10^5,
  IHD = 10^8,
  ID = 10^7
)
additionalParameters <- c(
  dye = 151 * 10^-6
)

# NOTE: data var column [M]
data <- readLines("../DBA.txt")
var <- lapply(data, function(x) {
  strsplit(x, split = "\t")[[1]][1]
}) |> unlist()
signal <- lapply(data, function(x) {
  strsplit(x, split = "\t")[[1]][2]
}) |> unlist()
# NOTE: Only the first dataset is used
df <- data.frame(var = var[2:28], signal = signal[2:28])
df$var <- as.numeric(df$var)
df$signal <- as.numeric(df$signal)
seed <- 49876
result <- opti(
  case = "dba_dye_const",
  lowerBounds = lowerBounds,
  upperBounds = upperBounds,
  path = df,
  seed = seed,
  ngen = 5000,
  npop = 100,
  errorThreshold = 0.4,
  additionalParameters = additionalParameters,
  add_info = as.character(seed)
)

# create new parameters based on optimization result
# ===============================
best_params <- result[[2]]
lb <- best_params * 0.8
ub <- best_params * 1.2

new_params <- lapply(1:4, function(x) {
  r <- runif(5000)
  lb[[x]] + (ub[[x]] - lb[[x]]) * r
})
new_params <- Reduce(cbind, new_params)
new_params <- as.data.frame(new_params)
names(new_params) <- c("kHD", "I0", "ID", "IHD")

# Evaluate the new parameters
# ===============================
env <- new.env()
env$d0 <- 151 * 10^-6
env$host <- df[, 1]
env$signal <- df[, 2]
new_params$errors <- apply(new_params, 1, function(parameter) {
  idx <- parent.frame()$i[]
  if ((idx %% 100) == 0) {
    print((100 / nrow(new_params)) * idx)
  }
  tsf:::lossFctDBA(parameter, env, FALSE)
})

# Visualize the results
# ===============================
df <- new_params[new_params$errors < 0.75, ]
p <- plot_ly(
  data = df,
  x = ~kHD,
  y = ~ID,
  z = ~errors,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2)
) %>%
  layout(
    scene = list(
      xaxis = list(title = list(text = "KaHD [1/M]")),
      yaxis = list(title = list(text = "I(0) [1/M]")),
      zaxis = list(title = list(text = "rel. Errors [%]")),
      camera = list(
        eye = list(
          x = 0.6, # rotate around y 0.6
          y = 2.4, # height of camera 2.4
          z = 1.2 # depth of campera 1.2
        )
      )
    ),
    showlegend = FALSE
  ) %>%
  add_trace(
    x = df$kHD,
    y = df$ID,
    z = df$errors,
    type = "mesh3d",
    alphahull = 6,
    opacity = 0.5,
    color = "blue"
  )

save_image(p, file = "DBA.svg")

save(p, file = "DBA.RData")
