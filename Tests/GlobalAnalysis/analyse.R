library(rootSolve)
library(ggplot2)
library(patchwork)
files <- list.files("./tsf/R", full.names = TRUE)
trash <- lapply(files, source)

df <- read.csv(
  "./Tests/GlobalAnalysis/testGA.csv",
  sep = ";"
)

res <- opti(
  case = "ida",
  lowerBounds = c(kG = 1000,
    c(I0 = 0, IHD = 0, ID = 0),
    c(I0 = 0, IHD = 0, ID = 0),
    c(I0 = 0, IHD = 0, ID = 0)
  ),
  upperBounds = c(kG = 10^8,
    c(I0 = 10, IHD = 10^7, ID = 10^7), # Orig
    c(I0 = 10, IHD = 10^5, ID = 10^6), # Orig / 100
    c(I0 = 10^7, IHD = 10^9, ID = 10^9) # Orig * 10
  ),
  path = df,
  additionalParameters = c(
    host = 1.00E-06,
    dye = 1.00E-06,
    kHD = 3.00E+06
  ),
  npop = 40,
  ngen = 1500,
  Topology = "random",
  seed = 1234,
  errorThreshold = 0.7
)

data <- res$data
data <- data.frame(
  x = rep(data[[1]], 6),
  y = Reduce(c, data[, 2:7]),
  group = c(rep("Signal1", nrow(data)),
            rep("Signal2", nrow(data)),
            rep("Signal3", nrow(data))
          ),
  colour = c(rep("measured", 3*nrow(data)), rep("simulated", 3*nrow(data)))
)
ggplot(data = data, aes(x = x, y = y)) +
  geom_point(aes(colour = colour)) +
  facet_wrap(~ group, scales = "free")


