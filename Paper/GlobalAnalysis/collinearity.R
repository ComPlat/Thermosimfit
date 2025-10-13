library(ggplot2)
library(corrplot)
library(RColorBrewer)

dfh <- read.csv("./Paper/GlobalAnalysis/Full.csv", header = TRUE)
wl_cols <- grep("^X\\d+$", names(dfh), value = TRUE)
wl_num  <- as.numeric(gsub("X", "", wl_cols))
X <- as.matrix(dfh[wl_cols])
M <- cor(X)
corrplot(
  M,
  type = "upper", order = "hclust",
  col = brewer.pal(n=8, name="RdYlBu"),
  tl.col = "black",
  tl.cex = 0.6
)
