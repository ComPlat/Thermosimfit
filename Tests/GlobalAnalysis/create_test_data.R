
setwd("Tests/GlobalAnalysis")
df <- read.csv("ida.csv",
  sep = ";",
  dec = ".",
  header = TRUE
)
names(df)[2] <- "signal1"
df$signal2 <- df$signal1 / 100
df$signal3 <- df$signal1 * 100
write.table(df, file = "testGA.csv", quote = FALSE, sep = ";", row.names = FALSE)

library(ggplot2)
dfp <- data.frame(
  var = rep(df$var, 3),
  signal = c(df$signal1, df$signal2, df$signal3),
  group = rep(c("signal1", "signal2", "signal3"), each = nrow(df))
)

ggplot(data = dfp, aes(x = var, y = signal)) +
  geom_point() +
  facet_wrap(~ group, scales = "free")

