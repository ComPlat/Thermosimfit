library(ggplot2)
df <- data.frame(
  x = rep(1:10, 2),
  y = c(1:10 + rnorm(10), 1:10 + rnorm(10, 2), 1:10 + rnorm(10), 1:10 + rnorm(10, 2)),
  group = rep(c("A.1", "B.1", "A.2", "B.2"), each = 10)
)

print(head(df))

ggplot(df, aes(x = x, y = y, color = group)) +
  geom_line()
