plotTSF <- function(df, xCol) {
  df_com <- data.frame(x = rep(df[, xCol], 2), 
                       y = c(df[, "signal"], df[, "signal_insilico"]),
                       group = c(
                         rep("ytrue", length(df[, xCol]) ),
                         rep("ypred", length(df[, xCol]) )
                       ) )
  df_d_hd <- data.frame(x = rep(df[, xCol], 2), 
                        y = c(df[, "d"], df[, "hd"]),
                        group = c(
                          rep("d", length(df[, xCol]) ),
                          rep("hd", length(df[, xCol]) )
                        ) )
  p1 <- ggplot(df_com, aes(x = x, y = y, colour = group)) +
          geom_point()
  p2 <- ggplot(data = df_d_hd, aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(.~ group)
  p1 / p2
}