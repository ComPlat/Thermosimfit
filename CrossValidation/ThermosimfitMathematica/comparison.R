setwd("./CrossValidation/ThermosimfitMathematica")

dba <- read.csv("dba_comparison_summary.csv")
ida <- read.csv("ida_comparison_summary.csv")
gda <- read.csv("gda_comparison_summary.csv")

df <- rbind(dba, ida, gda)
df$group <- c("mean", "sd")
df$model <- rep(c("DBA", "IDA", "GDA"), each = 2)
df <- tidyr::pivot_longer(df, cols = -c(group, model)) |> as.data.frame()
df
