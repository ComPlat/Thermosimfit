library(ggplot2)
library(cowplot)

sensi_plot <- function(case, path) {
  load(path)
  parameter <- lapply(seq_len(length(result)), function(idx) {
    res <- result[[idx]][[2]]
    return(res)
  })
  parameter <- Reduce(rbind, parameter)
  parameter <- apply(parameter, 2, mean)
  parameter <- as.data.frame(t(parameter))
  res <- result[[1]][[1]]
  ap <- result[[1]]$additionalParameters
  tsf::sensitivity(case, parameter, res, ap, 5)
}

p_dba <- sensi_plot("dba_dye_const", "../DecentFitParameterVariance/DBA_10_different_seeds.RData")
p_ida <- sensi_plot("ida", "../DecentFitParameterVariance/IDA_10_different_seeds.RData")
p_gda <- sensi_plot("gda", "../DecentFitParameterVariance/GDA_10_different_seeds.RData")

p <- plot_grid(
  p_dba, p_ida, p_gda,
  ncol = 3,
  labels = c("a", "b", "c")
)

save(p_dba, p_ida, p_gda, p, file = "Sensitivity.RData")
