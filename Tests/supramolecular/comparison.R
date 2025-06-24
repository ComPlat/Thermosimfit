load("/home/konrad/Documents/Thermosimfit/Tests/supramolecular/DBA_30Sims.RData")
parameter <- lapply(res, function(x) {
  x$parameter
})
parameter <- do.call(rbind, parameter)
parameter

# Validated by http://app.supramolecular.org/bindfit/
# Model: UV1:1
# Optimizer: Nelder Mead
# Min = 0.00000001
# Max = 10

res_supra <- list()
# File 1
# Initial: 1.00e-4
res_supra[[1]] <- 0.00222043678738177
# Initial: 0.001
res_supra[[2]] <- 0.00222043679952623
# Initial: 0.01
res_supra[[3]] <- 0.00222043679654596
# Initial: 0.1 --> Fit failed
# Initial: 0.2
res_supra[[4]] <- 0.00222043681889784
# Initial: 0.3
res_supra[[5]] <- 0.00222043679212135

# File 2
# Initial: 1.00e-4
res_supra[[6]] <- 0.00227126235369592
# Initial: 0.001
res_supra[[7]] <- 0.00227126237154298
# Initial: 0.01
res_supra[[8]] <- 0.00227126234582827
# Initial: 0.1 --> Fit failed again
# Initial: 0.2 --> Fit failed
res_supra[[9]] <- 0.00227126235669896
# Initial: 0.3
res_supra[[10]] <- 0.00227126236637477

# File 3
# Initial: 1.00e-4
res_supra[[11]] <- 0.00208937814712525
# Initial: 0.001
res_supra[[12]] <- 0.00208937816918479
# Initial: 0.01
res_supra[[13]] <- 0.00208937816309842
# Initial: 0.1 --> Fit failed again --> really strange
# Initial: 0.2 --> Fit failed
res_supra[[14]] <- 0.0020893781617455
# Initial: 0.3
res_supra[[15]] <- 0.00208937816083479



com <- data.frame(
  values = c(
    parameter[[1]], unlist(res_supra)
  ),
  groups = c(
    rep("Thermosimfit", nrow(parameter)),
    rep("Bindfit", length(res_supra))
  )
)
library(ggplot2)
p <- ggplot(data = com, aes(x = groups, y = values)) +
  geom_boxplot() +
  labs(y = "Ka(HD) [1/µM]", x = "")

ggsave(p,
  file = "/home/konrad/Documents/Thermosimfit/Tests/supramolecular/Comparison_Thermosimfit_Bindifit.png"
)
