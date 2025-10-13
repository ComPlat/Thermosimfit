setwd("./Paper/LocationSpread")
source("loadParams.R")
source("DistributionFitting.R")
source("Bootstrapping.R")
source("Plotting.R")
source("JDKBootstrapping.R")
source("CalcValues.R")

kdjoint_smr <- jdk_smr(p_ida)
set.seed(1234)
# kdjoint <- jdk(p_ida, n_boot = 1000)
# save(kdjoint, file = "kdjoint_bootstrapped10000.RData")
distris <- c("lognorm", "exp", "norm", "norm")

data <- lapply(1:4, function(x) {
  res <- list()

  bins <- hist(p_ida[, x], plot = FALSE)$breaks
  density <-  hist(p_ida[, x], plot = FALSE)$density
  mind <- min(density)
  maxd <- max(density)
  res[["RawData"]] <- list(p_ida[, x], bins)

  median_ci <- calc_location_ci_bootstrap(median, p_ida[, x])
  median_iqr <- median_iqr(p_ida[, x])
  mean_ci <- calc_location_ci_bootstrap(mean, p_ida[, x])
  kde_ci <- modus_ci_hdr(p_ida[, x])

  set.seed(1234)
  fd <- fit_distri(p_ida[, x], distris[x])
  df <- fd$df
  df$linetype <- "Fitted distribution"
  res[["Labels"]] <- fd

  kde_ci$kd$linetype <- "Kernel density"
  df <- rbind(df, kde_ci$kd)
  res[["Density"]] <- df
  jk <- kdjoint_smr$df[[x]]
  ref_peak <- max(df$y[df$linetype == "Kernel density"], na.rm = TRUE)
  s <- ref_peak / max(jk$y, na.rm = TRUE)
  jk$y <- jk$y * s
  res[["JointKernelDensity"]] <- jk

  location_error <- data.frame(
    x = c( mean_ci$location, median_ci$location, kde_ci$mode,
      kdjoint_smr$mode[[x]], kdjoint$mode[[x]],  median_iqr[1]),
    xmin = c( mean_ci$lower_ci, median_ci$lower_ci, kde_ci$lower_ci,
      kdjoint_smr$lower_ci[[x]], kdjoint$lower_ci[[x]], median_iqr[2]),
    xmax = c( mean_ci$upper_ci, median_ci$upper_ci, kde_ci$upper_ci,
      kdjoint_smr$upper_ci[[x]], kdjoint$upper_ci[[x]], median_iqr[3]),
    type = c(
      "Mean",
      "Median",
      "Mode (KD)",
      "Mode (JDK)",
      "Mode (JDK boot.)",
      "Median IQR"
    ),
    y = seq(mind, maxd, length.out = 6)
  )
  res[["LocationError"]] <- location_error
  res
})

data[[4]][["Labels"]]

source("Plotting.R")
plots <- lapply(1:4, function(x) {
  data <- data[[x]]
  p <- plot_raw_data(data[["RawData"]][[1]], data[["RawData"]][[2]])
  # p <- add_labels(p, data[["Labels"]])
  p <- add_density(p, data[["Density"]])
  p <- add_joint_kernel_density(p, data[["JointKernelDensity"]])
  p <- add_metrices(p, data[["LocationError"]])
  p
})

legend <- get_legend(plots[[1]])
plots <- lapply(plots, function(x) {
  x + theme(legend.position = "none")
})
plot_grid <- plot_grid(
  plotlist = plots, nrow = 4,
  labels = c("A", "B", "C", "D"), label_size = 28
)
final_plot <- plot_grid(
  plot_grid, legend,
  ncol = 1,
  rel_heights = c(1, 0.075)
)
final_plot

ggsave(final_plot,
  bg = "white",
  file = "LocationEstimation_1000nboot.png",
  width = 24,
  height = 16
)
