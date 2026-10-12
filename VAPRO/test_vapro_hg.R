library(tsf)

pathToHgData <- "./Tests/DBA_const_host/dba_dye_const.txt"
hostConcentration <- 0.000151

fit <- opti_vapro(
  case = "dba_host_const",
  lowerBounds = 1,
  upperBounds = 10^8,
  path = pathToHgData,
  additionalParameters = hostConcentration,
  nGrid = 2000
)
fit$parameter
fit$metrices
fit$signal_plots[[1]]
fit$d_hd_plot

bootstrap <- opti_vapro_bootstrap(
  case = "dba_host_const",
  lowerBounds = 1,
  upperBounds = 10^8,
  path = pathToHgData,
  additionalParameters = hostConcentration,
  nBoot = 200,
  nGrid = 1000,
  seed = 1234
)
bootstrap$summary
plot(bootstrap$draws[[1]], bootstrap$draws$loss)
