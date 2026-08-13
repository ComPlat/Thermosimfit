library(tsf)

pathToGdaData <- "./Tests/GDA/data_GDA-Estradiol-CB7-BE.txt"
gdaData <- read.csv(pathToGdaData, sep = "\t", dec = ".", header = TRUE)
gdaData[, 1] <- gdaData[, 1] / 10^3

hostConcentration <- 1.65E-06
guestConcentration <- 1.32E-06
dyeHostAssociationConstant <- 1.7E07

fit <- opti_vapro(
  case = "gda",
  lowerBounds = 10,
  upperBounds = 10^8,
  path = gdaData,
  additionalParameters = c(hostConcentration, guestConcentration, dyeHostAssociationConstant),
  nGrid = 2000
)
fit$parameter
fit$metrices
fit$signal_plots[[1]]
fit$d_hd_plot

bootstrap <- opti_vapro_bootstrap(
  case = "gda",
  lowerBounds = 10,
  upperBounds = 10^8,
  path = gdaData,
  additionalParameters = c(hostConcentration, guestConcentration, dyeHostAssociationConstant),
  nBoot = 2000,
  nGrid = 1000,
  seed = 1234
)
bootstrap$summary
bootstrap$sigma
bootstrap$draws
plot(bootstrap$draws[[1]], bootstrap$draws$loss)
plot(bootstrap$draws[[4]], bootstrap$draws$loss)
