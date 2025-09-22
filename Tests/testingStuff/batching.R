library(tsf)

test_batch <- function(error_fct) {
  path <- paste0(system.file("examples", package = "tsf"), "/IDABatch.csv")
  lowerBounds <- c(
    kG = 1000,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kG = 10^8,
    I0 = 100,
    IHD = 10^7,
    ID = 10^7
  )
  additionalParameters <- c(
    host = 1e-6,
    dye = 1e-6,
    kHD = 3e6
  )
  res <- tsf::batch(
    "ida",
    lowerBounds, upperBounds,
    path, additionalParameters,
    ngen = 100,
    num_cores = 6,
    num_rep = 3,
    seed = 1234,
    error_calc_fct = error_fct
  )
  ps <- Reduce(rbind, res[[1]]$params)
  ms <- Reduce(rbind, res[[1]]$metrices)
  list(parameter = ps, metrices = ms)
}
re <- test_batch(tsf:::rel_err)
rmse <- test_batch(tsf:::rmse)
huber <- test_batch(tsf:::huber)
sse <- test_batch(tsf:::sse)
re$metrices
rmse$metrices
huber$metrices
sse$metrices


