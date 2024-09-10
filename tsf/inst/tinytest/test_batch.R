library(tinytest)
library(tsf)

# tests batch
test_batch <- function() {
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
    num_cores = 2,
    num_rep = 3 # NOTE: thus 6 runs in total
  )
  lapply(res[[1]], function(x) {
    expect_true(x[[3]]$R2 > 0.99)
  })
}

test_batch()
