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
  metrices <- Reduce(rbind, res[[1]][[3]])
  trash <- sapply(metrices$R2, function(x) {
    expect_true(x > 0.99)
  })
  return()
}

test_batch()

# test batch gda

# test batch dba host const

# test batch dba dye const

# test batch with invalid:
#   - path
#   - model
#   - additionalParameters
#   - num_cores
#   - num_rep
