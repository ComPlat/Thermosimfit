library(tinytest)
library(tsf)

# test batch dba dye const
test_batch_dba_dye_const <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/DBADyeConstBatch.txt")
  lowerBounds <- c(
    kHD = 1000,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kHD = 10^10,
    I0 = 10^10,
    IHD = 10^10,
    ID = 10^10
  )
  additionalParameters <- c(
    host = 0.000151
  )
  res <- tsf::batch(
    "dba_dye_const",
    lowerBounds, upperBounds,
    path, additionalParameters,
    ngen = 100,
    num_cores = 6,
    num_rep = 3
  )
  metrices <- Reduce(rbind, res[[1]][[3]])
  trash <- sapply(metrices$R2, function(x) {
    expect_true(x > 0.95)
  })
  return(trash)
}
test_batch_dba_dye_const()

# test batch dba host const
test_batch_dba_host_const <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/DBAHostConstBatch.txt")
  lowerBounds <- c(
    kHD = 1000,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kHD = 10^10,
    I0 = 10^10,
    IHD = 10^10,
    ID = 10^10
  )
  additionalParameters <- c(
    host = 0.000151
  )
  res <- tsf::batch(
    "dba_host_const",
    lowerBounds, upperBounds,
    path, additionalParameters,
    ngen = 100,
    num_cores = 6,
    num_rep = 3
  )
  metrices <- Reduce(rbind, res[[1]][[3]])
  trash <- sapply(metrices$R2, function(x) {
    expect_true(x > 0.95)
  })
  return(trash)
}
test_batch_dba_host_const()

# test batch gda
test_batch_gda <- function() {
  path <- paste0(system.file("examples", package = "tsf"), "/GDABatch.txt")
  lowerBounds <- c(
    kG = 1000,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kG = 10^10,
    I0 = 10^10,
    IHD = 10^10,
    ID = 10^10
  )
  additionalParameters <- c(
    host = 1.65E-06,
    guest = 1.32E-06,
    kHD = 1.7E07
  )
  res <- tsf::batch(
    "gda",
    lowerBounds, upperBounds,
    path, additionalParameters,
    ngen = 150,
    num_cores = 6,
    num_rep = 3
  )
  metrices <- Reduce(rbind, res[[1]][[3]])
  trash <- sapply(metrices$R2, function(x) {
    expect_true(x > 0.95)
  })
  return(trash)
}
test_batch_gda()

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
    ngen = 100,
    num_cores = 6, # NOTE: depending of hardware has to be adapted
    num_rep = 3 # NOTE: thus 6 runs in total
  )
  metrices <- Reduce(rbind, res[[1]][[3]])
  trash <- sapply(metrices$R2, function(x) {
    expect_true(x > 0.95)
  })
  return(trash)
}
test_batch()

# test batch with invalid:
invalid_path <- function() {
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
  res <- try(tsf::batch(
    "ida",
    lowerBounds, upperBounds,
    path = "Invalid",
    additionalParameters,
    ngen = 100,
    num_cores = 6, # NOTE: depending of hardware has to be adapted
    num_rep = 3 # NOTE: thus 6 runs in total
  ))
  expect_true(class(res) == "try-error")
  return()
}
invalid_path()

invalid_model <- function() {
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
  res <- try(tsf::batch(
    "bla",
    lowerBounds, upperBounds,
    path,
    additionalParameters,
    ngen = 100,
    num_cores = 6,
    num_rep = 3
  ))
  expect_true(class(res) == "try-error")
  return()
}
invalid_model()

invalid_num_rep <- function() {
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
  res <- try(tsf::batch(
    "ida",
    lowerBounds, upperBounds,
    path,
    additionalParameters,
    ngen = 100,
    num_cores = 6,
    num_rep = -3
  ))
  expect_true(class(res) == "try-error")
  return()
}
invalid_num_rep()

invalid_num_cores <- function() {
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
  res <- try(tsf::batch(
    "ida",
    lowerBounds, upperBounds,
    path,
    additionalParameters,
    ngen = 100,
    num_cores = -3,
    num_rep = 3
  ))
  expect_true(class(res) == "try-error")
  return()
}
invalid_num_cores()

# TODO: test need better error message
invalid_ap <- function() {
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
    host = 1e-6
  )
  res <- try(tsf::batch(
    "ida",
    lowerBounds, upperBounds,
    path,
    additionalParameters,
    ngen = 100,
    num_cores = 3,
    num_rep = 3
  ))
  expect_true(class(res) == "try-error")
  return()
}
invalid_ap()
