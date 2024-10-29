dba <- function() {
  lowerBounds <- c(
    kHD = 0,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kHD = 10^5,
    I0 = 10^5,
    IHD = 10^8,
    ID = 10^7
  )
  additionalParameters <- c(
    dye = 151 * 10^-6
  )
  tsf::batch("dba_dye_const",
    lowerBounds, upperBounds,
    path = "../DBA.txt",
    additionalParameters,
    ngen = 150,
    num_rep = 5, num_cores = 5,
    errorThreshold = 0.6
  )
}
res <- dba()
m <- res[[1]]$metrices
m <- Reduce(rbind, m)
print(m)
p <- res[[1]]$params
p <- Reduce(rbind, p)
p$error <- m[, 1]
print(p)

save(res, file = "dba.RData")
stop()


gda <- function() {
  lowerBounds <- c(
    kG = 100,
    I0 = 0,
    IHD = 0,
    ID = 100
  )
  # NOTE: upper bounds are set due to results of preliminary runs
  upperBounds <- c(
    kG = 10^6,
    I0 = 10^3,
    IHD = 10^10,
    ID = 10^6
  )
  additionalParameters <- c(
    host = 50 * 10^-6,
    guest = 292 * 10^-6,
    kHD = 33000
  )
  tsf::batch("gda",
    lowerBounds, upperBounds,
    path = "../NewGDA/GDA_system_3.txt",
    additionalParameters,
    ngen = 100,
    num_rep = 5, num_cores = 5,
    errorThreshold = 1
  )
}
res <- gda()
save(res, file = "gda.RData")


dba <- function() {
  lowerBounds <- c(
    kHD = 0,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kHD = 10^7,
    I0 = 10^5,
    IHD = 10^8,
    ID = 10^7
  )
  additionalParameters <- c(
    dye = 151 * 10^-6
  )
  tsf::batch("dba_dye_const",
    lowerBounds, upperBounds,
    path = "../DBA.txt",
    additionalParameters,
    ngen = 1000,
    num_rep = 5, num_cores = 5,
    errorThreshold = 0.6
  )
}
res <- dba()
save(res, file = "dba.RData")

ida <- function() {
  lowerBounds <- c(
    kG = 0,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kG = 10^10,
    I0 = 10^2,
    IHD = 10^10,
    ID = 10^10
  )
  additionalParameters <- c(
    host = 4.3 * 10^-6,
    dye = 6 * 10^-6,
    kHD = 1.7E07
  )

  tsf::batch("ida",
    lowerBounds, upperBounds,
    path = "../IDA.txt",
    additionalParameters,
    ngen = 1000,
    num_rep = 5, num_cores = 5,
    errorThreshold = 0.6
  )
}
res <- ida()
save(res, file = "ida.RData")
