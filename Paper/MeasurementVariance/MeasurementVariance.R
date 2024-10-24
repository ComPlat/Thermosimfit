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
    num_rep = 5, num_cores = 4,
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
    num_rep = 5, num_cores = 4,
    errorThreshold = 0.6
  )
}
res <- ida()
save(res, file = "ida.RData")



gda <- function() {
  lowerBounds <- c(
    kG = 10,
    I0 = 0,
    IHD = 0,
    ID = 0
  )
  upperBounds <- c(
    kG = 10^9,
    I0 = 700,
    IHD = 10^9,
    ID = 10^9
  )
  additionalParameters <- c(
    host = 103 * 10^-6,
    guest = 1050 * 10^-6,
    kHD = 2431.14
  )

  tsf::batch("gda",
    lowerBounds, upperBounds,
    path = "../GDA.txt",
    additionalParameters,
    ngen = 1000,
    num_rep = 5, num_cores = 4,
    errorThreshold = 0.6
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
    num_rep = 5, num_cores = 4,
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
    num_rep = 5, num_cores = 4,
    errorThreshold = 0.6
  )
}
res <- ida()
save(res, file = "ida.RData")
