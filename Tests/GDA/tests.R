# Quantity	Value
# conc(H)	1.65e-06
# conc(Guest)	1.32e-06
# Kequ(HD)	1.7e07
# Kequ(HG)	1.8e06
# Signal-0	0.0408218
# Signal-HD	602000
# Signal-Dye	0

library(tsf)
df <- read.csv("data_GDA-Estradiol-CB7-BE.txt",
  sep = "\t",
  dec = ".",
  header = FALSE
)

df[, 1] <- df[, 1] / 10^3

res <- opti(
  case = "gda",
  lowerBounds = c(
    kG = 10,
    I0 = 0,
    IHD = 0,
    ID = 0
  ),
  upperBounds = c(
    kG = 10^8,
    I0 = 10^8,
    IHD = 10^10,
    ID = 10^8
  ),
  path = df,
  seed = 1234,
  additionalParameters = c(
    host = 1.65E-06,
    guest = 1.32E-06,
    kHD = 1.7E07
  ),
  npop = 40,
  ngen = 1000,
  Topology = "random",
  errorThreshold = 0.3
)


res[[3]]
