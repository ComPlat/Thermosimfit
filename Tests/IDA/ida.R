# Quantity	Value
# conc(H)	1.00E-06
# conc(D)	1.00E-06
# conc(G)	1/200000
# Kequ(HD)	3.00E+06
# Kequ(HG)	2.00E+07
# Signal-0	0
# Signal-HD	1.00E+06
# Signal-Dye	2.00E+05

library(tsf)
df <- read.csv("forKonrad-conc-vs-signal.csv",
  sep = ";",
  dec = ".",
  header = FALSE
)
l <- lapply(1234:1243, function(x) {
  set.seed(x)
  opti(
    case = "ida",
    lowerBounds = c(
      kG = 1000,
      I0 = 0,
      IHD = 0,
      ID = 0
    ),
    upperBounds = c(
      kG = 10^8,
      I0 = 100, # started at 10^7 but it ended always at 0...
      IHD = 10^7,
      ID = 10^7
    ),
    df,
    additionalParameters = c(
      host = 1.00E-06,
      dye = 1.00E-06,
      kHD = 3.00E+06
    ),
    npop = 40,
    ngen = 1000,
    Topology = "random",
    errorThreshold = 0.7
  )
})

pdf("Plots.pdf")
lapply(l, function(x) {
  x[[3]]
})
dev.off()

