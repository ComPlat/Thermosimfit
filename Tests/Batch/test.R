library(tsf)
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/Batch/")

res_batch <- tsf:::batch(
  case = "ida",
  lowerBounds = c(kG = 1000, I0 = 0, IHD = 0, ID = 0),
  upperBounds = c(kG = 10^8, I0 = 100, IHD = 10^7, ID = 10^7),
  path = "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/idaBatch.csv",
  additionalParameters = c(host = 1.00E-06, dye = 1.00E-06, kHD = 3.00E+06),
  ngen = 20, num_rep = 2
)
# saveRDS(res_batch[[1]], "res_batch.rds")

res_batch
