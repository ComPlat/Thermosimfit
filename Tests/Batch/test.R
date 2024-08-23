library(tsf)
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/Batch/")

res_batch <- tsf:::batch(
  case = "ida",
  lowerBounds = c(kG = 10, I0 = 0, IHD = 0, ID = 0),
  upperBounds = c(kG = 10^10, I0 = 10^7, IHD = 10^7, ID = 10^7),
  path = 
  "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/Reproducability/Test1/Combined",
  additionalParameters = c(host = 100, dye = 100, kHD =  2398.833 * 10^6),
  ngen = 2000, num_rep = 2
)
saveRDS(res_batch[[1]], "res_batch.rds")

# res_batch
