# f <- function() {
#   Sys.sleep(15)
#   return(1)
# }
# library(future)
# plan(multisession)
# r <- future({
#   f()
# })
#
# for (i in 1:100) {
#   print(resolved(r))
#   if (resolved(r)) {
#     print(value(r))
#     break
#   }
#   Sys.sleep(0.4)
# }
#
library(tsf)
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/Batch")
library(callr)
com <- Communicator$new()

res_batch <- r(
  tsf:::batch(
    case = "ida",
    lowerBounds = c(kG = 1000, I0 = 0, IHD = 0, ID = 0),
    upperBounds = c(kG = 10^8, I0 = 100, IHD = 10^7, ID = 10^7),
    path = "idaBatch.csv",
    additionalParameters = c(host = 1.00E-06, dye = 1.00E-06, kHD = 3.00E+06),
    ngen = 25,
    runAsShiny = com
  ),
  show = FALSE
)

print(res_batch)
print("test")
stop()
# while (TRUE) {
#   current_state <- com$getData()
#   print(current_state)
#   print(readLines("../../log.txt"))
#   if (resolved(res_batch)) break
#   Sys.sleep(0.25)
# }
#
#
