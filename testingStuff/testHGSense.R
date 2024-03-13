library(tsf)
library(ggplot2)
library(patchwork)
setwd("/home/konrad/Documents/GitHub/Thermosimfit/Reproducability/Test1")
files <- list.files(pattern = "*.txt")
H0 <- 100 # µM
res <- opti("hg", c(1, 0, 0, 0), c(10^10, 10*10^-4, 100, 100), files[[1]], c(H0),
            npop = 40, ngen = 100, Topology = "random") # 400
res
res[[2]]
c <- tsf:::Communicator$new()
tsf::sensitivity("hg", res[[2]], files[[1]], c(H0),
                 15, runAsShiny = c)
