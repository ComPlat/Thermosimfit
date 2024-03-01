library(tsf)
library(ggplot2)
library(patchwork)
setwd("/home/konrad/Documents/GitHub/Thermosimfit/Reproducability")
files <- list.files(pattern = "*.txt")
kHD <- 2398.833 * 10^6# log Ka = 3.38 
H0 <- 100 # µM
d0 <- 100 # µM
# guest from 0 - 366 µM

I0 <- 600
ID <- 2.48e06

res <- opti("ida", c(1, 0, 0, 0), c(10^11, 0.1, 100, 100), files[[1]], c(H0, d0, kHD),
            npop = 40, ngen = 300, Topology = "random")
res
res[[2]]

res <- lapply(files, function(x) {
  opti("ida", c(1, 0, 0, 0), c(10^10, 10, 100, 100), x, c(H0, d0, kHD),
       npop = 40, ngen = 150, Topology = "random")
})

p <- res[[3]][[1]] / res[[3]][[2]]
plot(p)
