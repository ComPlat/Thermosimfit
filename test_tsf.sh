#!/bin/bash

Rscript -e 'roxygen2::roxygenize("tsf")' && R CMD INSTALL tsf

Rscript -e '
library(tsf)
f <- function() {
           h + hd + hga -h0 = 0
           d + hd -d0 = 0
           ga + hga -ga0 = 0
           hga / (h*ga) -kga = 0
           hd / (h*d) -kd = 0
}
listElimVars <- list(c("h", "d", "hga", "ga"), c("hd", "h", "hga", "ga"))
unknownRoots <- c("d", "hd")
parameterVec <- c("kga", "I0", "IHD", "ID")
additionalParameterVec <- c("h0", "kd", "d0")
variedParameterVec <- c("ga0", "signal")
linearSystem <- function() {I0 + IHD * hd + ID * d}
createLossFunction(f, listElimVars, unknownRoots, parameterVec,
              additionalParameterVec,
              variedParameterVec, linearSystem)
.traceback()
'
exit

Rscript -e '
library(tsf)
f <- function() {
  h + hd + -h0 = 0
  d + hd -d0 = 0
  hd / (h*d) -kd = 0
}
elimVars <- c("h", "d")
createPolynom(f, elimVars)
'

Rscript -e '
set.seed(123)
library(tsf)
library(ggplot2)
library(patchwork)
path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
res <- opti("ida", c(1, 0, 0, 0), c(10^9, 10^6, 10^6, 10^6), path, c(4.3, 6.0, 7079458),
            npop = 40, ngen = 200)
p <- res[[3]][[1]] / res[[3]][[2]]
plot(p)
ggsave("Overview.png")
sensitivity("ida", res[[2]], path, c(4.3, 6.0, 7079458), 
            OffsetBoundaries = c(1000, 100, 200, 100))
ggsave("Sensitivity.png")
'

