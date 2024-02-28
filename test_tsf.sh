#!/bin/bash

Rscript -e 'roxygen2::roxygenize("tsf")' && R CMD INSTALL tsf

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

