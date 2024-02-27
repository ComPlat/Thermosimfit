#!/bin/bash

Rscript -e 'roxygen2::roxygenize("tsf")' && R CMD INSTALL tsf

Rscript -e '
library(tsf)
library(ggplot2)
path <- paste0(system.file("examples", package = "tsf"), "/IDA.txt")
res <- opti("ida", c(1, 0, 0, 0), c(10^9, 10^6, 10^6, 10^6), path, c(4.3, 6.0, 7079458))
lapply(1:length(res[[3]]), function(x) {
  plot(res[[3]][[x]])
  ggsave(paste0("File_", x, ".png"))
})
resSensitivity <- sensitivity("ida", res[[2]], path, c(4.3, 6.0, 7079458))
lapply(1:length(resSensitivity, function(x) {
  plot(resSensitivity)
  ggsave(paste0("File_", x, ".png"))
})
'

