#!/bin/bash

R CMD INSTALL ./tsf

Rscript -e "local<- TRUE; tsf::runApp(4000)"

# Rscript TestBatch.R
