#!/bin/bash

R CMD INSTALL ./tsf

Rscript -e "tsf::runApp(4000)"
