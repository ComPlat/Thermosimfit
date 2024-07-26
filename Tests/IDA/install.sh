#!/bin/bash

R CMD INSTALL ../../tsf

Rscript tests.R
