#!/bin/bash

ssh -i ../thermosimfit.pem ubuntu@193.196.36.224

sudo apt-get update

sudo apt install r-base-core
#sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
#sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

# install 
# 	- devtools
#   - remotes::install_github("ComPlat/Thermosimfit")