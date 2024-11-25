# Optimizing Unknown Parameters in Algebraic Systems for Thermodynamic Bindings

This repository contains tools for optimizing unknown parameters in algebraic systems that describe thermodynamic bindings of dyes and guests to a host.
The package includes a script interface and a Shiny app for convenient usage.

## Documentation

[Documentation](https://complat.github.io/Thermosimfit/)

## Features

- Identification of specific parameters (kHD or kGuest, I0, IHD, and ID) of different algebraic systems using the R package tsf.
- Preliminary work involves defining systems in Maxima, and subsequent elimination of specific variables, yielding two polynomials.
  More details can be found in [documentation](./Documentation) and [maxima code](./tsf/inst/maxima_code).
- Each polynomial includes known variables (e.g., concentration of the host), variables to be optimized (e.g., kGuest), and an unknown variable (either d or hd).
- For each algebraic system, a loss function is defined in R, taking the four parameters to be optimized as arguments and returning an error for the received parameter set.
- The parameterset consists of kHD or kGuest, I0, IHD, and ID, along with constant parameters such as the concentration of the host and a data.frame
  containing the independent variable (dye or guest) and the corresponding signal.
- Roots are calculated for each polynomial at each concentration of the independent variable, defining the concentration of either d or hd to the independent variable.
- In silico signal calculation based on the formula: SignalInSilico = I0 + IHD[hd] + ID[d].
- Comparison of the in silico signal to the measured signal, with the error returned by the loss function indicating the match quality.

## Installation

### Install R, RStudio, and a GitHub Package

#### Step 1: Install R

##### Linux (Ubuntu)
```bash
sudo apt update
sudo apt install -y r-base
```
Verify installation:
```bash
R --version
```

##### Windows/Mac
- Download R from [CRAN](https://cran.r-project.org/).
- Follow the installer instructions.

#### Step 2: Install RStudio
1. Download RStudio Desktop (free version) from [RStudio's website](https://posit.co/download/rstudio-desktop/).
2. Install it by following the installation steps for your operating system.

#### Step 3: Install `remotes` for GitHub Packages
1. Open R or RStudio.
2. Install the `remotes` package:
```R
install.packages("remotes")
```

#### Step 4: Install a Package from GitHub
1. Load the `remotes` library:
```R
library(remotes)
```
2. Use the `install_github` function to install the package "tsf"
```R
remotes::install_github("ComPlat/Thermosimfit", subdir = "tsf")
```
