roxygen2::roxygenise("tsf")
install.packages("tsf", repos = NULL, type = "source")
tsf::runApp(4005)
tinytest::test_package("tsf")

tinytest::run_test_file("./tsf/inst/tinytest/test_opti_vapro.R")

load_packages <- function() {
  packages <- c(
    "shiny", "DT", "shinydashboard", "shinyWidgets",
    "shinyjs", "shinytest2", "rootSolve", "ggplot2", "patchwork",
    "R6", "sensitivity", "openxlsx", "callr",
    "cowplot", "RColorBrewer", "plotly", "ks"
  )
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Required package not installed: ", pkg)
    }
    library(pkg, character.only = TRUE)
  }
}
load_packages()
files <- list.files("./tsf/R", full.names = TRUE)
trash <- lapply(files, source)
runApp(4005)
