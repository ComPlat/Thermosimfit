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
invisible(lapply(files, source))

path <- system.file("extdata", "dba_dye_const_real.txt", package = "tsf")
df <- read.csv(path, header = FALSE, sep = "\t")
parameter <- c(3e3, 2.0, 1.65e7, 1.6e6)
env <- new.env()
env$d0 <- 5
env$host <- df[, 1]
env$signal <- df[, 2]
env$n_sigs <- 1L
result <- lossFctDBA(parameter, env, TRUE)
df[, 2] <- result$insilico
file <- tempfile(fileext = ".txt")
write.csv(df, file, quote = FALSE, row.names = FALSE)
res <- opti(
  "dba_dye_const",
  c(1, 0, 1e2, 1e2), c(1e8, 1e4, 1e8, 1e8),
  file, env$d0,
  npop = 40, ngen = 100,
  engine = "ast2ast",
  seed = 2L
)
traceback()
