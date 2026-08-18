library(tsf)
library(tinytest)

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")

make_df_reactive <- function(path) {
  raw <- tsf:::importData(path)
  names(raw)[1] <- "var"
  names(raw)[2:ncol(raw)] <- paste0("signal", seq_len(ncol(raw) - 1L))
  shiny::reactiveValues(df = raw, nsigs = ncol(raw) - 1L)
}

task <- tsf:::Task$new()
task$start(tsf:::warm_ast2ast_cache)
task$wait()

progress_file <- tempfile()
file.create(progress_file)

path <- system.file("examples", "IDA.txt", package = "tsf")
df_reactive <- make_df_reactive(path)

out <- NULL
shiny::testServer(
  tsf:::server_opti_sensi_batch,
  args = list(
    id = "IDA", df_reactive = df_reactive,
    df_list_reactive = shiny::reactiveValues(data_frames = NULL),
    nclicks = shiny::reactiveVal(0), task = task, progress_file = progress_file
  ),
  {
    # Drive a VAPRO Optimization to completion first (needed by VAPRO
    # Uncertainty, which reuses its lb/ub/additionalParameters/nGrid).
    session$setInputs(H0_vapro = "5", D0_vapro = "6", kHD_vapro = "700000")
    session$setInputs(nGrid = 200, error_calc_fct_vapro = "rel. Error",
      kHG_lb_vapro = "1", kHG_ub_vapro = "1e9")
    session$setInputs(Start_Vapro_Opti = 1)
    for (i in 1:60) {
      Sys.sleep(0.25)
      session$elapse(300)
      if (vapro_opti_result_created()) break
    }

    session$setInputs(vapro_unc_nBoot = 10)
    session$setInputs(Start_Vapro_Uncertainty = 1)
    for (i in 1:60) {
      Sys.sleep(0.25)
      session$elapse(300)
      if (vapro_uncertainty_result_created()) break
    }
    out <<- vapro_uncertainty_result()
  }
)

test_vapro_uncertainty <- function() {
  expect_true(is.data.frame(out), info = "vapro uncertainty returns a data.frame")
  expect_equal(names(out), c("param", "estimate", "lower", "upper"))
  expect_equal(nrow(out), 4L, info = "one row per core parameter (Ka + I0/IHD/ID)")
}
test_vapro_uncertainty()

task$close()
