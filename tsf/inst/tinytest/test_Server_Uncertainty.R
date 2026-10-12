library(tsf)
library(tinytest)

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")

task <- tsf:::Task$new()
task$start(tsf:::warm_ast2ast_cache)
task$wait()

progress_file <- tempfile()
file.create(progress_file)

path <- system.file("examples", "IDA.txt", package = "tsf")
raw <- tsf:::importData(path)
names(raw)[1] <- "var"
names(raw)[2:ncol(raw)] <- paste0("signal", seq_len(ncol(raw) - 1L))
df_reactive <- shiny::reactiveValues(df = raw, nsigs = ncol(raw) - 1L)

batch_path <- system.file("examples", "IDABatch.csv", package = "tsf")
df_list <- suppressWarnings(tsf:::importDataBatch(batch_path))
df_list_reactive <- shiny::reactiveValues(data_frames = df_list)

extra_inputs <- list(H0 = "5", D0 = "6", kHD = "700000")

result <- NULL
shiny::testServer(
  tsf:::server_opti_sensi_batch,
  args = list(
    id = "IDA", df_reactive = df_reactive, df_list_reactive = df_list_reactive,
    nclicks = shiny::reactiveVal(0), task = task, progress_file = progress_file
  ),
  {
    # Drive a single-run Optimization to completion (needed by the "direct"
    # bootstrap method, which reuses its lb/ub/additionalParameters).
    do.call(session$setInputs, extra_inputs)
    session$setInputs(I0_lb = "0", I0_ub = "1e6")
    session$setInputs(IHD_lb = "0", IHD_ub = "1e6")
    session$setInputs(ID_lb = "0", ID_ub = "1e6")
    session$setInputs(Confirm = 1)
    session$setInputs(npop = 10, ngen = 15, threshold = 0.00001, topology = "random",
      error_calc_fct = "rel. Error", Seed = 1234, kHG_lb = "1", kHG_ub = "1e9")
    session$setInputs(Start_Opti = 1)
    for (i in 1:60) {
      Sys.sleep(0.25)
      session$elapse(300)
      if (opti_result_created()) break
    }

    # Drive a Batch-PSO run to completion (needed by the "batch" method).
    do.call(session$setInputs, setNames(extra_inputs, paste0(names(extra_inputs), "_batch")))
    session$setInputs(I0_lb_batch = "0", I0_ub_batch = "1e6")
    session$setInputs(IHD_lb_batch = "0", IHD_ub_batch = "1e6")
    session$setInputs(ID_lb_batch = "0", ID_ub_batch = "1e6")
    session$setInputs(Confirm_batch = 1)
    # NumRepDataset needs to be large enough that ks::Hpi() (automatic
    # bandwidth selection) doesn't hit a singular covariance matrix - too few
    # or too-similar repeated fits (npop/ngen this small converge to
    # near-identical points often) makes that fail.
    session$setInputs(NumRepDataset = 10, npop_batch = 10, ngen_batch = 15,
      threshold_batch = 0.00001, topology_batch = "random",
      error_calc_fct_batch = "rel. Error", Seed_batch = 1234,
      kHG_lb_batch = "1", kHG_ub_batch = "1e9")
    session$setInputs(Start_Batch = 1)
    for (i in 1:200) {
      Sys.sleep(0.25)
      session$elapse(300)
      if (batch_results_created()) break
    }

    # "direct" method
    session$setInputs(uncertainty_method = "direct")
    session$setInputs(unc_direct_nBoot = 5, unc_direct_seed = 1)
    session$setInputs(Start_Uncertainty = 1)
    for (i in 1:60) {
      Sys.sleep(0.25)
      session$elapse(300)
      if (uncertainty_result_created()) break
    }
    direct_result <<- uncertainty_result()

    # "batch" method
    session$setInputs(uncertainty_method = "batch")
    session$setInputs(unc_best_pct = 50, unc_n_boot = 20)
    session$setInputs(Start_Uncertainty = 1)
    for (i in 1:120) {
      Sys.sleep(0.25)
      session$elapse(300)
      if (uncertainty_result_created()) break
    }
    batch_result <<- uncertainty_result()
  }
)

test_direct_uncertainty <- function() {
  expect_true(is.data.frame(direct_result), info = "direct method returns a data.frame")
  expect_equal(names(direct_result), c("param", "estimate", "lower", "upper"))
  expect_equal(nrow(direct_result), 4L, info = "one row per core parameter (Ka + I0/IHD/ID)")
}
test_direct_uncertainty()

test_batch_uncertainty <- function() {
  expect_true(is.data.frame(batch_result), info = "batch method returns a data.frame")
  expect_equal(names(batch_result), c("param", "estimate", "lower", "upper"))
  expect_equal(nrow(batch_result), 4L, info = "one row per core parameter (Ka + I0/IHD/ID)")
}
test_batch_uncertainty()

task$close()
