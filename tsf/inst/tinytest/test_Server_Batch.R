library(tsf)
library(tinytest)

if (!requireNamespace("shiny", quietly = TRUE)) exit_file("needs shiny")

task <- tsf:::Task$new()
task$start(tsf:::warm_ast2ast_cache)
task$wait()

progress_file <- tempfile()
file.create(progress_file)

cases <- list(
  HG = list(
    path = system.file("examples", "DBAHostConstBatch.txt", package = "tsf"),
    extra_inputs = list(H0_batch = "5"),
    bound_prefix = "kHD",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e6, 1e6)
  ),
  DBA = list(
    path = system.file("examples", "DBADyeConstBatch.txt", package = "tsf"),
    extra_inputs = list(D0_batch = "5"),
    bound_prefix = "kHD",
    lb = c(1, 0, 1e2, 1e2), ub = c(1e8, 1e4, 1e8, 1e8)
  ),
  IDA = list(
    path = system.file("examples", "IDABatch.csv", package = "tsf"),
    extra_inputs = list(H0_batch = "5", D0_batch = "6", kHD_batch = "700000"),
    bound_prefix = "kHG",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e5, 1e5)
  ),
  GDA = list(
    path = system.file("examples", "GDABatch.txt", package = "tsf"),
    extra_inputs = list(H0_batch = "1.65", G0_batch = "1.8", kHD_batch = "1.7e7"),
    bound_prefix = "kHG",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e6, 1e6)
  )
)

run_batch_case <- function(case_name) {
  cfg <- cases[[case_name]]
  df_list <- suppressWarnings(tsf:::importDataBatch(cfg$path))
  lb_input <- paste0(cfg$bound_prefix, "_lb_batch")
  ub_input <- paste0(cfg$bound_prefix, "_ub_batch")

  out <- NULL
  shiny::testServer(
    tsf:::server_opti_sensi_batch,
    args = list(
      id = case_name, df_reactive = shiny::reactiveValues(df = NULL, nsigs = 1L),
      df_list_reactive = shiny::reactiveValues(data_frames = df_list),
      nclicks = shiny::reactiveVal(0), task = task, progress_file = progress_file
    ),
    {
      do.call(session$setInputs, cfg$extra_inputs)
      session$setInputs(I0_lb_batch = "0", I0_ub_batch = "1e6")
      session$setInputs(IHD_lb_batch = "0", IHD_ub_batch = "1e6")
      session$setInputs(ID_lb_batch = "0", ID_ub_batch = "1e6")
      session$setInputs(Confirm_batch = 1)

      inputs <- list(NumRepDataset = 1, npop_batch = 10, ngen_batch = 15,
        threshold_batch = 0.00001, topology_batch = "random",
        error_calc_fct_batch = "rel. Error", Seed_batch = 1234)
      inputs[[lb_input]] <- as.character(cfg$lb[1])
      inputs[[ub_input]] <- as.character(cfg$ub[1])
      do.call(session$setInputs, inputs)

      session$setInputs(Start_Batch = 1)
      for (i in 1:180) {
        Sys.sleep(0.25)
        session$elapse(300)
        if (batch_results_created()) break
      }
      out <<- list(created = batch_results_created(), result = result_batch())
    }
  )
  list(out = out, n_datasets = length(df_list))
}

test_batch_server <- function(case_name) {
  res <- run_batch_case(case_name)
  out <- res$out
  expect_true(out$created, info = paste(case_name, "batch_results_created"))
  expect_true(inherits(out$result, "BatchResult"), info = paste(case_name, "result is BatchResult"))
  values <- out$result[[1]]
  expect_true(is.list(values$params), info = paste(case_name, "params is a list"))
  expect_equal(length(values$params), res$n_datasets, info = paste(case_name, "one params entry per dataset"))
  for (p in values$params) {
    expect_true(is.data.frame(p), info = paste(case_name, "each params entry is a data.frame"))
  }
}

test_batch_server("HG")
test_batch_server("DBA")
test_batch_server("IDA")
test_batch_server("GDA")

task$close()
