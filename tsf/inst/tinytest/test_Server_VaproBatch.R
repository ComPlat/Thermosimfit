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
    extra_inputs = list(H0_vapro_batch = "5"),
    bound_prefix = "kHD"
  ),
  DBA = list(
    path = system.file("examples", "DBADyeConstBatch.txt", package = "tsf"),
    extra_inputs = list(D0_vapro_batch = "5"),
    bound_prefix = "kHD"
  ),
  IDA = list(
    path = system.file("examples", "IDABatch.csv", package = "tsf"),
    extra_inputs = list(H0_vapro_batch = "5", D0_vapro_batch = "6", kHD_vapro_batch = "700000"),
    bound_prefix = "kHG"
  ),
  GDA = list(
    path = system.file("examples", "GDABatch.txt", package = "tsf"),
    extra_inputs = list(H0_vapro_batch = "1.65", G0_vapro_batch = "1.8", kHD_vapro_batch = "1.7e7"),
    bound_prefix = "kHG"
  )
)

run_vapro_batch_case <- function(case_name) {
  cfg <- cases[[case_name]]
  df_list <- suppressWarnings(tsf:::importDataBatch(cfg$path))
  lb_input <- paste0(cfg$bound_prefix, "_lb_vapro_batch")
  ub_input <- paste0(cfg$bound_prefix, "_ub_vapro_batch")

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

      inputs <- list(nGrid_vapro_batch = 20, error_calc_fct_vapro_batch = "rel. Error")
      inputs[[lb_input]] <- "1"
      inputs[[ub_input]] <- "1e9"
      do.call(session$setInputs, inputs)

      session$setInputs(Start_Vapro_Batch = 1)
      for (i in 1:180) {
        Sys.sleep(0.25)
        session$elapse(300)
        if (vapro_batch_result_created()) break
      }
      out <<- list(created = vapro_batch_result_created(), result = vapro_batch_result())
    }
  )
  list(out = out, n_datasets = length(df_list))
}

test_vapro_batch_server <- function(case_name) {
  res <- run_vapro_batch_case(case_name)
  out <- res$out
  expect_true(out$created, info = paste(case_name, "vapro_batch_result_created"))
  expect_true(is.list(out$result), info = paste(case_name, "result is a list"))
  expect_equal(length(out$result), res$n_datasets, info = paste(case_name, "one entry per dataset"))
  for (r in out$result) {
    expect_true(is.data.frame(r$parameter), info = paste(case_name, "each entry has a parameter data.frame"))
  }
}

test_vapro_batch_server("HG")
test_vapro_batch_server("DBA")
test_vapro_batch_server("IDA")
test_vapro_batch_server("GDA")

task$close()
