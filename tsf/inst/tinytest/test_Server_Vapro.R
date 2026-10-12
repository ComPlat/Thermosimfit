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

cases <- list(
  HG = list(
    path = system.file("examples", "dba_dye_const.txt", package = "tsf"),
    extra_inputs = list(H0_vapro = "5"),
    bound_prefix = "kHD"
  ),
  DBA = list(
    path = system.file("examples", "dba_dye_const.txt", package = "tsf"),
    extra_inputs = list(D0_vapro = "5"),
    bound_prefix = "kHD"
  ),
  IDA = list(
    path = system.file("examples", "IDA.txt", package = "tsf"),
    extra_inputs = list(H0_vapro = "5", D0_vapro = "6", kHD_vapro = "700000"),
    bound_prefix = "kHG"
  ),
  GDA = list(
    path = system.file("examples", "GDA.txt", package = "tsf"),
    extra_inputs = list(H0_vapro = "1.65", G0_vapro = "1.8", kHD_vapro = "1.7e7"),
    bound_prefix = "kHG"
  )
)

run_vapro_case <- function(case_name) {
  cfg <- cases[[case_name]]
  df_reactive <- make_df_reactive(cfg$path)
  lb_input <- paste0(cfg$bound_prefix, "_lb_vapro")
  ub_input <- paste0(cfg$bound_prefix, "_ub_vapro")

  out <- NULL
  shiny::testServer(
    tsf:::server_opti_sensi_batch,
    args = list(
      id = case_name, df_reactive = df_reactive,
      df_list_reactive = shiny::reactiveValues(data_frames = NULL),
      nclicks = shiny::reactiveVal(0), task = task, progress_file = progress_file
    ),
    {
      do.call(session$setInputs, cfg$extra_inputs)

      inputs <- list(nGrid = 20, error_calc_fct_vapro = "rel. Error")
      inputs[[lb_input]] <- "1"
      inputs[[ub_input]] <- "1e9"
      do.call(session$setInputs, inputs)

      session$setInputs(Start_Vapro_Opti = 1)
      for (i in 1:120) {
        Sys.sleep(0.25)
        session$elapse(300)
        if (vapro_opti_result_created()) break
      }
      out <<- list(created = vapro_opti_result_created(), result = vapro_opti_result())
    }
  )
  out
}

test_vapro_server <- function(case_name) {
  res <- run_vapro_case(case_name)
  expect_true(res$created, info = paste(case_name, "vapro_opti_result_created"))
  expect_false(inherits(res$result, "ErrorClass"), info = paste(case_name, "not an ErrorClass"))
  expect_true(is.data.frame(res$result$parameter), info = paste(case_name, "parameter is data.frame"))
  expect_equal(nrow(res$result$parameter), 1L, info = paste(case_name, "single run: one row"))
  expect_true(is.data.frame(res$result$metrices), info = paste(case_name, "metrices is data.frame"))
  expect_true("R2" %in% names(res$result$metrices), info = paste(case_name, "metrices has R2"))
}

test_vapro_server("HG")
test_vapro_server("DBA")
test_vapro_server("IDA")
test_vapro_server("GDA")

task$close()
