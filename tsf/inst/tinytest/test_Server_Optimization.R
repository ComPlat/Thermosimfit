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
    model = "dba_host_const",
    path = system.file("examples", "dba_dye_const.txt", package = "tsf"),
    extra_inputs = list(H0 = "5"),
    bound_prefix = "kHD",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e5, 1e5)
  ),
  DBA = list(
    model = "dba_dye_const",
    path = system.file("examples", "dba_dye_const.txt", package = "tsf"),
    extra_inputs = list(D0 = "5"),
    bound_prefix = "kHD",
    lb = c(1, 0, 1e2, 1e2), ub = c(1e8, 1e4, 1e8, 1e8)
  ),
  IDA = list(
    model = "ida",
    path = system.file("examples", "IDA.txt", package = "tsf"),
    extra_inputs = list(H0 = "5", D0 = "6", kHD = "700000"),
    bound_prefix = "kHG",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e5, 1e5)
  ),
  GDA = list(
    model = "gda",
    path = system.file("examples", "GDA.txt", package = "tsf"),
    extra_inputs = list(H0 = "1.65", G0 = "1.8", kHD = "1.7e7"),
    bound_prefix = "kHG",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e6, 1e6)
  )
)

run_optimization_case <- function(case_name) {
  cfg <- cases[[case_name]]
  df_reactive <- make_df_reactive(cfg$path)
  lb_input <- paste0(cfg$bound_prefix, "_lb")
  ub_input <- paste0(cfg$bound_prefix, "_ub")

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
      session$setInputs(I0_lb = "0", I0_ub = "1e6")
      session$setInputs(IHD_lb = "0", IHD_ub = "1e6")
      session$setInputs(ID_lb = "0", ID_ub = "1e6")
      session$setInputs(Confirm = 1)

      inputs <- list(npop = 10, ngen = 15, threshold = 0.00001, topology = "random",
        error_calc_fct = "rel. Error", Seed = 1234)
      inputs[[lb_input]] <- as.character(cfg$lb[1])
      inputs[[ub_input]] <- as.character(cfg$ub[1])
      do.call(session$setInputs, inputs)

      session$setInputs(Start_Opti = 1)
      for (i in 1:120) {
        Sys.sleep(0.25)
        session$elapse(300)
        if (opti_result_created()) break
      }
      out <<- list(created = opti_result_created(), result = opti_result())
    }
  )
  out
}

test_opti_server <- function(case_name) {
  res <- run_optimization_case(case_name)
  expect_true(res$created, info = paste(case_name, "opti_result_created"))
  expect_false(inherits(res$result, "ErrorClass"), info = paste(case_name, "not an ErrorClass"))
  expect_true(is.data.frame(res$result$parameter), info = paste(case_name, "parameter is data.frame"))
  expect_equal(nrow(res$result$parameter), 1L, info = paste(case_name, "single run: one row"))
  expect_true(is.data.frame(res$result$metrices), info = paste(case_name, "metrices is data.frame"))
  expect_true("R2" %in% names(res$result$metrices), info = paste(case_name, "metrices has R2"))
}

test_opti_server("HG")
test_opti_server("DBA")
test_opti_server("IDA")
test_opti_server("GDA")

task$close()
