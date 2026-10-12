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
    extra_inputs = list(H0 = "5"),
    bound_prefix = "kHD",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e5, 1e5)
  ),
  DBA = list(
    path = system.file("examples", "dba_dye_const.txt", package = "tsf"),
    extra_inputs = list(D0 = "5"),
    bound_prefix = "kHD",
    lb = c(1, 0, 1e2, 1e2), ub = c(1e8, 1e4, 1e8, 1e8)
  ),
  IDA = list(
    path = system.file("examples", "IDA.txt", package = "tsf"),
    extra_inputs = list(H0 = "5", D0 = "6", kHD = "700000"),
    bound_prefix = "kHG",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e5, 1e5)
  ),
  GDA = list(
    path = system.file("examples", "GDA.txt", package = "tsf"),
    extra_inputs = list(H0 = "1.65", G0 = "1.8", kHD = "1.7e7"),
    bound_prefix = "kHG",
    lb = c(1, 0, 0, 0), ub = c(10^9, 1, 1e6, 1e6)
  )
)

# Drives a PSO single-run to completion (source data for Sensitivity), then
# runs Sensitivity off it. Also checks that picking "vapro" as the source -
# with no VAPRO run completed - is refused rather than silently using PSO.
run_sensitivity_case <- function(case_name) {
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

      # source picker refuses "vapro" when no VAPRO run has completed
      session$setInputs(sensi_param_source = "vapro")
      session$setInputs(sens_bounds = 15)
      session$setInputs(Start_Sensi = 1)
      session$elapse(300)
      refused_vapro <<- !sensi_result_created() && !vapro_opti_result_created()

      session$setInputs(sensi_param_source = "pso")
      session$setInputs(Start_Sensi = 1)
      # sensitivity() runs a fixed 1000-sample Sobol design (root-finding
      # loss for ida/gda), routinely ~1 minute real time - give it room.
      for (i in 1:200) {
        Sys.sleep(0.5)
        session$elapse(600)
        if (sensi_result_created()) break
      }
      out <<- list(created = sensi_result_created(), result = sensi_result())
    }
  )
  list(out = out, refused_vapro = refused_vapro)
}

test_sensitivity_server <- function(case_name) {
  res <- run_sensitivity_case(case_name)
  expect_true(res$refused_vapro, info = paste(case_name, "vapro source refused without a VAPRO run"))
  out <- res$out
  expect_true(out$created, info = paste(case_name, "sensi_result_created"))
  expect_true(is.data.frame(out$result), info = paste(case_name, "sensi_result is data.frame"))
  expect_equal(names(out$result), c("original", "bias", "std. error", "min. c.i.", "max. c.i."),
    info = paste(case_name, "sensi_result columns"))
  expect_true(nrow(out$result) > 0, info = paste(case_name, "sensi_result has rows"))
}

test_sensitivity_server("HG")
test_sensitivity_server("DBA")
test_sensitivity_server("IDA")
test_sensitivity_server("GDA")

task$close()
