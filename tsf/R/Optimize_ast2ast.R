pso_loss_fct <- function(case) {
  if (case == "dba_host_const") {
    lossFctHG
  } else if (case == "dba_dye_const") {
    lossFctDBA
  } else if (case == "ida") {
    lossFctIDA
  } else if (case == "gda") {
    lossFctGDA
  }
}

pso_error_code <- function(error_calc_fct) {
  if (identical(error_calc_fct, "rel. Error") || identical(error_calc_fct, "Rel. Error")) {
    1L
  } else if (identical(error_calc_fct, "RMSE")) {
    2L
  } else if (identical(error_calc_fct, "SSE")) {
    3L
  } else if (identical(error_calc_fct, "Huber")) {
    4L
  } else {
    stop("engine = \"ast2ast\" only supports the built-in error functions (\"Rel. Error\", \"RMSE\", \"SSE\", \"Huber\"), not a user-defined function")
  }
}

# Dispatch table mirroring vapro_a2a_spec() (OptimizeVapro_ast2ast.R), for
# the PSO (full-parameter, no NNLS) ast2ast loss functions.
pso_a2a_spec <- function(case) {
  if (case %in% c("dba_dye_const", "dba_host_const")) {
    list(
      types_f = types_f_dba,
      loss_fct = loss_fct_pso_dba_a2a,
      build_add_params = function(df, additionalParameters) {
        build_add_params_dba_a2a(df, additionalParameters, case)
      }
    )
  } else if (case == "ida") {
    list(
      types_f = types_f_ida,
      loss_fct = loss_fct_pso_ida_a2a,
      build_add_params = build_add_params_ida_a2a
    )
  } else if (case == "gda") {
    list(
      types_f = types_f_gda,
      loss_fct = loss_fct_pso_gda_a2a,
      build_add_params = build_add_params_gda_a2a
    )
  } else {
    stop("case has to be one of dba_dye_const, dba_host_const, ida or gda")
  }
}
