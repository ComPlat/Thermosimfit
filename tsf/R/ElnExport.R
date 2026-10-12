ELN_UNCERTAINTY_LABELS <- c(
  pso_batch = "PSO batch uncertainty",
  pso_direct = "PSO direct bootstrap",
  vapro_bootstrap = "VAPRO bootstrap"
)

eln_result_list <- function(model, uncertainty, uncertainty_method) {
  ka <- uncertainty[grepl("^Ka", uncertainty$param), , drop = FALSE]
  if (nrow(ka) != 1) {
    return(ErrorClass$new("Could not find Ka in the uncertainty result"))
  }
  list(
    analysis = "Binding Isotherm",
    model = model,
    Ka = ka$estimate,
    ka_uncertainty = (ka$upper - ka$lower) / 2,
    ka_lower = ka$lower,
    ka_upper = ka$upper,
    uncertainty_method = uncertainty_method
  )
}

# write_xlsx: function(file) writing the normal Thermosimfit xlsx output
eln_result_zip <- function(model, write_xlsx, uncertainty, uncertainty_method) {
  res <- eln_result_list(model, uncertainty, uncertainty_method)
  if (is(res, "ErrorClass")) {
    return(res)
  }
  dir <- tempfile("eln_result")
  dir.create(dir)
  json_file <- file.path(dir, "result.json")
  xlsx_file <- file.path(dir, "result.xlsx")
  jsonlite::write_json(res, json_file, auto_unbox = TRUE, digits = NA, pretty = TRUE)
  e <- try(write_xlsx(xlsx_file), silent = TRUE)
  if (inherits(e, "try-error") || !file.exists(xlsx_file)) {
    unlink(dir, recursive = TRUE)
    return(ErrorClass$new("Could not create the result xlsx"))
  }
  zip_file <- tempfile(fileext = ".zip")
  # -j: store the files without the tempdir path
  utils::zip(zip_file, c(json_file, xlsx_file), flags = "-j")
  unlink(dir, recursive = TRUE)
  if (!file.exists(zip_file)) {
    return(ErrorClass$new("Could not create the result zip"))
  }
  zip_file
}

eln_send_result <- function(session, model, write_xlsx, uncertainty, uncertainty_method) {
  zip_file <- eln_result_zip(model, write_xlsx, uncertainty, uncertainty_method)
  if (is(zip_file, "ErrorClass")) {
    print_noti(zip_file$message, duration = 0)
    return(invisible(FALSE))
  }
  on.exit(unlink(zip_file))
  getExportedValue("COMELN", "upload")(session, zip_file, new_name = "result.zip")
}
