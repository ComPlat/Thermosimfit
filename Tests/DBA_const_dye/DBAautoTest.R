library(shinytest2)
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/DBA_const_dye")
test_dba <- function() {
  # test optimization
  app <- tsf::runApp(4001)
  app <- AppDriver$new(app)
  app$set_inputs(`Sidebar` = "DBA")
  app$upload_file(
    upload = "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/DBA_const_dye/dba_dye_const.txt"
  )
  app$set_window_size(2000, 1000)
  app$set_inputs(`DBA-DBA_D0` = 0.000151)
  ngen <- 300
  app$set_inputs(`DBA-DBA_ngen` = ngen)
  app$set_inputs(`DBA-Seed` = 1234)
  app$click("DBA-DBA_Start_Opti")
  app$click("DBA-DBA_status") # requested after optimization
  app$click("DBA-DBA_cancel") # requested after optimization
  res <- app$get_values()$export
  stopifnot("clicked cancel?" = res$`DBA-cancel_clicked`)

  expected_params <- data.frame(
    kHD = 2309.988, I0 = 1e-15,
    IHD = 19772448, ID = 931478.9
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_params, res$`DBA-df_params`)
  stopifnot("Comparison of parameters" = all(errors < 0.001))

  expected_metrices <- data.frame(
    mse = 2417.408,
    rmse = 49.16714,
    mae = 43.35119,
    r2 = 0.99999273,
    r2adj = 0.9999248
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_metrices, res$`DBA-df_metrices`)
  stopifnot("Comparison of metrices" = all(errors < 0.001))

  stopifnot(
    "Correct number of iter" = tsf:::extract_iter(res$`DBA-status1`) == ngen
  )
  stopifnot(
    "Initialisation?" =
      res$`DBA-status2` == "Initialisation"
  )

  file <- app$get_download("DBA-DBA_download")
  file.copy(file, "./resultDBA.xlsx", overwrite = TRUE)

  app$wait_for_idle()
  pdf("DBA_dye_const.pdf")
  app$get_screenshot()

  # test sensitivity
  app$set_inputs(`DBA-ResultPanel` = "Sensitivity analysis")
  app$click("DBA-DBA_Start_Sensi")
  app$click("DBA-DBA_status_sense")

  res <- app$get_values()$export
  app$get_screenshot()
  print(res)
  dev.off()

  file <- app$get_download("DBA-DBA_sensi_download")
  file.copy(file, "./resultSensitivityDBA.xlsx", overwrite = TRUE)
  app$wait_for_idle()

  app$stop()
}


test_dba()
