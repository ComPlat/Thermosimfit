setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/DBA_const_host")
library(shinytest2)
test_hg <- function() {
  # test optimization
  app <- tsf::runApp(4001)
  app <- AppDriver$new(app)
  app$set_inputs(`Sidebar` = "HG")
  app$upload_file(
    upload = "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/DBA_const_host/dba_dye_const.txt"
  )
  app$set_window_size(2000, 1000)
  app$set_inputs(`HG-HG_H0` = 0.000151)
  ngen <- 300
  app$set_inputs(`HG-HG_ngen` = ngen)
  app$set_inputs(`HG-Seed` = 1234)
  app$click("HG-HG_Start_Opti")
  app$click("HG-HG_status") # requested after optimization
  app$click("HG-HG_cancel") # requested after optimization
  res <- app$get_values()$export

  stopifnot("clicked cancel?" = res$`HG-cancel_clicked`)

  expected_params <- data.frame(
    kHD = 31988684, I0 = 185.5784,
    IHD = 4738552, ID = 1556450
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_params, res$`HG-df_params`)
  stopifnot("Comparison of parameters" = all(errors < 0.001))

  expected_metrices <- data.frame(
    mse = 2981.826,
    rmse = 54.6061,
    mae = 45.52409,
    r2 = 0.986781,
    r2adj = 0.9863252
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_metrices, res$`HG-df_metrices`)
  stopifnot("Comparison of metrices" = all(errors < 0.001))

  stopifnot(
    "Correct number of iter" = tsf:::extract_iter(res$`HG-status1`) == ngen
  )
  stopifnot(
    "Initialisation?" =
      res$`HG-status2` == "Initialisation"
  )

  file <- app$get_download("HG-HG_download")
  file.copy(file, "./resultHG.xlsx", overwrite = TRUE)

  app$wait_for_idle()
  pdf("HG_dye_const.pdf")
  app$get_screenshot()

  # test sensitivity

  app$set_inputs(`HG-ResultPanel` = "Sensitivity analysis")
  app$click("HG-HG_Start_Sensi")
  app$click("HG-HG_status_sense")

  res <- app$get_values()$export
  app$get_screenshot()
  print(res)
  dev.off()

  file <- app$get_download("HG-HG_sensi_download")
  file.copy(file, "./resultSensitivityHG.xlsx", overwrite = TRUE)
  app$wait_for_idle()

  app$stop()
}


test_hg()
