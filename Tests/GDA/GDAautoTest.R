setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/GDA")
library(shinytest2)
test_gda <- function() {
  # test optimization
  app <- tsf::runApp(4001)
  app <- AppDriver$new(app)
  app$set_inputs(`Sidebar` = "GDA")
  app$upload_file(
    upload =
      "data_GDA_Estradiol_CB7_BE.txt"
  )
  app$set_window_size(2000, 1000)
  app$set_inputs(`GDA-GDA_H0` = 1.65E-06)
  app$set_inputs(`GDA-GDA_G0` = 1.32E-06)
  app$set_inputs(`GDA-GDA_kHD` = 1.7E07)
  ngen <- 300
  app$set_inputs(`GDA-GDA_ngen` = ngen)
  app$set_inputs(`GDA-Seed` = 1234)
  app$set_inputs(`GDA-GDA_IHD_ub` = 10^10)
  app$click("GDA-GDA_Start_Opti")
  app$click("GDA-GDA_status") # requested after optimization
  app$click("GDA-GDA_cancel") # requested after optimization
  res <- app$get_values()$export
  print(res)
  stopifnot("clicked cancel?" = res$`GDA-cancel_clicked`)

  expected_params <- data.frame(
    kHG = 1248690, I0 = 243.992,
    IHD = 3317294882, ID = 1e-15
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_params, res$`GDA-df_params`)
  stopifnot("Comparison of parameters" = all(errors < 0.001))

  expected_metrices <- data.frame(
    mse = 4525.197,
    rmse = 67.26959,
    mae = 47.95483,
    r2 = 0.9988945,
    r2adj = 0.9988156
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_metrices, res$`GDA-df_metrices`)
  stopifnot("Comparison of metrices" = all(errors < 0.001))

  stopifnot(
    "Correct number of iter" = tsf:::extract_iter(res$`GDA-status1`) == ngen
  )
  stopifnot(
    "Initialisation?" =
      res$`GDA-status2` == "Initialisation"
  )

  file <- app$get_download("GDA-GDA_download")
  file.copy(file, "./resultGDA.xlsx", overwrite = TRUE)

  app$wait_for_idle()
  pdf("GDA.pdf")
  app$get_screenshot()

  # test sensitivity
  app$set_inputs(`GDA-ResultPanel` = "Sensitivity analysis")
  app$click("GDA-GDA_Start_Sensi")
  app$click("GDA-GDA_status_sense")

  res <- app$get_values()$export
  app$get_screenshot()
  print(res)
  dev.off()

  file <- app$get_download("GDA-GDA_sensi_download")
  file.copy(file, "./resultSensitivityGDA.xlsx", overwrite = TRUE)
  app$wait_for_idle()

  app$stop()
}


test_gda()
