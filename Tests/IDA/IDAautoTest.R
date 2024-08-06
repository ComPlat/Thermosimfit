setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA")
library(shinytest2)
test_ida <- function() {
  # test optimization
  app <- tsf::runApp(4001)
  app <- AppDriver$new(app)
  app$set_inputs(`Sidebar` = "IDA")
  app$upload_file(
    upload =
      "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/forKonrad-conc-vs-signal.csv"
  )
  app$set_window_size(2000, 1000)
  app$set_inputs(`IDA-IDA_H0` = 1e-6)
  app$set_inputs(`IDA-IDA_D0` = 1e-6)
  app$set_inputs(`IDA-IDA_kHD` = 3e6)
  ngen <- 300
  app$set_inputs(`IDA-IDA_ngen` = ngen)
  app$set_inputs(`IDA-Seed` = 1234)
  app$click("IDA-IDA_Start_Opti")
  app$click("IDA-IDA_status") # requested after optimization
  app$click("IDA-IDA_cancel") # requested after optimization
  res <- app$get_values()$export

  stopifnot("clicked cancel?" = res$`IDA-cancel_clicked`)

  expected_params <- data.frame(
    kHG = 15848501, I0 = 1e-15,
    IHD = 969067.3, ID = 190498.9
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_params, res$`IDA-df_params`)
  stopifnot("Comparison of parameters" = all(errors < 0.001))

  expected_metrices <- data.frame(
    mse = 4.193714e-05,
    rmse = 0.006475889,
    mae = 0.00406907,
    r2 = 0.9988749,
    r2adj = 0.998852
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_metrices, res$`IDA-df_metrices`)
  stopifnot("Comparison of metrices" = all(errors < 0.001))

  stopifnot(
    "Correct number of iter" = tsf:::extract_iter(res$`IDA-status1`) == ngen
  )
  stopifnot(
    "Initialisation?" =
      res$`IDA-status2` == "Initialisation"
  )

  file <- app$get_download("IDA-IDA_download")
  file.copy(file, "./resultIDA.xlsx", overwrite = TRUE)

  app$wait_for_idle()
  pdf("IDA.pdf")
  app$get_screenshot()

  # test sensitivity
  app$set_inputs(`IDA-ResultPanel` = "Sensitivity analysis")
  app$click("IDA-IDA_Start_Sensi")
  app$click("IDA-IDA_status_sense")

  res <- app$get_values()$export
  app$get_screenshot()
  print(res)
  dev.off()

  file <- app$get_download("IDA-IDA_sensi_download")
  file.copy(file, "./resultSensitivityIDA.xlsx", overwrite = TRUE)
  app$wait_for_idle()

  app$stop()
}


test_ida()
