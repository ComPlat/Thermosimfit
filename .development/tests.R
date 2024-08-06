setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/.development")
library(shinytest2)

test_dba <- function() {
  # test optimization
  app <- tsf::runApp(4001)
  app <- AppDriver$new(app)
  app$set_inputs(`Sidebar` = "DBA")
  app$upload_file(upload = "../Tests/DBA_const_dye/dba_dye_const.txt")
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
  app$set_inputs(`ResultPanel` = "Sensitivity analysis")
  app$click("DBA-DBA_Start_Sensi")
  app$click("DBA-DBA_cancel_sense")
  app$click("DBA-DBA_status_sense")

  res <- app$get_values()$export
  app$get_screenshot()
  print(res)
  dev.off()

  file <- app$get_download("DBA_sensi_download")
  file.copy(file, "./resultSensitivityDBA.xlsx", overwrite = TRUE)
  app$wait_for_idle()

  app$stop()
}


test_dba()
print("test")
stop()

test_hg <- function() {
  app <- tsf::runApp(4001)
  app <- AppDriver$new(app)
  app$set_inputs(`Sidebar` = "HG")
  app$upload_file(upload = "../Tests/DBA_const_dye/dba_dye_const.txt")
  app$set_window_size(2000, 1000)
  app$set_inputs(`HG-HG_D0` = 0.000151)
  ngen <- 300
  app$set_inputs(`HG-HG_ngen` = ngen)
  app$set_inputs(`HG-Seed` = 1234)
  app$click("HG-HG_Start_Opti")
  app$click("HG-HG_status") # requested after optimization
  app$click("HG-HG_cancel") # requested after optimization
  res <- app$get_values()$export

  stopifnot("clicked cancel?" = res[[1]])

  expected_params <- data.frame(
    kHD = 2309.988, I0 = 1e-15,
    IHD = 19772448, ID = 931478.9
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_params, res[[3]])
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
  }, expected_metrices, res[[2]])
  stopifnot("Comparison of metrices" = all(errors < 0.001))

  stopifnot("Correct number of iter" = tsf:::extract_iter(res[[4]]) == ngen)
  stopifnot(
    "Initialisation?" =
      res[[5]] == "Initialisation"
  )

  file <- app$get_download("HG-HG_download")
  file.copy(file, "./resultHG.xlsx", overwrite = TRUE)
  app$wait_for_idle()
  pdf("HG_dye_const.pdf")
  app$get_screenshot()
  dev.off()
  app$stop()
}


test_ida <- function() {
  app <- tsf::runApp(4001)
  app <- AppDriver$new(app)
  app$set_inputs(`Sidebar` = "IDA")

  app$upload_file(upload = "../Tests/IDA/forKonrad-conc-vs-signal.csv")
  app$set_window_size(2000, 1000)
  app$set_inputs(`IDA-IDA_D0` = 1e-06)
  app$set_inputs(`IDA-IDA_H0` = 1e-06)
  app$set_inputs(`IDA-IDA_kHD` = 3e06)
  ngen <- 300
  app$set_inputs(`IDA-IDA_ngen` = ngen)
  app$set_inputs(`IDA-Seed` = 1234)

  app$click("IDA-IDA_Start_Opti")
  app$click("IDA-IDA_status") # requested after optimization
  app$click("IDA-IDA_cancel") # requested after optimization
  Sys.sleep(1.5)
  res <- app$get_values()$export
  print(res)
  str(res)
  expected_params <- data.frame(
    kHG = 15848501, I0 = 1e-15,
    IHD = 969067.3, ID = 190498.9
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_params, res[[2]])
  stopifnot(all(errors < 0.001))

  expected_metrices <- data.frame(
    mse = 4.193714e-05,
    rmse = 0.006475889,
    mae = 0.00406907,
    r2 = 0.9988749,
    r2adj = 0.998852
  )
  errors <- Map(function(a, b) {
    abs(a - b) / b
  }, expected_metrices, res[[1]])
  stopifnot(all(errors < 0.001))

  file <- app$get_download("IDA-IDA_download")
  file.copy(file, ".", overwrite = TRUE)

  app$stop()
}

test_ida()
