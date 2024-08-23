setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA")
library(shinytest2)

# run optimization

pdf("Optimization.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "IDA")
app$upload_file(
  upload =
  "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/forKonrad-conc-vs-signal.csv"
)
app$set_window_size(2000, 1000)
app$set_inputs(`IDA-H0` = 1e-6)
app$set_inputs(`IDA-D0` = 1e-6)
app$set_inputs(`IDA-kHD` = 3e6)
ngen <- 10000
app$set_inputs(`IDA-ngen` = ngen)
app$set_inputs(`IDA-Seed` = 1234)

app$click("IDA-Start_Opti")
for(i in 1:30) {
  Sys.sleep(1)
  app$get_html("#IDA-output") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("IDA-cancel")
Sys.sleep(5)
res <- app$get_values()$export
app$get_screenshot()

# check results
stopifnot("clicked cancel?" = res$`IDA-cancel_clicked`)

expected_params <- data.frame(
  kHG = 11161491, I0 = 1e-15,
  IHD = 1011828, ID = 172848.4)
errors <- Map(function(a, b) {
  abs(a - b) / b
}, expected_params, res$`IDA-df_params`)
stopifnot("Comparison of parameters" = all(errors < 0.001))

expected_metrices <- data.frame(
  mse = 0.0001532932,
  rmse = 0.01238116,
  mae = 0.009648227,
  r2 = 0.9927963,
  r2adj = 0.9926493
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

file <- app$get_download("IDA-download")
file.copy(file, "./resultIDA.xlsx", overwrite = TRUE)
dev.off()

# test batch
pdf("Batch.pdf")
app$upload_file(
  upload_batch =
  "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/idaBatch.csv"
)
app$set_inputs(`IDA-NumRepDataset` = 3)
app$click("IDA-Start_Batch")
app$set_inputs(`IDA-ResultPanel` = "Batch processing")
for(i in 1:30) {
  Sys.sleep(1)
  app$get_html("#IDA-output_Batch") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("IDA-cancel_Batch")
Sys.sleep(5)
res <- app$get_values()$export
app$get_screenshot()
dev.off()



# test press cancel sensitivity
pdf("SensitivityPressCancel.pdf")
app$click("IDA-Start_Sensi")
app$set_inputs(`IDA-ResultPanel` = "Sensitivity analysis")
Sys.sleep(5)
app$click("IDA-cancel_sense")
for(i in 1:10) {
  app$get_screenshot()
  Sys.sleep(1)
}
dev.off()

# test sensitivity
pdf("Sensitivity.pdf")
app$click("IDA-Start_Sensi")
app$set_inputs(`IDA-ResultPanel` = "Sensitivity analysis")
while(TRUE) {
  Sys.sleep(3)
  app$get_screenshot()
  res_temp <- app$get_values()$export |> names()
  app$get_html("#IDA-output_sense") |> cat()
  cat("\n")
  if("IDA-sense_plot" %in% res_temp) {
    break
  }
}
res <- app$get_values()$export
file <- app$get_download("IDA-sensi_download")
file.copy(file, "./resultSensitivityIDA.xlsx", overwrite = TRUE)
dev.off()
# clean up
app$stop()



