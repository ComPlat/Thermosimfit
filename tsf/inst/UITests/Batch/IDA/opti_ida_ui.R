library(shinytest2)
library(tsf)

# run optimization
pdf("Batch.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "IDA")
app$upload_file(
  upload_batch =
    "/home/konrad/Documents/Thermosimfit/tsf/inst/UITests/Batch/IDA/idaBatch.csv"
)
app$set_window_size(2000, 1000)
app$set_inputs(`IDA-H0` = 1e-6)
app$set_inputs(`IDA-D0` = 1e-6)
app$set_inputs(`IDA-kHD` = 3e6)
ngen <- 1000
app$set_inputs(`IDA-ngen` = ngen)
app$set_inputs(`IDA-kHG_lb` = 10)
app$set_inputs(`IDA-kHG_ub` = 10^8)
app$set_inputs(`IDA-I0_lb` = 0)
app$set_inputs(`IDA-I0_ub` = 10^8)
app$set_inputs(`IDA-IHD_lb` = 0)
app$set_inputs(`IDA-IHD_ub` = 10^8)
app$set_inputs(`IDA-ID_lb` = 0)
app$set_inputs(`IDA-ID_ub` = 10^8)
app$set_inputs(`IDA-NumRepDataset` = 1)
app$set_inputs(`IDA-NumCores` = 3)

app$view() # watch live app

app$set_inputs(`IDA-ResultPanel` = "Batch processing")
app$click("IDA-Start_Batch")
for (i in 1:30) {
  Sys.sleep(1)
  app$get_html("#IDA-output_Batch") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("IDA-cancel_Batch")
Sys.sleep(10)
app$get_screenshot()
file <- app$get_download("IDA-batch_download")
file.copy(file, "./resultIDA.xlsx", overwrite = TRUE)

Sys.sleep(20)
app$stop()
dev.off()
