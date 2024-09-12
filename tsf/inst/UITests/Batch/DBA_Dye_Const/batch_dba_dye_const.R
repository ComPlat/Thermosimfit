library(shinytest2)
library(tsf)

# run optimization
pdf("Batch.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "DBA")
app$upload_file(
  upload_batch =
    "batch.txt"
)
app$set_window_size(2000, 1000)
app$set_inputs(`DBA-D0` = 0.000151)
ngen <- 1000
app$set_inputs(`DBA-ngen` = ngen)
app$set_inputs(`DBA-kHD_lb` = 10)
app$set_inputs(`DBA-kHD_ub` = 10^8)
app$set_inputs(`DBA-I0_lb` = 0)
app$set_inputs(`DBA-I0_ub` = 10^8)
app$set_inputs(`DBA-IHD_lb` = 0)
app$set_inputs(`DBA-IHD_ub` = 10^8)
app$set_inputs(`DBA-ID_lb` = 0)
app$set_inputs(`DBA-ID_ub` = 10^8)
app$set_inputs(`DBA-NumRepDataset` = 1)
app$set_inputs(`DBA-NumCores` = 3)

app$view() # watch live app

app$set_inputs(`DBA-ResultPanel` = "Batch processing")
app$click("DBA-Start_Batch")
for (i in 1:30) {
  Sys.sleep(1)
  app$get_html("#DBA-output_Batch") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("DBA-cancel_Batch")
Sys.sleep(10)
app$get_screenshot()
file <- app$get_download("DBA-batch_download")
file.copy(file, "./resultDBA.xlsx", overwrite = TRUE)

Sys.sleep(20)
app$stop()
dev.off()
