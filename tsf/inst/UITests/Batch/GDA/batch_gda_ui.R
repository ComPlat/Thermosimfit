library(shinytest2)
library(tsf)

# run optimization
pdf("Batch.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "GDA")
app$upload_file(
  upload_batch =
    "Batch.txt"
)
app$set_window_size(2000, 1000)
app$set_inputs(`GDA-H0` = 1.65E-06)
app$set_inputs(`GDA-G0` = 1.32E-06)
app$set_inputs(`GDA-kHD` = 1.7E07)
ngen <- 1000
app$set_inputs(`GDA-ngen` = ngen)
app$set_inputs(`GDA-kHG_lb` = 10)
app$set_inputs(`GDA-kHG_ub` = 10^10)
app$set_inputs(`GDA-I0_lb` = 0)
app$set_inputs(`GDA-I0_ub` = 10^10)
app$set_inputs(`GDA-IHD_lb` = 0)
app$set_inputs(`GDA-IHD_ub` = 10^10)
app$set_inputs(`GDA-ID_lb` = 0)
app$set_inputs(`GDA-ID_ub` = 10^10)
app$set_inputs(`GDA-NumRepDataset` = 1)
app$set_inputs(`GDA-NumCores` = 3)

app$view() # watch live app

app$set_inputs(`GDA-ResultPanel` = "Batch processing")
app$click("GDA-Start_Batch")
for (i in 1:30) {
  Sys.sleep(1)
  app$get_html("#GDA-output_Batch") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("GDA-cancel_Batch")
Sys.sleep(10)
app$get_screenshot()
file <- app$get_download("GDA-batch_download")
file.copy(file, "./resultGDA.xlsx", overwrite = TRUE)

Sys.sleep(20)
app$stop()
dev.off()
