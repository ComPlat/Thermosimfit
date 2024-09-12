library(shinytest2)
library(tsf)

# run optimization
pdf("Batch.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "HG")
app$upload_file(
  upload_batch =
    "batch.txt"
)
app$set_window_size(2000, 1000)
app$set_inputs(`HG-H0` = 0.000151)
ngen <- 1000
app$set_inputs(`HG-ngen` = ngen)
app$set_inputs(`HG-kHD_lb` = 10)
app$set_inputs(`HG-kHD_ub` = 10^8)
app$set_inputs(`HG-I0_lb` = 0)
app$set_inputs(`HG-I0_ub` = 10^8)
app$set_inputs(`HG-IHD_lb` = 0)
app$set_inputs(`HG-IHD_ub` = 10^8)
app$set_inputs(`HG-ID_lb` = 0)
app$set_inputs(`HG-ID_ub` = 10^8)
app$set_inputs(`HG-NumRepDataset` = 1)
app$set_inputs(`HG-NumCores` = 3)

app$view() # watch live app

app$set_inputs(`HG-ResultPanel` = "Batch processing")
app$click("HG-Start_Batch")
for (i in 1:30) {
  Sys.sleep(1)
  app$get_html("#HG-output_Batch") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("HG-cancel_Batch")
Sys.sleep(10)
app$get_screenshot()
file <- app$get_download("HG-batch_download")
file.copy(file, "./resultHG.xlsx", overwrite = TRUE)

Sys.sleep(20)
app$stop()
dev.off()
