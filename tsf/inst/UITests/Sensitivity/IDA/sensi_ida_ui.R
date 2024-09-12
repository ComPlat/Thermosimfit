library(shinytest2)
library(tsf)

# run optimization
pdf("Sensitivity.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "IDA")
app$upload_file(
  upload = "./IDA.csv"
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
app$set_inputs(`IDA-I0_ub` = 1000)
app$set_inputs(`IDA-IHD_lb` = 0)
app$set_inputs(`IDA-IHD_ub` = 10^6)
app$set_inputs(`IDA-ID_lb` = 0)
app$set_inputs(`IDA-ID_ub` = 10^6)
app$set_inputs(`IDA-Seed` = 1234)

app$click("IDA-Start_Opti")
for (i in 1:15) {
  Sys.sleep(1)
  app$get_html("#IDA-output") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("IDA-cancel")
Sys.sleep(5)
res <- app$get_values()$export
app$get_screenshot()

print(res)

# Sensitivity analysis
app$set_inputs(`IDA-ResultPanel` = "Sensitivity analysis")
app$view()
app$click("IDA-Start_Sensi")

while (TRUE) {
  text <- app$get_html("#IDA-output_sense")
  if (grepl("Completed: 100%", text)) {
    break
  }
  print(text)
  Sys.sleep(1)
}
Sys.sleep(5)

file <- app$get_download("IDA-sensi_download")
file.copy(file, "./resultIDA.xlsx", overwrite = TRUE)

Sys.sleep(5)

app$stop()
dev.off()
