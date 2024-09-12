library(shinytest2)
library(tsf)

# run optimization
pdf("Sensitivity.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "DBA")
app$upload_file(
  upload = "./DBA.txt"
)
app$set_window_size(2000, 1000)
app$set_inputs(`DBA-D0` = 103 * 10^-6)
ngen <- 1000
app$set_inputs(`DBA-ngen` = ngen)
app$set_inputs(`DBA-kHD_lb` = 10)
app$set_inputs(`DBA-kHD_ub` = 10^8)
app$set_inputs(`DBA-I0_lb` = 0)
app$set_inputs(`DBA-I0_ub` = 1000)
app$set_inputs(`DBA-IHD_lb` = 0)
app$set_inputs(`DBA-IHD_ub` = 10^8)
app$set_inputs(`DBA-ID_lb` = 0)
app$set_inputs(`DBA-ID_ub` = 10^8)
app$set_inputs(`DBA-Seed` = 1234)

app$click("DBA-Start_Opti")
for (i in 1:10) {
  Sys.sleep(1)
  app$get_html("#DBA-output") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("DBA-cancel")
Sys.sleep(5)
res <- app$get_values()$export
app$get_screenshot()

print(res)

# Sensitivity analysis
app$set_inputs(`DBA-ResultPanel` = "Sensitivity analysis")
app$view() # TODO. same error as for gda
app$click("DBA-Start_Sensi")
app$set_inputs(`DBA-ResultPanel` = "Sensitivity analysis")

while (TRUE) {
  text <- app$get_html("#DBA-output_sense")
  str(text)
  if (grepl("Completed: 100%", text)) {
    break
  }
  print(text)
  Sys.sleep(1)
}
Sys.sleep(5)

file <- app$get_download("DBA-sensi_download")
file.copy(file, "./resultDBA.xlsx", overwrite = TRUE)

Sys.sleep(5)

app$stop()
dev.off()
