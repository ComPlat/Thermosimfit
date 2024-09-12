library(shinytest2)
library(tsf)

# run optimization
pdf("Sensitivity.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "GDA")
app$upload_file(
  upload = "./GDA.txt"
)
app$set_window_size(2000, 1000)
app$set_inputs(`GDA-H0` = 103 * 10^-6)
app$set_inputs(`GDA-G0` = 1050 * 10^-6)
app$set_inputs(`GDA-kHD` = 1.81e02)
ngen <- 1000
app$set_inputs(`GDA-ngen` = ngen)
app$set_inputs(`GDA-kHG_lb` = 10)
app$set_inputs(`GDA-kHG_ub` = 10^8)
app$set_inputs(`GDA-I0_lb` = 0)
app$set_inputs(`GDA-I0_ub` = 1000)
app$set_inputs(`GDA-IHD_lb` = 0)
app$set_inputs(`GDA-IHD_ub` = 10^8)
app$set_inputs(`GDA-ID_lb` = 0)
app$set_inputs(`GDA-ID_ub` = 10^8)
app$set_inputs(`GDA-Seed` = 1234)

app$click("GDA-Start_Opti")
for (i in 1:10) {
  Sys.sleep(1)
  app$get_html("#GDA-output") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("GDA-cancel")
Sys.sleep(5)
res <- app$get_values()$export
app$get_screenshot()

print(res)

# Sensitivity analysis
app$set_inputs(`GDA-ResultPanel` = "Sensitivity analysis")
# app$view() # TODO: the run was not successfull when calling view. Check why
app$click("GDA-Start_Sensi")
app$set_inputs(`GDA-ResultPanel` = "Sensitivity analysis")

while (TRUE) {
  text <- app$get_html("#GDA-output_sense")
  if (grepl("Completed: 100%", text)) {
    break
  }
  print(text)
  Sys.sleep(1)
}
Sys.sleep(5)

file <- app$get_download("GDA-sensi_download")
file.copy(file, "./resultGDA.xlsx", overwrite = TRUE)

Sys.sleep(5)

app$stop()
dev.off()
