library(shinytest2)
library(tsf)

# run optimization
pdf("Optimization.pdf")
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

app$view()

app$click("GDA-Start_Opti")
for (i in 1:30) {
  Sys.sleep(1)
  app$get_html("#GDA-output") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("GDA-cancel")
Sys.sleep(5)
res <- app$get_values()$export
app$get_screenshot()

file <- app$get_download("GDA-download")
file.copy(file, "./resultGDA.xlsx", overwrite = TRUE)

print(res)
app$stop()
dev.off()
