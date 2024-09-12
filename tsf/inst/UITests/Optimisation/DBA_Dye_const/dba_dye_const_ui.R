library(shinytest2)
library(tsf)

# run optimization
pdf("Optimization.pdf")
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

app$view()

app$click("DBA-Start_Opti")
for (i in 1:15) {
  Sys.sleep(1)
  app$get_html("#DBA-output") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("DBA-cancel")
Sys.sleep(5)
res <- app$get_values()$export

print(res)

app$get_screenshot()
file <- app$get_download("DBA-download")
file.copy(file, "./resultDBA.xlsx", overwrite = TRUE)
app$stop()
dev.off()
