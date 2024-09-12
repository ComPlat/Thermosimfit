library(shinytest2)
library(tsf)

# run optimization
pdf("Optimization.pdf")
app <- tsf::runApp(4001)
app <- AppDriver$new(app)
app$set_inputs(`Sidebar` = "HG")
app$upload_file(
  upload = "./HG.txt"
)
app$set_window_size(2000, 1000)
app$set_inputs(`HG-D0` = 103 * 10^-6)
ngen <- 1000
app$set_inputs(`HG-ngen` = ngen)
app$set_inputs(`HG-kHD_lb` = 10)
app$set_inputs(`HG-kHD_ub` = 10^8)
app$set_inputs(`HG-I0_lb` = 0)
app$set_inputs(`HG-I0_ub` = 1000)
app$set_inputs(`HG-IHD_lb` = 0)
app$set_inputs(`HG-IHD_ub` = 10^8)
app$set_inputs(`HG-ID_lb` = 0)
app$set_inputs(`HG-ID_ub` = 10^8)
app$set_inputs(`HG-Seed` = 1234)

app$view()

app$click("HG-Start_Opti")
for (i in 1:15) {
  Sys.sleep(1)
  app$get_html("#HG-output") |> cat()
  cat("\n")
  app$get_screenshot()
}
app$click("HG-cancel")
Sys.sleep(5)
res <- app$get_values()$export

print(res)

app$get_screenshot()
file <- app$get_download("HG-download")
file.copy(file, "./resultHG.xlsx", overwrite = TRUE)
app$stop()
dev.off()
