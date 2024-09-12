library(shinytest2)
library(tsf)

# run optimization
pdf("Optimization.pdf")
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

app$view() # wathc live app
app$click("IDA-Start_Opti")
for (i in 1:30) {
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

# check results
stopifnot("clicked cancel?" = res$`IDA-cancel_clicked`)

expected_params <- data.frame(
  kHG = 18274364, I0 = 0.1791767,
  IHD = 835882.6, ID = 8420.621
)
expected_metrices <- data.frame(
  mse = 3.436589e-05,
  rmse = 0.005862243,
  mae = 0.005005597,
  r2 = 0.999833,
  r2adj = 0.9998296
)
stopifnot(
  "Correct number of iter" = tsf:::extract_iter(res$`IDA-status1`) == ngen
)

file <- app$get_download("IDA-download")
file.copy(file, "./resultIDA.xlsx", overwrite = TRUE)


# Test negative boundaries (should throw error)
app$set_inputs(`IDA-kHG_lb` = -10)
app$set_inputs(`IDA-kHG_ub` = 1e08)
app$click("IDA-Start_Opti")
Sys.sleep(30)
app$get_screenshot()

Sys.sleep(4)
app$stop()
dev.off()
