setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA")
library(shinytest2)
test_ida <- function() {
  # test optimization
  app <- tsf::runApp(4001)
  app <- AppDriver$new(app)
  app$set_inputs(`Sidebar` = "IDA")
  app$upload_file(
    upload_batch =
      "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/idaBatch.csv"
  )
  app$set_window_size(2000, 1000)
  app$set_inputs(`IDA-IDA_H0` = 1e-6)
  app$set_inputs(`IDA-IDA_D0` = 1e-6)
  app$set_inputs(`IDA-IDA_kHD` = 3e6)
  ngen <- 20
  app$set_inputs(`IDA-IDA_ngen` = ngen)

  app$set_inputs(`IDA-ResultPanel` = "Batch processing")
  app$click("IDA-IDA_Start_Batch")
  app$view()
  Sys.sleep(100)
  app$stop()
}


test_ida()
