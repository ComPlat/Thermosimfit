setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA")
library(shinytest2)

app <- tsf::runApp(4001)
app <- AppDriver$new(app)
# app$set_inputs(`Sidebar` = "IDA")
# app$set_inputs(`IDA-ResultPanel` = "Batch processing")
app$get_values()$output|> str()
