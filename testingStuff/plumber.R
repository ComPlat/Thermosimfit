# plumber.R
# Install the plumber package if you haven't already: install.packages("plumber")
library(plumber)

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @apiTitle Plumber API Example
#* @get /start
function(req) {
  enteredText <- req$GET$text
  print(paste("Entered Text:", enteredText))
  
  result <- list()
  for (i in 1:15) {
    result <- c(result, as.character(i))
    Sys.sleep(0.5)
  }
  return(result)
}

#* @get /stop
function() {
  return("Process stopped")
}

#* @post /capitalize
function(req) {
  # Parse the JSON data from the request body
  json_data <- fromJSON(req$postBody)
  
  enteredText <- json_data$text
  capitalizedText <- toupper(enteredText)
  return(capitalizedText)
}