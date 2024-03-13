library(shiny)
library(promises)

ui <- fluidPage(
  actionButton("btn_create_future", "Create Future"),
  actionButton("btn_extract_value", "Extract Value"),
  textOutput("result_output")
)

server <- function(input, output, session) {
  
  future_result <- NULL
  
  observeEvent(input$btn_create_future, {
    future_result <<- future({
      # Simulating a time-consuming task
      Sys.sleep(3)
      return("Future completed!")
    })
  })
  
  observeEvent(input$btn_extract_value, {
    if (!is.null(future_result)) {
      value <- value(future_result)
      output$result_output <- renderText(value)
    } else {
      output$result_output <- renderText("Please create the future first.")
    }
  })
}

shinyApp(ui, server)
