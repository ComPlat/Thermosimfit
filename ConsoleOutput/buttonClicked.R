library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  actionButton("myButton", "Click me"),
  tags$script(
    '
    Shiny.addCustomMessageHandler("buttonInfo", function(message) {
      var info = message.info;
      alert("Button Info: " + info);
    });
    '
  )
)

server <- function(input, output, session) {
  observeEvent(input$myButton, {
    # Get some information related to the button
    buttonInfo <- "Button clicked!"
    
    # Send the information to JavaScript
    #runjs(sprintf("Shiny.setInputValue('buttonInfo', %s);", shiny::JS(buttonInfo)))
    
  })
}

shinyApp(ui, server)
