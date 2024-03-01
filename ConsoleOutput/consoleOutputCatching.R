library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(), 
  actionButton("press", "Click me"),
  verbatimTextOutput("output"),
  tags$script(
    "Shiny.addCustomMessageHandler('updateField', function(message) {
      var result = message.message;
      //$('#output').text(result);
      $('#output').append(result + '\\n');
    });"
  ),
  tags$script(
    "Shiny.addCustomMessageHandler('clearField', function(message) {
      $('#output').empty();
    });"
  )
)

calculate <- function(session) {
  for (i in 1:5) {
    print(i)
    session$sendCustomMessage(type = "updateField", list(message = as.character(i)))
    Sys.sleep(0.5)
  }
}

server <- function(input, output, session) {
  observeEvent(input$press, {
    session$sendCustomMessage(type = "clearField", list(message = NULL))
    calculate(session)
    session$sendCustomMessage(type = "updateField", list(message = "test"))
  })
}

shinyApp(ui, server)