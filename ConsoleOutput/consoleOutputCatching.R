library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(), 
  actionButton("press", "Click me"),
  actionButton("stop", "Stop calc"),
  verbatimTextOutput("output"),
  tags$script(
    "var appendCount = 0;
    Shiny.addCustomMessageHandler('updateField', function(message) {
      if (appendCount < 10) {
        var result = message.message;
        $('#output').append(result + '\\n');
        appendCount++;
      } else {
        appendCount = 0; // Reset append count after 10 appends
        $('#output').empty(); // Clear the output
      }
    });"
  ),
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
  for (i in 1:15) {
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
  
  observeEvent(input$stop, {
    print("Stop pressed")
  })
  
}

shinyApp(ui, server)