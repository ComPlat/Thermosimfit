library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(), 
  actionButton("press", "Click me"),
  actionButton("stop", "Stop calc"),
  verbatimTextOutput("output"),
  tags$script(
    "Shiny.addCustomMessageHandler('updateField', function(message) {
      console.log(message.arg);
      console.log(message);
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
    session$sendCustomMessage(type = "updateField", list(
                                                      message = as.character(i),
                                                      arg = 1) )
    Sys.sleep(0.5)
  }
}

server <- function(input, output, session) {
  observeEvent(input$press, {
    session$sendCustomMessage(type = "clearField", list(message = NULL))
    calculate(session)
    session$sendCustomMessage(type = "updateField", list(message = "test", arg = "bla"))
  })
  
  observeEvent(input$stop, {
    print("Stop pressed")
  })
  
}

shinyApp(ui, server)