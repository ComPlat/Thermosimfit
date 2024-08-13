# I want to create a shiny app where the user can start a long running process
# The status of the long runnign process should be regularly requested by
# an asynchronous javascript function and displayed to the user.
# The long running process should be run in a  promise


library(shiny)
library(promises)
library(future)


plan(multisession)
ui <- fluidPage(
  actionButton("start", "Start Long Process"),
  textOutput("status"),
  tags$script(src = "Shiny.addCustomMessageHandler('status', function(message) {
  $('#status').text(message);
});
")
)

server <- function(input, output, session) {
  status <- reactiveVal("Not started")

  observeEvent(input$start, {
    status("Running...")
    future({
      for (i in 1:10) {
        Sys.sleep(1) # Simulate part of long running process
        status(paste("Running... ", i, "/10 seconds"))
      }
      TRUE
    }) %...>% {
      status("Done")
    }
  })

  observe({
    invalidateLater(1000, session) # Invalidate this reactive context every 1 second
    session$sendCustomMessage(type = "status", message = status())
  })
}

shinyApp(ui, server)
