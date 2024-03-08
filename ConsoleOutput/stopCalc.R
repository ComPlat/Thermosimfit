# https://www.r-bloggers.com/2018/07/long-running-tasks-with-shiny-challenges-and-solutions/
library(shiny)
library(promises)
library(future)
plan(multisession)

ui <- fluidPage(
  titlePanel("Long Run Stoppable Async"),
  sidebarLayout(
    sidebarPanel(
      actionButton('run', 'Run'),
      actionButton('cancel', 'Cancel'),
      actionButton('status', 'Check Status')
    ),
    mainPanel(
      tableOutput("result")
    )
  )
)

server <- function(input, output) {
  N <- 10
  
  # Status File
  status_file <- tempfile()
  
  get_status <- function(){
    scan(status_file, what = "character",sep="\n")
  }
  
  set_status <- function(msg){
    write(msg, status_file)
  }
  
  fire_interrupt <- function(){
    set_status("interrupt")
  }
  
  fire_ready <- function(){
    set_status("Ready")
  }
  
  fire_running <- function(perc_complete){
    if (missing(perc_complete))
      msg <- "Running..."
    else
      msg <- paste0("Running... ", perc_complete, "% Complete")
    set_status(msg)
  }
  
  interrupted <- function(){
    get_status() == "interrupt"
  }
  
  # Delete file at end of session
  onStop(function(){
    print(status_file)
    if (file.exists(status_file))
      unlink(status_file)
  })
  
  # Create Status File
  fire_ready()
  
  
  nclicks <- reactiveVal(0)
  result_val <- reactiveVal()
  
  observeEvent(input$run,{
    # Don't do anything if analysis is already being run
    if (nclicks() != 0) {
      showNotification("Already running analysis")
      return(NULL)
    }
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    result_val(data.frame(Status = "Running..."))
    fire_running()
    result <- future({
      print("Running...")
      for (i in 1:N) {
        # Long Running Task
        Sys.sleep(1)
        # Check for user interrupts
        if (interrupted()) { 
          print("Stopping...")
          stop("User Interrupt")
        }
        # Notify status file of progress
        fire_running(100*i/N)
      }
      #Some results
      quantile(rnorm(1000))
    }) %...>% result_val()
    # Catch inturrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      showNotification(e$message)
                    })
    # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  output$result <- renderTable({
    req(result_val())
  })
  
  # Register user interrupt
  observeEvent(input$cancel,{
    print("Cancel")
    fire_interrupt()
  })
  
  # Let user get analysis progress
  observeEvent(input$status,{
    print("Status")
    showNotification(get_status())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
