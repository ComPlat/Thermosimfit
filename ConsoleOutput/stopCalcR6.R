# https://www.r-bloggers.com/2018/07/long-running-tasks-with-shiny-challenges-and-solutions/
library(R6)
library(shiny)
library(promises)
library(future)

Communicator <- R6::R6Class("Communicator",
  public = list(
    file = NULL,
    result = NULL,
    fileValid = FALSE,
    initialize = function() {
      self$file = tempfile()
      self$result = tempfile()
      write("Ready", self$file)
      write("", self$result)
      self$fileValid = TRUE
    },
    getStatus = function() {
      stopifnot(self$fileValid)
      scan(self$file, what = "character", sep = "\n")  
    },
    setStatus = function(msg){
      stopifnot(self$fileValid)
      write(msg, self$file)
    },
    setData = function(data) {
      stopifnot(self$fileValid)
      write(data, self$result)
    },
    getData = function() {
      stopifnot(self$fileValid)
      scan(self$result, what = "character", sep = "\n")  
    },
    interrupt = function() {
      stopifnot(self$fileValid)
      self$setStatus("interrupt")
    },
    ready = function() {
      stopifnot(self$fileValid)
      self$setStatus("ready")
    },
    running = function(percComplete) {
      stopifnot(self$fileValid)
      msg <- "Running..."
      if (!missing(percComplete)) {
        msg <- paste0("Running... ", percComplete, "% Complete")  
      }
      self$setStatus(msg)  
    },
    isInterrupted = function() {
      stopifnot(self$fileValid)
      self$getStatus() == "interrupt"
    },
    destroy = function() {
      stopifnot(self$fileValid)
      if (file.exists(self$file)) unlink(self$file)
      if (file.exists(self$result)) unlink(self$result)
      self$fileValid <- FALSE
    }
  )                            
)

c <- Communicator$new()
f <- function(n, c) {
  res <- c()
  for (i in 1:n) {
    c$setData(c(res, i)) #paste0(c(res, i), collapse = " , "))
    res <- c(res, i)
    status <- c$getStatus()
    if (status == "interrupt") {
      return()
    }
    Sys.sleep(1)
  }
}

plan(multisession)
fut <- future(f(100, c)) 
c$setStatus("interrupt")
c$getData()
c$destroy()


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
  c <- Communicator$new()
  
  onStop(function(){
    c$destroy()
  })
  
  nclicks <- reactiveVal(0)
  result_val <- reactiveVal()
  
  observeEvent(input$run,{
    if (nclicks() != 0) {
      showNotification("Already running analysis")
      return(NULL)
    }
    nclicks(nclicks() + 1)
    result_val(data.frame(Status = "Running..."))
    c$running()
    result <- future({
      set.seed(1234)
      res <- c()
      for (i in 1:N) {
        Sys.sleep(1)
        c$setData(c(res, i))
        status <- c$getStatus()
        if (status == "interrupt") return()
        c$running(100*i/N)
      }
      quantile(rnorm(1000))
    }) %...>% result_val()
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      showNotification(e$message)
                    })
    result <- finally(result,
                      function(){
                        c$ready()
                        nclicks(0)
                      })
    NULL
  })
  
  output$result <- renderTable({
    req(result_val())
  })
  
  observeEvent(input$cancel,{
    c$interrupt()
  })
  
  observeEvent(input$status,{
    showNotification(c$getStatus())
    showNotification(c$getData())
  })
}

shinyApp(ui = ui, server = server)