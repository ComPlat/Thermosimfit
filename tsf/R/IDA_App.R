idaUI <- function(id) {
	tabItem(
           tabName = "IDA",
           tags$script(
            "Shiny.addCustomMessageHandler('IDAupdateField', function(message) {
              var result = message.message;
              $('#IDA-IDA_output').append(result + '\\n');
            });"
            ),
           tags$script(
            "Shiny.addCustomMessageHandler('IDAclearField', function(message) {
              $('#IDA-IDA_output').empty();
            });"
            ),
           tags$script(
            "Shiny.addCustomMessageHandler('IDAupdateFieldSense', function(message) {
              var result = message.message;
              $('#IDA-IDA_output_sense').html(result);
            });"
            ),
           tags$script(
            "Shiny.addCustomMessageHandler('IDAclearFieldSense', function(message) {
              $('#IDA-IDA_output_sense').empty();
            });"
            ),

           fluidRow(

             box(
               textInput(NS(id, "IDA_H0"), "Host conc.", value = 0),
               textInput(NS(id, "IDA_D0"), "Dye conc.", value = "0"),
               textInput(NS(id, "IDA_kHD"), "kHD", value = "0"),
               numericInput(NS(id, "IDA_npop"), "Number of particles", value = 40),
               numericInput(NS(id, "IDA_ngen"), "Number of generations", value = 200),
               selectInput(NS(id, "IDA_topology"), "Topology of particle swarm",
                 c("star" = "star",
                   "random" = "random arbitrary neighberhood"), selectize = FALSE),
               numericInput(NS(id, "IDA_threshold"), "Threshold of the error", value = 0.00001),
               width = 6,
               title = "Parameter", solidHeader = TRUE,
               status = "warning", height = 600
              ),

             box(
               box(
                 textInput(NS(id, "IDA_kHD_lb"), "kHD value lower boundary", value = 0),
                 textInput(NS(id, "IDA_kHD_ub"), "kHD value upper boundary", value = 1e09)
                 ),
               box(
                 textInput(NS(id, "IDA_I0_lb"), "I0 value lower boundary", value = 0),
                 textInput(NS(id, "IDA_I0_ub"), "I0 value upper boundary", value = 1)
                 ),
               box(
                 textInput(NS(id, "IDA_IHD_lb"), "IHD value lower boundary", value = 0),
                 textInput(NS(id, "IDA_IHD_ub"), "IHD value upper boundary", value = 1e06)
                 ),
               box(
                 textInput(NS(id, "IDA_ID_lb"), "ID value lower boundary", value = 0),
                 textInput(NS(id, "IDA_ID_ub"), "ID value upper boundary", value = 1e06)
                 ),
               width = 6, title = "Boundaries", solidHeader = TRUE,
               status = "warning", height = 600
               )
            ),

           fluidRow(
            tabBox(

              tabPanel("Optimization",
                fluidRow(
                 box(
                  box(
                   actionButton(NS(id, "IDA_Start_Opti"),"Start Optimization"),
                   actionButton(NS(id, 'IDA_cancel'), 'Cancel'),
                   actionButton(NS(id, 'IDA_status'), 'Get Status'),
                   downloadButton(NS(id, "IDA_download"),"Save result of optimization"),
                   verbatimTextOutput(NS(id, "IDA_output")),
                   width = 12
                   ),
                  box(
                   br(),
                   DT::DTOutput(NS(id, "IDA_params")),
                   DT::DTOutput(NS(id, "IDA_metrices")),
                   plotOutput(NS(id, "IDA_plot")),
                   width = 7, solidHeader = TRUE, status = "warning"
                   ),
                  width = 12, title = "Optimization", solidHeader = TRUE,
                  collapsible = TRUE, status = "warning"
                  )
                 )
              ),

              tabPanel("Sensitivity analysis",
                fluidRow(
                 box(
                   box(
                     numericInput(NS(id, "IDA_sens_bounds"), "+/- boundary in [%]", value = 15),
                     actionButton(NS(id, "IDA_Start_Sensi"),"Start Sensitivity analysis"),
                     actionButton(NS(id, 'IDA_cancel_sense'), 'Cancel'),
                     actionButton(NS(id, 'IDA_status_sense'), 'Get Status'),
                     downloadButton(NS(id, "IDA_sensi_download"), "Save result of sensitivity analysis"),
                     verbatimTextOutput(NS(id, "IDA_output_sense")),
                     width = 12
                     ),
                   box(
                     br(),
                     plotOutput(NS(id, "IDA_sensi")),
                     width = 7, solidHeader = TRUE, status = "warning"
                     ),
                   width = 12, title = "Sensitivity analysis", solidHeader = TRUE,
                   collapsible = TRUE, status = "warning"
                   )
                 )
              ),

              width = 12
              )
            )

           
    )
}



idaServer <- function(id, df, com, com_sense, nclicks, nclicks_sense) {

  moduleServer(id, function(input, output, session) {  
  
  result_val <- reactiveVal()
  result_val_sense <- reactiveVal()

  observeEvent(input$IDA_Start_Opti, {
    if(nclicks() != 0 | nclicks_sense() != 0){
      showNotification("Already running analysis")
      return(NULL)
    }
    session$sendCustomMessage(type = "IDAclearField", list(message = NULL))
    nclicks(nclicks() + 1)
    result_val(data.frame(Status="Running..."))
    com$running()
    session$sendCustomMessage(type = "IDAclearField", list(message = NULL, arg = 1))
    req(input$IDA_H0); req(input$IDA_D0)
    req(input$IDA_kHD); req(input$IDA_npop); req(input$IDA_ngen)
    req(input$IDA_threshold); req(input$IDA_kHD_lb); req(input$IDA_kHD_ub)
    req(input$IDA_IHD_lb); req(input$IDA_IHD_ub); req(input$IDA_ID_lb)
    req(input$IDA_ID_ub); req(input$IDA_I0_lb); req(input$IDA_I0_ub)
    lb <- c(input$IDA_kHD_lb, input$IDA_I0_lb, input$IDA_IHD_lb, input$IDA_ID_lb) 
    lb <- convertToNum(lb)
    req(!("Error" %in% lb))
    ub <- c(input$IDA_kHD_ub, input$IDA_I0_ub, input$IDA_IHD_ub, input$IDA_ID_ub)
    ub <- convertToNum(ub)
    req(!("Error" %in% ub))
    additionalParameters <- c(input$IDA_H0, input$IDA_D0, input$IDA_kHD) 
    additionalParameters <- convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    npop <- input$IDA_npop
    ngen <- input$IDA_ngen
    topo <- input$IDA_topology
    et <- input$IDA_threshold

    result <- future({
      opti("ida", lb, ub, df, additionalParameters,
            npop, ngen, topo, et, com)
    }, seed = TRUE) 
    promises::`%...>%`(result, result_val())
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      showNotification(e$message, duration = 0)
                    })
    result <- finally(result,
                      function(){
                        com$ready()
                        nclicks(0)
                      })
    
    NULL
  })
  observeEvent(input$IDA_cancel,{
    com$interrupt()
  })
  observeEvent(input$IDA_status, {
    req(nclicks() != 0)
    session$sendCustomMessage(type = "IDAupdateField",
     list(message = com$getData()))
  })
  output$IDA_params <- renderDT({
    req(length(result_val()) == 4)
    req(!is.null(result_val()[[2]]))
    result_val()[[2]]
  })
  output$IDA_plot <- renderPlot({
  	req(length(result_val()) == 4)
    req(!is.null(result_val()[[3]]))
    result_val()[[3]]
  })
  output$IDA_metrices <- renderDT({
  	req(length(result_val()) == 4)
    req(!is.null(result_val()[[4]]))
    as.data.frame(result_val()[[4]])
  })
  output$IDA_download <- downloadHandler(
    filename = function() {
      "result.xlsx"
    },
    content = function(file) {
    	req(length(result_val()) == 4)

        wb <- openxlsx::createWorkbook()
        addWorksheet(wb, "Results")
        curr_row <- 1
        curr_val <- result_val()[[1]]
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- result_val()[[2]]
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- as.data.frame(result_val()[[4]])
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- result_val()[[3]]
        tempfile_plot <- tempfile(fileext = ".png")
        ggsave(tempfile_plot,
               plot = curr_val, width = 10, height = 6) 
        insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
        openxlsx::saveWorkbook(wb, file)
        unlink(tempfile_plot)
    }
  )



  observeEvent(input$IDA_Start_Sensi, {
    if(nclicks_sense() != 0 | nclicks() != 0){
      showNotification("Already running analysis")
      return(NULL)
    }
    nclicks_sense(nclicks_sense() + 1)
    result_val_sense(data.frame(Status="Running..."))
    session$sendCustomMessage(type = "IDAclearFieldSense", list(message = NULL, arg = 1))
    com_sense$running()
    req(input$IDA_H0); req(input$IDA_D0)
    req(input$IDA_kHD); req(input$IDA_sens_bounds); req(length(result_val()) == 4)
    additionalParameters <- c(input$IDA_H0, input$IDA_D0, input$IDA_kHD) 
    additionalParameters <- convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    optim_params <- result_val()[[2]]
    sense_bounds <- input$IDA_sens_bounds
    result_sense <- future({
      sensitivity("ida", optim_params, df, additionalParameters,
                        sense_bounds, runAsShiny = com_sense)
    }, seed = TRUE) 
    promises::`%...>%`(result_sense, result_val_sense())
    result_sense <- catch(result_sense,
                    function(e){
                      result_val_sense(NULL)
                      print(e$message)
                      showNotification(e$message, duration = 0)
                    })
    result_sense <- finally(result_sense,
                      function(){
                        com_sense$ready() 
                        nclicks_sense(0)
                      })
    NULL
  })
  observeEvent(input$IDA_cancel_sense,{
    com_sense$interrupt()
  })
  observeEvent(input$IDA_status_sense, {
    req(nclicks_sense() != 0)
    session$sendCustomMessage(type = "IDAupdateFieldSense",
                   list(message = com_sense$getStatus() ))
  })
  output$IDA_sensi <- renderPlot({
    req(inherits(result_val_sense(), "ggplot")) 
    result_val_sense()
  })
  output$IDA_sensi_download <- downloadHandler(
    filename = function() {
      "result.xlsx"
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      addWorksheet(wb, "Results")
      tempfile_plot <- tempfile(fileext = ".png")
      if(inherits(result_val_sense(), "ggplot")) {
        curr_val <- result_val_sense()
        ggsave(tempfile_plot,
               plot = curr_val, width = 10, height = 6) 
        insertImage(wb, "Results", tempfile_plot, startRow = 1)
      }
      openxlsx::saveWorkbook(wb, file)
      unlink(tempfile_plot)
    }
  )


	})
}