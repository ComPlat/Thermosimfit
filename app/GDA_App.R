gdaUI <- function(id) {
  tabItem(
           tabName = "GDA",
           tags$script(
            "Shiny.addCustomMessageHandler('GDAupdateField', function(message) {
              var result = message.message;
              $('#GDA-GDA_output').append(result + '\\n');
            });"
            ),
           tags$script(
            "Shiny.addCustomMessageHandler('GDAclearField', function(message) {
              $('#GDA-GDA_output').empty();
            });"
            ),
           tags$script(
            "Shiny.addCustomMessageHandler('GDAupdateFieldSense', function(message) {
              var result = message.message;
              $('#GDA-GDA_output_sense').html(result);
            });"
            ),
           tags$script(
            "Shiny.addCustomMessageHandler('GDAclearFieldSense', function(message) {
              $('#GDA-GDA_output_sense').empty();
            });"
            ),

           fluidRow(

             box(
               textInput(NS(id, "GDA_H0"), "Host conc.", value = 0),
               textInput(NS(id, "GDA_G0"), "Guest conc.", value = "0"),
               textInput(NS(id, "GDA_kHD"), "kHD", value = "0"),
               numericInput(NS(id, "GDA_npop"), "Number of particles", value = 40),
               numericInput(NS(id, "GDA_ngen"), "Number of generations", value = 200),
               selectInput(NS(id, "GDA_topology"), "Topology of particle swarm",
                 c("star" = "star",
                   "random" = "random arbitrary neighberhood"), selectize = FALSE),
               numericInput(NS(id, "GDA_threshold"), "Threshold of the error", value = 0.00001),
               width = 6,
               title = "Parameter", solidHeader = TRUE,
               status = "warning", height = 600
              ),

             box(
               box(
                 textInput(NS(id, "GDA_kHD_lb"), "kHD value lower boundary", value = 0),
                 textInput(NS(id, "GDA_kHD_ub"), "kHD value upper boundary", value = 1e09)
                 ),
               box(
                 textInput(NS(id, "GDA_I0_lb"), "I0 value lower boundary", value = 0),
                 textInput(NS(id, "GDA_I0_ub"), "I0 value upper boundary", value = 1)
                 ),
               box(
                 textInput(NS(id, "GDA_IHD_lb"), "IHD value lower boundary", value = 0),
                 textInput(NS(id, "GDA_IHD_ub"), "IHD value upper boundary", value = 1e06)
                 ),
               box(
                 textInput(NS(id, "GDA_ID_lb"), "ID value lower boundary", value = 0),
                 textInput(NS(id, "GDA_ID_ub"), "ID value upper boundary", value = 1e06)
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
                   actionButton(NS(id, "GDA_Start_Opti"),"Start Optimization"),
                   actionButton(NS(id, 'GDA_cancel'), 'Cancel'),
                   actionButton(NS(id, 'GDA_status'), 'Get Status'),
                   downloadButton(NS(id, "GDA_download"),"Save result of optimization"),
                   verbatimTextOutput(NS(id, "GDA_output")),
                   width = 12
                   ),
                  box(
                   br(),
                   DT::DTOutput(NS(id, "GDA_params")),
                   DT::DTOutput(NS(id, "GDA_metrices")),
                   plotOutput(NS(id, "GDA_plot")),
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
                     numericInput(NS(id, "GDA_sens_bounds"), "+/- boundary in [%]", value = 15),
                     actionButton(NS(id, "GDA_Start_Sensi"),"Start Sensitivity analysis"),
                     actionButton(NS(id, 'GDA_cancel_sense'), 'Cancel'),
                     actionButton(NS(id, 'GDA_status_sense'), 'Get Status'),
                     downloadButton(NS(id, "GDA_sensi_download"), "Save result of sensitivity analysis"),
                     verbatimTextOutput(NS(id, "GDA_output_sense")),
                     width = 12
                     ),
                   box(
                     br(),
                     plotOutput(NS(id, "GDA_sensi")),
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



gdaServer <- function(id, df, com, com_sense, nclicks, nclicks_sense) {

  moduleServer(id, function(input, output, session) {  

  result_val <- reactiveVal()
  result_val_sense <- reactiveVal()

  observeEvent(input$GDA_Start_Opti, {
    if(nclicks() != 0 | nclicks_sense() != 0){
      showNotification("Already running analysis")
      return(NULL)
    }
    session$sendCustomMessage(type = "GDAclearField", list(message = NULL))
    nclicks(nclicks() + 1)
    result_val(data.frame(Status="Running..."))
    com$running()
    session$sendCustomMessage(type = "GDAclearField", list(message = NULL, arg = 1))
    req(input$GDA_H0); req(input$GDA_G0)
    req(input$GDA_kHD); req(input$GDA_npop); req(input$GDA_ngen)
    req(input$GDA_threshold); req(input$GDA_kHD_lb); req(input$GDA_kHD_ub)
    req(input$GDA_IHD_lb); req(input$GDA_IHD_ub); req(input$GDA_ID_lb)
    req(input$GDA_ID_ub); req(input$GDA_I0_lb); req(input$GDA_I0_ub)
    lb <- c(input$GDA_kHD_lb, input$GDA_I0_lb, input$GDA_IHD_lb, input$GDA_ID_lb) 
    lb <- tsf:::convertToNum(lb)
    req(!("Error" %in% lb))
    ub <- c(input$GDA_kHD_ub, input$GDA_I0_ub, input$GDA_IHD_ub, input$GDA_ID_ub)
    ub <- tsf:::convertToNum(ub)
    req(!("Error" %in% ub))
    additionalParameters <- c(input$GDA_H0, input$GDA_G0, input$GDA_kHD) 
    additionalParameters <- tsf:::convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    npop <- input$GDA_npop
    ngen <- input$GDA_ngen
    topo <- input$GDA_topology
    et <- input$GDA_threshold

    result <- future({
      opti("gda", lb, ub, df, additionalParameters,
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
  observeEvent(input$GDA_cancel,{
    com$interrupt()
  })
  observeEvent(input$GDA_status, {
    req(nclicks() != 0)
    session$sendCustomMessage(type = "GDAupdateField",
     list(message = com$getData()))
  })
  output$GDA_params <- renderDT({
    req(length(result_val()) == 4)
    req(!is.null(result_val()[[2]]))
    result_val()[[2]]
  })
  output$GDA_plot <- renderPlot({
    req(length(result_val()) == 4)
    req(!is.null(result_val()[[3]]))
    result_val()[[3]]
  })
  output$GDA_metrices <- renderDT({
    req(length(result_val()) == 4)
    req(!is.null(result_val()[[4]]))
    as.data.frame(result_val()[[4]])
  })
  output$GDA_download <- downloadHandler(
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



  observeEvent(input$GDA_Start_Sensi, {
    if(nclicks_sense() != 0 | nclicks() != 0){
      showNotification("Already running analysis")
      return(NULL)
    }
    nclicks_sense(nclicks_sense() + 1)
    result_val_sense(data.frame(Status="Running..."))
    session$sendCustomMessage(type = "GDAclearFieldSense", list(message = NULL, arg = 1))
    com_sense$running()
    req(input$GDA_H0); req(input$GDA_G0)
    req(input$GDA_kHD); req(input$GDA_sens_bounds); req(length(result_val()) == 4)
    additionalParameters <- c(input$GDA_H0, input$GDA_G0, input$GDA_kHD) 
    additionalParameters <- tsf:::convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    optim_params <- result_val()[[2]]
    sense_bounds <- input$GDA_sens_bounds
    result_sense <- future({
      sensitivity("gda", optim_params, df, additionalParameters,
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
  observeEvent(input$GDA_cancel_sense,{
    com_sense$interrupt()
  })
  observeEvent(input$GDA_status_sense, {
    req(nclicks_sense() != 0)
    session$sendCustomMessage(type = "GDAupdateFieldSense",
                   list(message = com_sense$getStatus() ))
  })
  output$GDA_sensi <- renderPlot({
    req(inherits(result_val_sense(), "ggplot")) 
    result_val_sense()
  })
  output$GDA_sensi_download <- downloadHandler(
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