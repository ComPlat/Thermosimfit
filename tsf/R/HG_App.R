hgUI <- function(id) {
	tabItem(
           tabName = "HG",
           box(
             textInput(NS(id, "HG_H0"), "Host conc.", value = 0),
             numericInput(NS(id, "HG_npop"), "Number of particles", value = 40),
             numericInput(NS(id, "HG_ngen"), "Number of generations", value = 200),
             selectInput(NS(id, "HG_topology"), "Topology of particle swarm",
                         c("star" = "star",
                           "random" = "random"), selectize = FALSE),
             numericInput(NS(id, "HG_threshold"), "Threshold of the error", value = 0.00001),
             width = 12
           ),
           box(
             box(
               textInput(NS(id, "HG_kHD_lb"), "kHD value lower boundary", value = 0),
               textInput(NS(id, "HG_kHD_ub"), "kHD value upper boundary", value = 1e09)
             ),
             box(
               textInput(NS(id, "HG_I0_lb"), "I0 value lower boundary", value = 0),
               textInput(NS(id, "HG_I0_ub"), "I0 value upper boundary", value = 1)
             ),
             box(
               textInput(NS(id, "HG_IHD_lb"), "IHD value lower boundary", value = 0),
               textInput(NS(id, "HG_IHD_ub"), "IHD value upper boundary", value = 1e06)
             ),
             box(
               textInput(NS(id, "HG_ID_lb"), "ID value lower boundary", value = 0),
               textInput(NS(id, "HG_ID_ub"), "ID value upper boundary", value = 1e06)
             ),
             actionButton(NS(id, "HG_Start_Opti"),"Start Optimization"),
             downloadButton(NS(id, "HG_download"),"Save result of optimization"),
             actionButton(NS(id, 'HG_cancel'), 'Cancel'),
             actionButton(NS(id, 'HG_status'), 'Check Status'),
             verbatimTextOutput(NS(id, "HG_output")),
             DT::DTOutput(NS(id, "HG_params")),
             DT::DTOutput(NS(id, "HG_metrices")),
             box(
               plotOutput(NS(id, "HG_plot")),
               width = 9
             ),
             width = 12
           ),
           box(
             numericInput(NS(id, "HG_sens_bounds"), "+/- boundary in [%]", value = 15),
             box(
               actionButton(NS(id, "HG_Start_Sensi"),"Start Sensitivity analysis"),
               downloadButton(NS(id, "HG_sensi_download"), "Save result of sensitivity analysis")
             ),
             div(id = NS(id, "HG_sens_runs"),
                 style = "display: none;
                          font-weight: bold;
                          font-size: 16px;
                          margin-top: 10px;",
                 "Sensitivity analysis runs"),
             plotOutput(NS(id, "HG_sensi")),
             width = 12
           )
    )
}



hgServer <- function(id, df, result_val, nclicks, fire_running, fire_ready, fire_interrupt,
					 interrupted, get_status, set_status) {

  moduleServer(id, function(input, output, session) {
  
  observeEvent(input$HG_Start_Opti, {
    
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("Already running analysis")
      return(NULL)
    }
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    result_val(data.frame(Status="Running..."))
    fire_running()
    
    session$sendCustomMessage(type = "clearField", list(message = NULL, arg = 1))
    req(input$HG_H0)
    req(input$HG_npop)
    req(input$HG_ngen)
    req(input$HG_threshold)
    req(input$HG_kHD_lb)
    req(input$HG_kHD_ub)
    req(input$HG_IHD_lb)
    req(input$HG_IHD_ub)
    req(input$HG_ID_lb)
    req(input$HG_ID_ub)
    req(input$HG_I0_lb)
    req(input$HG_I0_ub)
    lb <- c(input$HG_kHD_lb, input$HG_I0_lb, input$HG_IHD_lb, input$HG_ID_lb) # isolate them?
    lb <- convertToNum(lb)
    req(!("Error" %in% lb))
    ub <- c(input$HG_kHD_ub, input$HG_I0_ub, input$HG_IHD_ub, input$HG_ID_ub)
    ub <- convertToNum(ub)
    req(!("Error" %in% ub))
    additionalParameters <- c(input$HG_H0)
    additionalParameters <- convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    npop <- input$HG_npop
    ngen <- input$HG_ngen
    topo <- input$HG_topology
    et <- input$HG_threshold
    result <- future({
      opti("hg", lb, ub, df, additionalParameters,
            npop, ngen, topo, et, list(interrupted, fire_running))
    }) 
    promises::`%...>%`(result, result_val())
    # Catch inturrupt (or any other error) and notify user
    result <- catch(result,
                    function(e){
                      result_val(NULL)
                      print(e$message)
                      showNotification(e$message, duration = 0)
                    })
    # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
    result <- finally(result,
                      function(){
                        fire_ready() 
                        nclicks(0)
                      })
    
    NULL
  })
  
  # Register user interrupt
  observeEvent(input$HG_cancel,{
    fire_interrupt()
  })
  
  # Let user get analysis progress
  observeEvent(input$HG_status,{
    showNotification(get_status())
  })

  output$HG_params <- renderDT({
    req(length(result_val()) == 4)
    req(!is.null(result_val()[[2]]))
    result_val()[[2]]
  })
  output$HG_plot <- renderPlot({
  	req(length(result_val()) == 4)
    req(!is.null(result_val()[[3]]))
    result_val()[[3]]
  })
  output$HG_metrices <- renderDT({
  	req(length(result_val()) == 4)
    req(!is.null(result_val()[[4]]))
    as.data.frame(result_val()[[4]])
  })
  
  output$HG_download <- downloadHandler(
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
    }
  )

  HG <- reactiveValues(sensi_res = NULL, sensi_run = FALSE)
  
  observeEvent(input$HG_Start_Sensi, {
    req(input$HG_H0)
    req(input$HG_sens_bounds)
    req(length(result_val()) == 4)
    additionalParameters <- c(input$HG_H0)
    additionalParameters <- convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    shinyjs::show(id = "HG_sens_runs", anim = TRUE)
    temp <- try(sensitivity("hg", result_val()[[2]], df, additionalParameters,
                        input$HG_sens_bounds))
    if (inherits(temp, "try-error")) {
      showNotification(temp, duration = 0)
    } else if(is(temp, "ErrorClass")) {
      showNotification(temp$message, duration = 0)
      shinyjs::hide(id = "HG_sens_runs", anim = TRUE)
    } else {
      HG$sensi_res <- temp
      HG$sensi_run <- TRUE
      shinyjs::hide(id = "HG_sens_runs", anim = TRUE)
    }
  })

  output$HG_sensi <- renderPlot({
    HG$sensi_res
  })

  output$HG_sensi_download <- downloadHandler(
    filename = function() {
      "result.xlsx"
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      addWorksheet(wb, "Results")
      if(HG$optimization_run) {
        curr_val <- HG$sensi_res
        tempfile_plot <- tempfile(fileext = ".png")
        ggsave(tempfile_plot,
               plot = curr_val, width = 10, height = 6) 
        insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
      }
      openxlsx::saveWorkbook(wb, file)
    }
  )
	})
}