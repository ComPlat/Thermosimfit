server <- function(input, output, session) {
  
  HG <- reactiveValues(
    df = NULL,
    opti_res = NULL,
    sensi_res = NULL,
    optimization_run = FALSE,
    sensi_run = FALSE
  )
  output$HG_dat <- renderDT({
    req(input$HG_upload)
    df <- importData(input$HG_upload$datapath) 
     if (is.data.frame(df)) {
      if(ncol(df) != 2) {
        showNotification("Data has wrong dimensions two columns were expected")
      } else if(nrow(df) == 0) {
        showNotification("Data has 0 rows.")
      } else {
        names(df) <- c("dye", "signal")
        HG$df <- df 
      }
     } else {
       showNotification("File can not be used. Upload into R failed!", duration = 0)
     }
  })
  observeEvent(input$HG_Start_Opti, {
    session$sendCustomMessage(type = "clearField", list(message = NULL))
    req(!is.null(HG$df))
    req(input$HG_H0)
    req(input$HG_npop)
    req(input$HG_ngen)
    req(input$HG_topology)
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
    ub <- c(input$HG_kHD_ub, input$HG_I0_ub, input$HG_IHD_ub, input$HG_ID_ub)
    additionalParameters <- c(input$HG_H0)
    npop <- input$HG_npop
    ngen <- input$HG_ngen
    topo <- input$HG_topology
    et <- input$HG_threshold
    temp <- opti("hg", lb, ub, HG$df, additionalParameters, npop, ngen, topo, et, session)
    if(is(temp, "ErrorClass")) {
      showNotification(temp$message, duration = 0)
    } else {
     HG$opti_res <- temp 
     HG$optimization_run <- TRUE
    }
  })
  output$HG_download <- downloadHandler(
    filename = function() {
      "result.xlsx"
    },
    content = function(file) {
        wb <- openxlsx::createWorkbook()
        addWorksheet(wb, "Results")
        if(HG$optimization_run) {
          curr_row <- 1
          curr_val <- HG$opti_res[[1]]
          writeData(wb, "Results", curr_val, startRow = curr_row)
          curr_row <- curr_row + dim(curr_val)[1] + 5
          
          curr_val <- HG$opti_res[[2]]
          writeData(wb, "Results", curr_val, startRow = curr_row)
          curr_row <- curr_row + dim(curr_val)[1] + 5
          
          curr_val <- as.data.frame(HG$opti_res[[4]])
          writeData(wb, "Results", curr_val, startRow = curr_row)
          curr_row <- curr_row + dim(curr_val)[1] + 5
          
          curr_val <- HG$opti_res[[3]]
          tempfile_plot <- tempfile(fileext = ".png")
          ggsave(tempfile_plot,
                 plot = curr_val, width = 10, height = 6) 
          insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
        }
        openxlsx::saveWorkbook(wb, file)
    }
  )
  output$HG_df <- renderDT({
    HG$opti_res[[1]]
  })
  output$HG_params <- renderDT({
    HG$opti_res[[2]]
  })
  output$HG_plot <- renderPlot({
    HG$opti_res[[3]]
  })
  output$HG_metrices <- renderDT({
    as.data.frame(HG$opti_res[[4]])
  })
  observeEvent(input$HG_Start_Sensi, {
    req(!is.null(HG$df))
    req(input$HG_H0)
    req(input$HG_npop)
    req(input$HG_ngen)
    req(input$HG_topology)
    req(input$HG_threshold)
    req(input$HG_kHD_lb)
    req(input$HG_kHD_ub)
    req(input$HG_IHD_lb)
    req(input$HG_IHD_ub)
    req(input$HG_ID_lb)
    req(input$HG_ID_ub)
    req(input$HG_I0_lb)
    req(input$HG_I0_ub)
    req(input$HG_sens_bounds)
    lb <- c(input$HG_kHD_lb, input$HG_I0_lb, input$HG_IHD_lb, input$HG_ID_lb) # isolate them?
    ub <- c(input$HG_kHD_ub, input$HG_I0_ub, input$HG_IHD_ub, input$HG_ID_ub)
    additionalParameters <- c(input$HG_H0)
    temp <- sensitivity("hg", HG$opti_res[[2]], HG$df, additionalParameters,
                        input$HG_sens_bounds)
    if(is(temp, "ErrorClass")) {
      showNotification(temp$message, duration = 0)
    } else {
      HG$sensi_res <- temp
      HG$sensi_run <- TRUE
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
  

  output$GDA_dat <- renderDT({
    # req(input$GDA_upload)
    # df <- upload(input$GDA_upload$datapath) 
    # if (is.data.frame(df)) {
    #   var$df <- df
    # } else {
    #   showNotification("File can not be used. Upload into R failed!", duration = 0)
    # }
  })
  
  observeEvent(input$GDA_Start_Opti, {
    
  })

  observeEvent(input$GDA_Start_Sensi, {
    
  })

  output$IDA_dat <- renderDT({
    # req(input$IDA_upload)
    # df <- upload(input$IDA_upload$datapath) 
    # if (is.data.frame(df)) {
    #   var$df <- df
    # } else {
    #   showNotification("File can not be used. Upload into R failed!", duration = 0)
    # }
  })
  
  observeEvent(input$IDA_Start_Opti, {
    
  })

  observeEvent(input$IDA_Start_Sensi, {
    
  })

}