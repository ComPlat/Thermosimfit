server <- function(input, output, session) {
  
  # data import
  # ============================================================================
  data <- reactiveValues(df = NULL)
  
  output$df <- renderDT({
    req(input$upload)
    df <- importData(input$upload$datapath) 
    if (is.data.frame(df)) {
      if(ncol(df) != 2) {
        showNotification("Data has wrong dimensions two columns were expected")
      } else if(nrow(df) == 0) {
        showNotification("Data has 0 rows.")
      } else {
        names(df) <- c("var", "signal")
        data$df <- df 
      }
    } else {
      showNotification("File can not be used. Upload into R failed!", duration = 0)
    }
  })
  
  observeEvent(input$mod, {
    req(!is.null(data$df))
    req(is.data.frame(data$df))
    req(input$op)
    req(input$new_col)
    dt <- data$df
    op <- input$op
    new_col <- input$new_col
    new <- NULL
    err <- NULL
    e <- try({
      ast <- getAST(str2lang(op))
      ast <- ast[[length(ast)]]
    })
    if (is(e, "ErrorClass")) {
      showNotification(e$message)
      return()
    } else if(inherits(e, "try-error")) {
      showNotification(e)
      return()
    }
    e <- try(
      new <- with(dt, eval(parse(text = op)))
    )
    if (inherits(e, "try-error")) {
      err <- conditionMessage(attr(e, "condition"))
    } else {
      data$df[, new_col] <- new
    }
    output$df <- renderDT(data$df)
    output$mod_error <- renderText(err)  
  })
  
  # Utils
  # ============================================================================
  convertToNum <- function(l) {
    res <- sapply(l, function(x) {
      ast <- try(getAST(str2lang(x)))
      if (is(e, "ErrorClass")) {
        showNotification(e$message)
        return("Error")
      } else if(inherits(e, "try-error")) {
        showNotification(e)
        return("Error")
      } else {
        return(eval(parse(text = x)))
      }
    })
    return(res)
  }
  
  # HG
  # ============================================================================
  HG <- reactiveValues(
    opti_res = NULL,
    sensi_res = NULL,
    optimization_run = FALSE,
    sensi_run = FALSE
  )
  observeEvent(input$HG_Start_Opti, {
    session$sendCustomMessage(type = "clearField", list(message = NULL, arg = 1))
    req(!is.null(data$df))
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
    temp <- try(opti("hg", lb, ub, data$df, additionalParameters,
                 npop, ngen, topo, et, list(session, 1)))
    if (inherits(temp, "try-error")) {
      showNotification(temp, duration = 0)
    } else if(is(temp, "ErrorClass")) {
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
    req(!is.null(data$df))
    req(input$HG_H0)
    req(input$HG_sens_bounds)
    additionalParameters <- c(input$HG_H0)
    additionalParameters <- convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    shinyjs::show(id = "HG_sens_runs", anim = TRUE)
    temp <- try(sensitivity("hg", HG$opti_res[[2]], data$df, additionalParameters,
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
  
  
  
  
  
  
  # GDA
  # ============================================================================
  GDA <- reactiveValues(
    opti_res = NULL,
    sensi_res = NULL,
    optimization_run = FALSE,
    sensi_run = FALSE
  )
  observeEvent(input$GDA_Start_Opti, {
    session$sendCustomMessage(type = "clearField", list(message = NULL, arg = 2))
    req(!is.null(data$df))
    req(input$GDA_H0)
    req(input$GDA_G0)
    req(input$GDA_kHD)
    req(input$GDA_npop)
    req(input$GDA_ngen)
    req(input$GDA_threshold)
    req(input$GDA_kHD_lb)
    req(input$GDA_kHD_ub)
    req(input$GDA_IHD_lb)
    req(input$GDA_IHD_ub)
    req(input$GDA_ID_lb)
    req(input$GDA_ID_ub)
    req(input$GDA_I0_lb)
    req(input$GDA_I0_ub)
    lb <- c(input$GDA_kHD_lb, input$GDA_I0_lb, input$GDA_IHD_lb, input$GDA_ID_lb) # isolate them?
    lb <- convertToNum(lb)
    req(!("Error" %in% lb))
    ub <- c(input$GDA_kHD_ub, input$GDA_I0_ub, input$GDA_IHD_ub, input$GDA_ID_ub)
    ub <- convertToNum(ub)
    req(!("Error" %in% ub))
    additionalParameters <- c(input$GDA_H0, input$GDA_G0, input$GDA_kHD)
    additionalParameters <- convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    npop <- input$GDA_npop
    ngen <- input$GDA_ngen
    topo <- input$GDA_topology
    et <- input$GDA_threshold
    temp <- try(opti("gda", lb, ub, data$df, additionalParameters, npop, ngen,
                 topo, et, list(session, 2)))
    if (inherits(temp, "try-error")) {
      showNotification(temp, duration = 0)
    } else if(is(temp, "ErrorClass")) {
      showNotification(temp$message, duration = 0)
    } else {
      GDA$opti_res <- temp 
      GDA$optimization_run <- TRUE
    }
  })
  output$GDA_download <- downloadHandler(
    filename = function() {
      "result.xlsx"
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      addWorksheet(wb, "Results")
      if(GDA$optimization_run) {
        curr_row <- 1
        curr_val <- GDA$opti_res[[1]]
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- GDA$opti_res[[2]]
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- as.data.frame(GDA$opti_res[[4]])
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- GDA$opti_res[[3]]
        tempfile_plot <- tempfile(fileext = ".png")
        ggsave(tempfile_plot,
               plot = curr_val, width = 10, height = 6) 
        insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
      }
      openxlsx::saveWorkbook(wb, file)
    }
  )
  output$GDA_params <- renderDT({
    GDA$opti_res[[2]]
  })
  output$GDA_plot <- renderPlot({
    GDA$opti_res[[3]]
  })
  output$GDA_metrices <- renderDT({
    as.data.frame(GDA$opti_res[[4]])
  })
  observeEvent(input$GDA_Start_Sensi, {
    req(!is.null(data$df))
    req(input$GDA_H0)
    req(input$GDA_G0)
    req(input$GDA_kHD)
    req(input$GDA_sens_bounds)
    additionalParameters <- c(input$GDA_H0, input$GDA_G0, input$GDA_kHD)
    additionalParameters <- convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    shinyjs::show(id = "GDA_sens_runs", anim = TRUE)
    temp <- try(sensitivity("gda", GDA$opti_res[[2]], data$df, additionalParameters,
                        input$GDA_sens_bounds))
    if (inherits(temp, "try-error")) {
      showNotification(temp, duration = 0)
    } else if(is(temp, "ErrorClass")) {
      showNotification(temp$message, duration = 0)
      shinyjs::hide(id = "GDA_sens_runs", anim = TRUE)
    } else {
      GDA$sensi_res <- temp
      GDA$sensi_run <- TRUE
      shinyjs::hide(id = "GDA_sens_runs", anim = TRUE)
    }
  })
  output$GDA_sensi <- renderPlot({
    GDA$sensi_res
  })
  output$GDA_sensi_download <- downloadHandler(
    filename = function() {
      "result.xlsx"
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      addWorksheet(wb, "Results")
      if(GDA$optimization_run) {
        curr_val <- GDA$sensi_res
        tempfile_plot <- tempfile(fileext = ".png")
        ggsave(tempfile_plot,
               plot = curr_val, width = 10, height = 6) 
        insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
      }
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  
  
  
  
  
  
  # IDA
  # ============================================================================
  IDA <- reactiveValues(
    opti_res = NULL,
    sensi_res = NULL,
    optimization_run = FALSE,
    sensi_run = FALSE
  )
  observeEvent(input$IDA_Start_Opti, {
    session$sendCustomMessage(type = "clearField", list(message = NULL, arg = 3))
    req(!is.null(data$df))
    req(input$IDA_H0)
    req(input$IDA_D0)
    req(input$IDA_kHD)
    req(input$IDA_npop)
    req(input$IDA_ngen)
    req(input$IDA_threshold)
    req(input$IDA_kHD_lb)
    req(input$IDA_kHD_ub)
    req(input$IDA_IHD_lb)
    req(input$IDA_IHD_ub)
    req(input$IDA_ID_lb)
    req(input$IDA_ID_ub)
    req(input$IDA_I0_lb)
    req(input$IDA_I0_ub)
    lb <- c(input$IDA_kHD_lb, input$IDA_I0_lb, input$IDA_IHD_lb, input$IDA_ID_lb) # isolate them?
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
    temp <- try(opti("ida", lb, ub, data$df, additionalParameters, npop, ngen,
                 topo, et, list(session, 3)))
    if (inherits(temp, "try-error")) {
      showNotification(temp, duration = 0)
    } else if(is(temp, "ErrorClass")) {
      showNotification(temp$message, duration = 0)
    } else {
      IDA$opti_res <- temp 
      IDA$optimization_run <- TRUE
    }
  })
  output$IDA_download <- downloadHandler(
    filename = function() {
      "result.xlsx"
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      addWorksheet(wb, "Results")
      if(IDA$optimization_run) {
        curr_row <- 1
        curr_val <- IDA$opti_res[[1]]
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- IDA$opti_res[[2]]
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- as.data.frame(IDA$opti_res[[4]])
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5
        
        curr_val <- IDA$opti_res[[3]]
        tempfile_plot <- tempfile(fileext = ".png")
        ggsave(tempfile_plot,
               plot = curr_val, width = 10, height = 6) 
        insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
      }
      openxlsx::saveWorkbook(wb, file)
    }
  )
  output$IDA_params <- renderDT({
    IDA$opti_res[[2]]
  })
  output$IDA_plot <- renderPlot({
    IDA$opti_res[[3]]
  })
  output$IDA_metrices <- renderDT({
    as.data.frame(IDA$opti_res[[4]])
  })
  observeEvent(input$IDA_Start_Sensi, {
    req(!is.null(data$df))
    req(input$IDA_H0)
    req(input$IDA_D0)
    req(input$IDA_kHD)
    req(input$IDA_sens_bounds)
    additionalParameters <- c(input$IDA_H0, input$IDA_D0, input$IDA_kHD)
    additionalParameters <- convertToNum(additionalParameters)
    req(!("Error" %in% additionalParameters))
    shinyjs::show(id = "IDA_sens_runs", anim = TRUE)
    temp <- try(sensitivity("ida", IDA$opti_res[[2]], data$df, additionalParameters,
                        input$IDA_sens_bounds))
    if (inherits(temp, "try-error")) {
      showNotification(temp, duration = 0)
    } else if(is(temp, "ErrorClass")) {
      showNotification(temp$message, duration = 0)
      shinyjs::hide(id = "IDA_sens_runs", anim = TRUE)
    } else {
      IDA$sensi_res <- temp
      IDA$sensi_run <- TRUE
      shinyjs::hide(id = "IDA_sens_runs", anim = TRUE)
    }
  })
  output$IDA_sensi <- renderPlot({
    IDA$sensi_res
  })
  output$IDA_sensi_download <- downloadHandler(
    filename = function() {
      "result.xlsx"
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      addWorksheet(wb, "Results")
      if(IDA$optimization_run) {
        curr_val <- IDA$sensi_res
        tempfile_plot <- tempfile(fileext = ".png")
        ggsave(tempfile_plot,
               plot = curr_val, width = 10, height = 6) 
        insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
      }
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
  
  

}