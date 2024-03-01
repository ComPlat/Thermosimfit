# https://gist.github.com/jcheng5/3830244757f8ca25d4b00ce389ea41b3
withConsoleRedirect <- function(containerId, expr) {
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
      ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

server <- function(input, output, session) {
  
  HG <- reactiveValues(
    df = NULL,
    opti_res = NULL
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
    sink(temp_output <- tempfile(), type = "output")
    on.exit(sink(), add = TRUE)  # Reset sink on exit
    temp <- opti("hg", lb, ub, HG$df, additionalParameters, npop, ngen, topo, et)
    output$consoleOutput <- renderPrint({
      cat(readLines(temp_output), sep = "\n")
    })
  })
  
  observeEvent(input$HG_Start_Sensi, {
    
  })
  

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