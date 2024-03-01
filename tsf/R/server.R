server <- function(input, output, session) {
  
  output$HG_dat <- renderDT({
    # req(input$HG_upload)
    # df <- upload(input$HG_upload$datapath) 
    # if (is.data.frame(df)) {
    #   var$df <- df
    # } else {
    #   showNotification("File can not be used. Upload into R failed!", duration = 0)
    # }
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
  
  output$IDA_dat <- renderDT({
    # req(input$IDA_upload)
    # df <- upload(input$IDA_upload$datapath) 
    # if (is.data.frame(df)) {
    #   var$df <- df
    # } else {
    #   showNotification("File can not be used. Upload into R failed!", duration = 0)
    # }
  })
  
  observeEvent(input$HG_Start_Opti, {
    
  })
  
  observeEvent(input$GDA_Start_Opti, {
    
  })
  
  observeEvent(input$IDA_Start_Opti, {
    
  })
  
  
  observeEvent(input$HG_Start_Sensi, {
    
  })
  
  observeEvent(input$GDA_Start_Sensi, {
    
  })
  
  observeEvent(input$IDA_Start_Sensi, {
    
  })
}