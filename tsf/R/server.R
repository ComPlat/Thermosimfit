
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
      return(df)
    })
  

  HG_com <- Communicator$new()
  HG_com_sense <- Communicator$new()
  GDA_com <- Communicator$new()
  GDA_com_sense <- Communicator$new()
  IDA_com <- Communicator$new()
  IDA_com_sense <- Communicator$new()
  nclicks <- reactiveVal(0)
  nclicks_sense <- reactiveVal(0)

  hgServer("HG", data$df, HG_com, HG_com_sense, nclicks, nclicks_sense)
  idaServer("IDA", data$df, IDA_com, IDA_com_sense, nclicks, nclicks_sense)
  gdaServer("GDA", data$df, GDA_com, GDA_com_sense, nclicks, nclicks_sense)

  onStop(function(){
    HG_com$destroy()
    HG_com_sense$destroy()
    IDA_com$destroy()
    IDA_com_sense$destroy()
    GDA_com$destroy()
    GDA_com_sense$destroy()
  })
  

}