server <- function(input, output, session) {
  isolate({
    send_and_read_info(paste0("add:", session$token))
  })

  onSessionEnded(function() {
    isolate({
      send_and_read_info(paste0("remove:", session$token))
    })
  })

  # data import
  # ============================================================================
  data <- reactiveValues(df = NULL)

  observeEvent(input$upload, {
    req(input$upload)
    df <- importData(input$upload$datapath)
    if (is.data.frame(df)) {
      if (ncol(df) != 2) {
        showNotification("Data has wrong dimensions, two columns were expected")
      } else if (nrow(df) == 0) {
        showNotification("Data has 0 rows.")
      } else {
        names(df) <- c("var", "signal")
        data$df <- df
        output$df <- renderDT(data$df)
      }
    } else {
      if (is(df, "ErrorClass")) {
        print_noti(df$message, duration = 0)
      }
      print_noti("File cannot be used. Upload into R failed!", duration = 0)
    }
  })

  observeEvent(input$mod, {
    req(!is.null(data$df))
    req(is.data.frame(data$df))
    rwn(nchar(input$op) > 0, "No operation was defined")
    req(input$new_col)
    req(nchar(input$new_col) > 0)
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
    } else if (inherits(e, "try-error")) {
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


  data_batch <- reactiveValues(data_frames = NULL)

  # TODO: remove
  # list_dataframes <-
  #   importDataBatch("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/idaBatch.csv")
  #
  # observe({ output$active_df <- renderDT(data$df)
  #   data_batch$data_frames <- list_dataframes
  #   data$df <- data_batch$data_frames[[1]]
  #   output$df <- renderDT(data$df)
  # })

  observeEvent(input$upload_batch, {
    req(input$upload_batch)
    list_dataframes <- importDataBatch(input$upload_batch$datapath)
    error <- NULL
    for (i in seq_along(list_dataframes)) {
      df <- list_dataframes[[i]]
      if (is.data.frame(df)) {
        if (ncol(df) != 2) {
          error <- "Error: Data has wrong dimensions, two columns were expected"
          break
          showNotification(
            paste0(
              "Measurement Nr. ", i,
              "Data has wrong dimensions, two columns were expected"
            )
          )
        } else if (nrow(df) == 0) {
          error <- "Error: Data has 0 rows."
          break
          showNotification(
            paste0("Measurement Nr. ", i, " Data has 0 rows.")
          )
        } else {
          names(df) <- c("var", "signal")
        }
      } else {
        error <- "Error: File cannot be used. Upload into R failed!"
        break
        showNotification(
          paste0("Measurement Nr. ", i,
            " cannot be used. Upload into R failed!",
            duration = 0
          )
        )
      }
    }
    if (is.null(error) && is.list(list_dataframes) && length(list_dataframes) > 0) {
      data_batch$data_frames <- list_dataframes
    }
    data$df <- data_batch$data_frames[[1]]
    output$active_df <- renderDT(data$df)
    output$df <- renderDT(data$df)
  })

  observeEvent(input$active_dataset, {
    req(!is.null(data_batch$data_frames))
    req(is.list(data_batch$data_frames))
    req(input$active_dataset)
    req(input$active_dataset > 0)
    req(input$active_dataset <= length(data_batch$data_frames))
    data$df <- data_batch$data_frames[[input$active_dataset]]
    output$active_df <- renderDT(data$df)
    output$df <- renderDT(data$df)
  })

  nclicks <- reactiveVal(0)

  hgServer("HG", data, data_batch, nclicks)
  dbaServer("DBA", data, data_batch, nclicks)
  idaServer("IDA", data, data_batch, nclicks)
  gdaServer("GDA", data, data_batch, nclicks)
}
