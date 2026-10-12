server <- function(input, output, session) {
  isolate({
    send_and_read_info(paste0("add:", session$token))
  })

  task <- Task$new()
  task$start(warm_ast2ast_cache)
  progress_file <- file.path(tempdir(), paste0("tsf_progress_", session$token, ".log"))
  file.create(progress_file)

  showModal(modalDialog(
    title = "Preparing analysis engine",
    "Compiling the loss functions, this happens once per session...",
    footer = NULL, easyClose = FALSE
  ))
  warmup_done <- reactiveVal(FALSE)
  observe({
    req(!warmup_done())
    invalidateLater(500)
    if (task$is_done()) {
      task$collect()
      warmup_done(TRUE)
      removeModal()
    }
  })

  onSessionEnded(function() {
    isolate({
      send_and_read_info(paste0("remove:", session$token))
    })
    task$destructor()
    unlink(progress_file)
  })

  # data import
  # ============================================================================
  data <- reactiveValues(df = NULL, nsigs = 0L, from_eln = FALSE)

  observeEvent(input$upload, {
    req(input$upload)
    df <- importData(input$upload$datapath)
    if (is.data.frame(df)) {
      if (ncol(df) < 2) {
        showNotification("Data has wrong dimensions, at leat two columns were expected")
      } else {
        names(df)[1] <- "var"
        names(df)[2:ncol(df)] <- paste0("signal", seq_len(ncol(df) - 1L))
        data$df <- df
        data$nsigs <- ncol(df) - 1L
        data$from_eln <- FALSE
        output$df <- renderDT(data$df)
      }
    } else {
      if (is(df, "ErrorClass")) {
        print_noti(df$message, duration = 0)
      }
      print_noti("File cannot be used. Upload into R failed!", duration = 0)
    }
  })

  data_batch <- reactiveValues(data_frames = NULL)

  observeEvent(input$upload_batch, {
    req(input$upload_batch)
    list_dataframes <- importDataBatch(input$upload_batch$datapath)
    error <- NULL
    for (i in seq_along(list_dataframes)) {
      df <- list_dataframes[[i]]
      if (is.data.frame(df)) {
        if (ncol(df) < 2) {
          error <- "Error: Data has wrong dimensions, at least columns were expected"
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
          names(df)[1] <- "var"
          names(df)[2:ncol(df)] <- paste0("signal", seq_len(ncol(df) - 1L))
          data$nsigs <- ncol(df) - 1L
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
      data$from_eln <- FALSE
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

  # ELN import
  # ============================================================================
  if (Sys.getenv("RUN_MODE") == "SERVER") observeEvent(session$clientData$url_search, once = TRUE, {
    if (!identical(getQueryString(session)$method, "BindingIsotherm")) {
      return()
    }
    if (!requireNamespace("COMELN", quietly = TRUE)) {
      print_noti(paste(
        "ELN import requires the 'COMELN' package.",
        'Install it via remotes::install_github("ComPlat/OpenStats", subdir = "comeln")'
      ), duration = 0)
      return()
    }
    path <- getExportedValue("COMELN", "download")(session, tempdir())
    res <- parse_eln_payload(path)
    unlink(path)
    if (is(res, "ErrorClass")) {
      print_noti(res$message, duration = 0)
      return()
    }
    data$df <- res$dfs[[1]]
    data$nsigs <- 1L
    data$from_eln <- TRUE
    if (length(res$dfs) > 1) {
      data_batch$data_frames <- res$dfs
      output$active_df <- renderDT(data$df)
    }
    output$df <- renderDT(data$df)
    eln_prefill_inputs(session, res$model, res$constants)
    updateTabItems(session, "Sidebar", res$model)
  })

  nclicks <- reactiveVal(0)

  server_opti_sensi_batch("HG", data, data_batch, nclicks, task, progress_file)
  server_opti_sensi_batch("DBA", data, data_batch, nclicks, task, progress_file)
  server_opti_sensi_batch("IDA", data, data_batch, nclicks, task, progress_file)
  server_opti_sensi_batch("GDA", data, data_batch, nclicks, task, progress_file)
}
