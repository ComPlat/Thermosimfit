server_opti_sensi_batch <- function(id, df_reactive, df_list_reactive, nclicks) {
  df <- reactive({df_reactive$df})
  df_list <- reactive({df_list_reactive$data_frames})
  moduleServer(id, function(input, output, session) {

    observeEvent(input$helpButton, {
      showModal(modalDialog(
        title = "Help",
        HTML("Conduct two optimizations. First with wide boundaries. \n
          Afterwards chose narrow boundaries based on the result of the first optimization."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input$AdviceUBIHD, {
      showModal(modalDialog(
        title = "Help",
        HTML("Set upper boundary to IHD * conc ≈ Signal"),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    # Optimization
    # ===============================================================================
    invalid_time <- reactiveVal(1100)
   
    opti_result_created <- reactiveVal(FALSE)
    opti_result <- reactiveVal()
    process <- reactiveVal()
    cancel_clicked <- reactiveVal(FALSE)
    setup_done <- reactiveVal(FALSE)

    # NOTE: Start of model specific code
    # ===============================================================================
    check_inputs <- function() {
      if (id == "HG") {
        rwn(input$H0 != "", "Please enter a value for the Host")
        rwn(!is.na(input$npop),
          "Please enter a value for number of particles")
        rwn(!is.na(input$ngen),
          "Please enter a value for the number of generations")
        rwn(!is.na(input$threshold),
          "Please enter a value for the error threshold")
        rwn(input$kHD_lb != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub != "",
          "Please enter a value for the upper boundary of KaHD")
        rwn(input$IHD_lb != "",
          "Please enter a value for the lower boundary of I(HD)")
        rwn(input$IHD_ub != "",
          "Please enter a value for the upper boundary of I(HD)")
        rwn(input$ID_lb != "",
          "Please enter a value for the lower boundary of I(D)")
        rwn(input$ID_ub != "",
          "Please enter a value for the upper boundary of I(D)")
        rwn(input$I0_lb != "",
          "Please enter a value for the lower boundary of I(0)")
        rwn(input$I0_ub != "",
          "Please enter a value for the upper boundary of I(0)")
        rwn(!is_integer(input$npop),
          "Please enter an integer value for number of particles")
        rwn(!is_integer(input$ngen),
          "Please enter an integer value for number of generations")
      } else if (id == "DBA") {
        rwn(input$D0 != "", "Please enter a value for the Dye")
        rwn(!is.na(input$npop),
          "Please enter a value for number of particles")
        rwn(!is.na(input$ngen),
          "Please enter a value for the number of generations")
        rwn(!is.na(input$threshold),
          "Please enter a value for the error threshold")
        rwn(input$kHD_lb != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub != "",
          "Please enter a value for the upper boundary of KaHD")
        rwn(input$IHD_lb != "",
          "Please enter a value for the lower boundary of I(HD)")
        rwn(input$IHD_ub != "",
          "Please enter a value for the upper boundary of I(HD)")
        rwn(input$ID_lb != "",
          "Please enter a value for the lower boundary of I(D)")
        rwn(input$ID_ub != "",
          "Please enter a value for the upper boundary of I(D)")
        rwn(input$I0_lb != "",
          "Please enter a value for the lower boundary of I(0)")
        rwn(input$I0_ub != "",
          "Please enter a value for the upper boundary of I(0)")
        rwn(!is_integer(input$npop),
          "Please enter an integer value for number of particles")
        rwn(!is_integer(input$ngen),
          "Please enter an integer value for number of generations")
      } else if (id == "IDA") {
        rwn(input$H0 != "", "Please enter a value for the Host")
        rwn(input$D0 != "", "Please enter a value for the Dye")
        rwn(input$kHD != "", "Please enter a value for KaHD")
        rwn(!is.na(input$npop),
          "Please enter a value for number of particles")
        rwn(!is.na(input$ngen),
          "Please enter a value for the number of generations")
        rwn(!is.na(input$threshold),
          "Please enter a value for the error threshold")
        rwn(input$kHG_lb != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub != "",
          "Please enter a value for the upper boundary of KaHG")
        rwn(input$IHD_lb != "",
          "Please enter a value for the lower boundary of I(HD)")
        rwn(input$IHD_ub != "",
          "Please enter a value for the upper boundary of I(HD)")
        rwn(input$ID_lb != "",
          "Please enter a value for the lower boundary of I(D)")
        rwn(input$ID_ub != "",
          "Please enter a value for the upper boundary of I(D)")
        rwn(input$I0_lb != "",
          "Please enter a value for the lower boundary of I(0)")
        rwn(input$I0_ub != "",
          "Please enter a value for the upper boundary of I(0)")
        rwn(!is_integer(input$npop),
          "Please enter an integer value for number of particles")
        rwn(!is_integer(input$ngen),
          "Please enter an integer value for number of generations")
      } else if(id == "GDA") {
        rwn(input$H0 != "", "Please enter a value for the Host")
        rwn(input$G0 != "", "Please enter a value for the Guest")
        rwn(input$kHD != "", "Please enter a value for KaHD")
        rwn(!is.na(input$npop),
          "Please enter a value for number of particles")
        rwn(!is.na(input$ngen),
          "Please enter a value for the number of generations")
        rwn(!is.na(input$threshold),
          "Please enter a value for the error threshold")
        rwn(input$kHG_lb != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub != "",
          "Please enter a value for the upper boundary of KaHG")
        rwn(input$IHD_lb != "",
          "Please enter a value for the lower boundary of I(HD)")
        rwn(input$IHD_ub != "",
          "Please enter a value for the upper boundary of I(HD)")
        rwn(input$ID_lb != "",
          "Please enter a value for the lower boundary of I(D)")
        rwn(input$ID_ub != "",
          "Please enter a value for the upper boundary of I(D)")
        rwn(input$I0_lb != "",
          "Please enter a value for the lower boundary of I(0)")
        rwn(input$I0_ub != "",
          "Please enter a value for the upper boundary of I(0)")
        rwn(!is_integer(input$npop),
          "Please enter an integer value for number of particles")
        rwn(!is_integer(input$ngen),
          "Please enter an integer value for number of generations")
      }
    }

    check_inputs_sensi <- function() { 
      if (id == "HG") {
        rwn(input$H0 != "",
          "Please enter a value for the Host")
        rwn(!is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(opti_result_created(),
          "Please run first an optimization") 
      } else if (id == "DBA") {
        rwn(input$D0 != "",
          "Please enter a value for the Dye")
        rwn(!is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(opti_result_created(),
          "Please run first an optimization") 
      } else if (id == "IDA") {
        rwn(input$H0 != "",
          "Please enter a value for the Host")
        rwn(input$D0 != "",
          "Please enter a value for the Dye")
        rwn(input$kHD != "",
          "Please enter a value for KaHD")
        rwn(!is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(opti_result_created(),
          "Please run first an optimization") 
      } else if (id == "GDA") {
        rwn(input$H0 != "",
          "Please enter a value for the Host")
        rwn(input$G0 != "",
          "Please enter a value for the Guest")
        rwn(input$kHD != "",
          "Please enter a value for KaHD")
        rwn(!is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(opti_result_created(),
          "Please run first an optimization") 
      }
    }

    create_lb <- function() {
      lb <- ""
      if (id == "HG" || id == "DBA") {
        lb <- convert_all_to_num(
          "lower boundaries",
          input$kHD_lb, input$I0_lb, input$IHD_lb, input$ID_lb
        )
      } else if (id == "IDA" || id == "GDA") {
        lb <- convert_all_to_num(
          "lower boundaries",
          input$kHG_lb, input$I0_lb, input$IHD_lb, input$ID_lb
        )
      }
      return(lb)
    }

    create_ub <- function() {
      ub <- ""
      if (id == "HG" || id == "DBA") {
        ub <- convert_all_to_num(
          "upper boundaries",
          input$kHD_ub, input$I0_ub, input$IHD_ub, input$ID_ub
        )
      } else if (id == "IDA" || id == "GDA") {
        ub <- convert_all_to_num(
          "upper boundaries",
          input$kHG_ub, input$I0_ub, input$IHD_ub, input$ID_ub
        )
      }
      return(ub)
    }

    create_additional_parameters <- function() {
      if (id == "HG") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0 
        )
        return(additionalParameters)
      } else if (id == "DBA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$D0 
        )
        return(additionalParameters)
      } else if (id == "IDA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0, input$D0, input$kHD
        )
        return(additionalParameters)
      } else if(id == "GDA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0, input$G0, input$kHD
        )
        return(additionalParameters)
      }
    }

    create_npop <- function() {
      npop <- convert_num_to_int(input$npop)
      return(npop)
    }

    create_ngen <- function() {
      ngen <- convert_num_to_int(input$ngen)
      return(ngen)
    }

    create_topology <- function() {
      topo <- input$topology
      return(topo)
    }

    create_error_threshold <- function() {
      et <- input$threshold
      return(et)
    }

    get_Model <- function() {
      if (id == "HG") {
        return("dba_host_const")
      } else if (id == "DBA") {
        return("dba_dye_const")
      } else if (id == "IDA") {
        return("ida")
      } else if (id == "GDA") {
        return("gda")
      }
    }

    get_Model_capital <- function() {
      if (id == "HG") {
        return("DBA (Host constant")
      } else if (id == "DBA") {
        return("DBA (Dye constant)")
      } else if (id == "IDA") {
        return("IDA")
      } else if (id == "GDA") {
        return("GDA")
      }
    }

    get_K_param <- function() {
      if (id == "HG" || id == "DBA") {
        return("K<sub>a</sub>(HD) [M]")
      } else if (id == "IDA" || id == "GDA") {
        return("K<sub>a</sub>(HG) [M]")
      }
    }

    get_update_field <- function() {
      if (id == "HG") {
        return("HGupdateField")
      } else if (id == "DBA") {
        return("DBAupdateField")
      } else if (id == "IDA") {
        return("IDAupdateField")
      } else if (id == "GDA") {
        return("GDAupdateField")
      }
    }
    
    get_update_field_sense <- function() {
      if (id == "HG") {
        return("HGupdateFieldSense")
      } else if (id == "DBA") {
        return("DBAupdateFieldSense")
      } else if (id == "IDA") {
        return("IDAupdateFieldSense")
      } else if (id == "GDA") {
        return("GDAupdateFieldSense")
      }
    }

    get_update_field_batch <- function() {
      if (id == "HG") {
        return("HGupdateFieldBatch")
      } else if (id == "DBA") {
        return("DBAupdateFieldBatch")
      } else if (id == "IDA") {
        return("IDAupdateFieldBatch")
      } else if (id == "GDA") {
        return("GDAupdateFieldBatch")
      }
    }
    # NOTE: End of model specific code
    # ===============================================================================

    get_opti_result <- function() {
      opti_result()$parameter
    }

    get_sens_bounds <- function() {
     input$sens_bounds
    }

    opti_message <-function(message) {
      session$sendCustomMessage(
        type = get_update_field(),
        list(message = message)
      )
      return(NULL)
    }

    observeEvent(input$Start_Opti, {
      # checks
      if (nclicks() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      check_inputs()
      request_cores(1, session$token)
      lb <- create_lb()
      ub <- create_ub()
      additionalParameters <- create_additional_parameters()
      npop <- create_npop()
      ngen <- create_ngen()
      topo <- create_topology()
      et <- create_error_threshold()
      seed <- input$Seed
      if (is.na(seed)) seed <- as.numeric(Sys.time())
      # clear everything
      setup_done(FALSE)
      opti_result_created(FALSE)
      process(NULL)
      invalid_time(1100)
      nclicks(nclicks() + 1)
      opti_message("Initializing...")

      # start process
      result <- call_opti_in_bg( get_Model(), lb, ub, df(),
        additionalParameters, seed, npop, ngen, topo, et
      )
      process(result)
      setup_done(TRUE)
      NULL
    })

    process_done <- function() {
      req(setup_done())
      req(length(process()) > 0)
      if (process()$is_alive()) {
        req(process()$get_status() != "running")
        req(process()$get_status() != "sleeping")
      }
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }

    correct_results <- function() {
      req(opti_result_created())
      req(!is.null(opti_result()))
    }

    observeEvent(input$cancel, {
      exportTestValues(
        cancel_clicked = TRUE
      )
      req(nclicks() != 0)
      req(!is.null(process()))
      cancel_clicked(TRUE)
    })

    observe({
      invalidateLater(invalid_time())
      req(nclicks() != 0)
      req(!is.null(process()))
      # is cancel_clicked
      if (cancel_clicked()) {
        setup_done(TRUE)
        cancel_clicked(FALSE)
        nclicks(0)
        process()$interrupt()
        process()$wait()
        e <- try(opti_result(process()$get_result()))
        if(inherits(e, "try-error")) {
          opti_result(NULL)
          opti_result_created(FALSE)
        }
        process()$kill()
        opti_message("")
        send_and_read_info(paste0("release: ", session$token))
        return(NULL)
      }
      # check status
      print_error(process()$read_error())
      m <- process()$read_output()
      m <- print_status(m, get_Model())
      req(is.character(m))
      if(m != "") opti_message(m)
    })

    get_opti_data <- reactive({
      if(class(process())[[1]] == "r_process") {
        req(!process()$is_alive())
      }
      try(opti_result(process()$get_result()))
      try({
        if (is.null(opti_result())) {
          # NOTE: handling error in background process
          opti_message("")
          send_and_read_info(paste0("release: ", session$token))
          process()$wait()
          process()$kill()
          process(NULL)
          return(NULL)
        }
      })
      process()$kill()
      send_and_read_info(paste0("release: ", session$token))
      process(NULL)
    })

    # observe results
    observe({
      invalidateLater(invalid_time())
      if (process_done() && !opti_result_created()) {
        get_opti_data()
        opti_result_created(TRUE)
      }
    })

    output$params <- renderDT({
      correct_results()
      res <- opti_result()[[2]]
      names(res)[1] <- get_K_param()
      exportTestValues(
        df_params = res
      )
      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 3)
    })

    output$plot <- renderPlotly({
      correct_results()
      plot_results_plotly(opti_result()[[1]], get_Model())
    })

    output$metrices <- renderDT({
      correct_results()
      res <- as.data.frame(opti_result()[[4]])
      names(res)[4] <- c("R<sup>2</sup>")
      names(res)[5] <- c("R<sup>2</sup> adjusted")
      exportTestValues(
        df_metrices = res
      )
      datatable(res,
        escape = FALSE,
        caption = "Error Metrics: Comparison of in silico signal and measured signal"
      ) |>
        formatSignif(columns = 1:ncol(res), digits = 6)
    })

    output$download <- downloadHandler(
      filename = function() {
        paste("result", switch(input$file_type,
          xlsx = ".xlsx",
          csv = ".csv"
        ), sep = "")
      },
      content = function(file) {
        correct_results()
        result_val <-opti_result()
        if (input$file_type == "xlsx") {
          download_file(get_Model_capital(), file, result_val)
        } else {
          download_csv(get_Model_capital(), file, result_val)
        }
      }
    )



    # sensitivity
    # ===============================================================================

    sensi_message <-function(message) {
      session$sendCustomMessage(
        type = get_update_field_sense(),
        list(message = message)
      )
      return(NULL)
    }
    sensi_result_created <- reactiveVal(FALSE)
    sensi_result <- reactiveVal()
    sensi_process <- reactiveVal()
    sensi_cancel_clicked <- reactiveVal(FALSE)
    sensi_setup_done <- reactiveVal(FALSE)

    observeEvent(input$Start_Sensi, {
      # checks
      if (nclicks() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      check_inputs_sensi()
      request_cores(1, session$token)
      additionalParameters <- create_additional_parameters()
      optim_params <- get_opti_result()
      sense_bounds <- get_sens_bounds()
      # clear everything
      sensi_setup_done(FALSE)
      invalid_time(1100)
      sensi_process(NULL)
      sensi_result_created(FALSE)
      sensi_message("Initializing...")
      # start process
      result <- call_sensi_in_bg(get_Model(), optim_params, df(),
        additionalParameters, sense_bounds)
      nclicks(nclicks() + 1)
      sensi_process(result)
      sensi_setup_done(TRUE)
      NULL
    })

    sensi_process_done <- function() {
      req(sensi_setup_done())
      req(length(sensi_process()) > 0)
      if(sensi_process()$is_alive())  {
        req(sensi_process()$get_status() != "running")
        req(sensi_process()$get_status() != "sleeping")
      }
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }
    observeEvent(input$cancel_sense, {
      exportTestValues(
        cancel_sense_clicked = TRUE
      )
      req(nclicks() != 0)
      req(!is.null(sensi_process()))
      sensi_cancel_clicked(TRUE)
    })

    observe({
      invalidateLater(invalid_time())
      req(nclicks() != 0)
      req(!is.null(sensi_process()))
      # if cancel sense clicked
      if (sensi_cancel_clicked()) {
        sensi_setup_done(TRUE)
        sensi_cancel_clicked(FALSE)
        nclicks(0)
        sensi_process()$interrupt()
        sensi_process()$wait()
        sensi_process()$kill()
        sensi_result(NULL)
        sensi_message("")
        send_and_read_info(paste0("release: ", session$token))
        return(NULL)
      }
      # check status
      print_error(sensi_process()$read_error())
      m <- sensi_process()$read_output()
      req(is.character(m))
      if(nchar(m) > 0) {
        m <- gsub('"', "", m)
        m <- gsub("\\[.*?\\] ", "", m)
        m <- gsub("\n", "", m)
        m <- paste0("Completed: ", m, "%")
        sensi_message(m)
      }
    })

    get_sensi_result <- reactive({
      if(class(sensi_process())[[1]] == "r_process") {
        req(!sensi_process()$is_alive())
      }
      # TODO: wrap in try
      e <- try(sensi_result(sensi_process()$get_result()))
      if (inherits(e, "try-error")) {
        sensi_result(NULL)
        sensi_result_created(FALSE)
      }
    })
    
    # observe results
    observe({
      invalidateLater(invalid_time())
      if (sensi_process_done() && !sensi_result_created()) {
        try(get_sensi_result())
        try({
          if (inherits(sensi_result(), "ErrorClass")) {
            # NOTE: handling error in background process
            print_error(sensi_result()$message)
            sensi_result(NULL)
            sensi_message("")
            send_and_read_info(paste0("release: ", session$token))
            sensi_process()$wait()
            sensi_process()$kill()
            sensi_process(NULL)
            return(NULL)
          }
        })
        sensi_result_created(TRUE)
        sensi_process()$kill()
        sensi_process()$wait()
        sensi_process(NULL)
        send_and_read_info(paste0("release: ", session$token))
      } 
    })

    output$sensi_plot <- renderPlot({
      req(sensi_result_created())
      req(inherits(sensi_result(), "ggplot"))
      exportTestValues(
        sense_plot = {
          sensi_result()
        }
      )
      sensi_result()
    })

    output$sensi_download <- downloadHandler(
      filename = function() {
        "result.xlsx"
      },
      content = function(file) {
        req(sensi_result_created())
        wb <- openxlsx::createWorkbook()
        addWorksheet(wb, "Results")
        tempfile_plot <- tempfile(fileext = ".png")
        if (sensi_result_created()) {
          curr_val <- sensi_result()
          ggsave(tempfile_plot,
            plot = curr_val, width = 10, height = 6
          )
          insertImage(wb, "Results", tempfile_plot, startRow = 1)
        }
        openxlsx::saveWorkbook(wb, file)
        unlink(tempfile_plot)
      }
    )


    # Batch analysis
    # ===============================================================================
    setup_batch_done <- reactiveVal(FALSE)
    batch_results_created <- reactiveVal(FALSE)
    cancel_batch_clicked <- reactiveVal(FALSE)
    num_rep_batch <- reactiveVal()
    stdout <- reactiveVal(NULL)
    task_queue <- reactiveVal(NULL)
    result_batch <- reactiveVal()

    batch_message <-function(message) {
      session$sendCustomMessage(
        type = get_update_field_batch(),
        list(message = message)
      )
      return(NULL)
    }

    check_inputs_batch <- function() {
      rwn(
        !is.na(input$NumRepDataset),
        "Please provide a number of replicates/dataset"
      )
      rwn(
        !is_integer(input$NumRepDataset),
        "Please provide an integer entry for the replicates/dataset"
      )
      rwn(
        length(df_list()) > 0,
        "The dataset list seems to be empty. Please upload a file"
      )
      rwn( # TODO: update also other server code
        !is_integer(input$NumCores),
        "Please provide an integer entry for number of cores"
      )
    }

    get_num_core <- function() {
      res <- convert_num_to_int(input$NumCores)
      if(res == 0) {
        res <- 1
      }
      return(res)
    }

    observeEvent(input$Start_Batch, {
      # Check running analysis
      if (nclicks() != 0 ) {
        print_noti("Already running analysis")
        return(NULL)
      }
      # check input
      check_inputs()
      check_inputs_batch()
      lb <- create_lb()
      ub <- create_ub()
      additionalParameters <- create_additional_parameters()
      npop <- create_npop()
      ngen <- create_ngen()
      topo <- create_topology()
      et <- create_error_threshold()
      num_cores <- get_num_core()
      # check seed case
      seed <- input$Seed
      num_rep <- as.integer(input$NumRepDataset)
      num_rep_batch(num_rep)
      seed_case <- determine_seed_case(seed, num_rep)
      seed_origin <- NULL
      if (seed_case == 3) {
        seed_origin <- seed
      }
      # clear everything
      stdout(NULL)
      result_batch(NULL)
      invalid_time(1100)
      setup_batch_done(FALSE)
      batch_results_created(FALSE)
      output$batch_data_plot <- renderPlotly({
        plot.new()
      })

      output$batch_params_plot <- renderPlot({
        plot.new()
      })
      output$batch_metrices_plot <- renderPlot({
        plot.new()
      })

      size <- length(df_list()) * num_rep
      if (num_cores > size) {
        num_cores <- size
      }
      stdout(character(num_cores))
      request_cores(num_cores, session$token)
      nclicks(nclicks() + 1)
      seeds <- numeric(size)
      seeds_from <- 1:1e6
      session$sendCustomMessage(
        type = get_update_field_batch(),
        list(message = "Initializing...")
      )

      # TODO: use function create_task_queue

      # 1. create seeds in loop
      for (i in seq_len(size)) {
        if (seed_case == 1) {
          seed <- sample(seeds_from, 1)
        } else if (seed_case == 3) {
          if (i %in% seq(1, size, num_rep)) {
            seed <- seed_origin
          } else {
            seed <- sample(seeds_from, 1)
          }
        } else if (seed_case == 2) {
          seed <- seed # TODO: check is this correct
        }
        seeds[i] <- seed
      }

      # 2. Create message lists for each process
      messages <- character(size)
      counter_messages <- 1
      for (i in seq_len(length(df_list()))) {
        for (j in seq_len(num_rep)) {
          messages[counter_messages] <-
            paste0("Dataset = ", i, "; Replicate = ", j)
          counter_messages <- counter_messages + 1
        }
      }

      # 3. Fill task queue
      # TODO: add df idx and num rep info directly and not via messages
      dfs <- rep(df_list(), each = num_rep)
      task_queue(TaskQueue$new(
        get_Model(),
        lb, ub, dfs,
        additionalParameters, seeds,
        npop, ngen, topo, et,
        messages, num_cores
      ))

      # 4. assign tasks
      task_queue()$assign()

      setup_batch_done(TRUE)
      NULL
    })

    batch_process_done <- function() {
      req(setup_batch_done())
      req(!is.null(task_queue()))
      if (task_queue()$check() &&
            !task_queue()$queue_empty()) {
        task_queue()$assign()
      }
      if (!task_queue()$queue_empty()) {
        return(FALSE)
      }
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }

    observeEvent(input$cancel_Batch, {
      exportTestValues(
        cancel_clicked_batch = TRUE
      )
      req(nclicks() != 0)
      cancel_batch_clicked(TRUE)
    })

    update_status <- function() {
      # NOTE: check status
      # (errors are not printed otherwise screen is full of errors)
      stdout(task_queue()$get_status(stdout()))
      bind <- function(a, b) {
        if (is.null(a) && is.null(b)) {
          return("Initialisation")
        }
        if (is.null(a)) {
          return(b)
        }
        if (is.null(b)) {
          return(a)
        }
        paste0(a, "\n", b)
      }
      m <- tryCatch(Reduce(bind, stdout()), error = function(e) {
        print(e)
        return("Error")
      })
      req(is.character(m))
      progress_bar <- task_queue()$get_progress_bar()
      m <- paste(m, "\n", progress_bar)
      session$sendCustomMessage(
        type = get_update_field_batch(),
        list(message = m)
      )
    }


    # observe status
    observe({
      invalidateLater(invalid_time())
      req(nclicks() != 0)
      req(!is.null(task_queue()))
      req(task_queue()$filled)
      # is cancel_batch_clicked
      if (cancel_batch_clicked()) {
        task_queue()$interrupt()
        setup_batch_done(TRUE)
        cancel_batch_clicked(FALSE)
        nclicks(0)
        send_and_read_info(paste0("release: ", session$token))
        return(NULL)
      }
      update_status()
    })

    get_data <- reactive({
      values <- try({
        task_queue()$seperate_results()
      })
      if (inherits(values, "try-error")) {
        batch_message("")
        print_error("Error in background process")
      }
      task_queue()$kill()
      send_and_read_info(paste0("release: ", session$token))
    })

    # observe results
    observe({
      invalidateLater(invalid_time())
      if (batch_process_done() && !batch_results_created()) {
        get_data()
        batch_results_created(TRUE)
        stdout(NULL)
        # NOTE: clear status
        session$sendCustomMessage(
          type = get_update_field_batch(),
          list(message = "")
        )
        values <- task_queue()$results
        result_batch(values)
        output$batch_data_plot <- renderPlotly({
          entirePlotPlotly(
            values
          )
        })
        task_queue(NULL)
      }
    })

    output$batch_download <- downloadHandler(
      filename = function() {
        "result.xlsx"
      },
      content = function(file) {
        req(batch_results_created())
        req(!is.null(result_batch()))
        values <- result_batch()
        download_batch_file(
          get_Model_capital(),
          file,
          values
        )
      }
    )
  })
}
