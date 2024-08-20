idaServer <- function(id, df_reactive, df_list_reactive, com, com_sense, com_batch,
                      nclicks) {
  df <- reactive({df_reactive$df})
  df_list <- reactive({df_list_reactive$data_frames})
  moduleServer(id, function(input, output, session) {
    # general stuff
    # ===============================================================================
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

    opti_message <-function(message) {
      session$sendCustomMessage(
        type = "IDAupdateField",
        list(message = message)
      )
      return(NULL)
    }

    sensi_message <-function(message) {
      session$sendCustomMessage(
        type = "IDAupdateFieldSense",
        list(message = message)
      )
      return(NULL)
    }

    # reactive values
    # ===============================================================================
    invalid_time <- reactiveVal(1100)

    # Optimization
    # ===============================================================================
    opti_result_created <- reactiveVal(FALSE)
    opti_result <- reactiveVal()
    process <- reactiveVal()
    cancel_clicked <- reactiveVal(FALSE)
    setup_done <- reactiveVal(FALSE)

    check_inputs <- function() {
      rwn(input$IDA_H0 != "", "Please enter a value for the Host")
      rwn(input$IDA_D0 != "", "Please enter a value for the Dye")
      rwn(input$IDA_kHD != "", "Please enter a value for KaHD")
      rwn(!is.na(input$IDA_npop),
        "Please enter a value for number of particles")
      rwn(!is.na(input$IDA_ngen),
        "Please enter a value for the number of generations")
      rwn(!is.na(input$IDA_threshold),
        "Please enter a value for the error threshold")
      rwn(input$IDA_kHD_lb != "",
        "Please enter a value for the lower boundary of KaHG")
      rwn(input$IDA_kHD_ub != "",
        "Please enter a value for the upper boundary of KaHG")
      rwn(input$IDA_IHD_lb != "",
        "Please enter a value for the lower boundary of I(HD)")
      rwn(input$IDA_IHD_ub != "",
        "Please enter a value for the upper boundary of I(HD)")
      rwn(input$IDA_ID_lb != "",
        "Please enter a value for the lower boundary of I(D)")
      rwn(input$IDA_ID_ub != "",
        "Please enter a value for the upper boundary of I(D)")
      rwn(input$IDA_I0_lb != "",
        "Please enter a value for the lower boundary of I(0)")
      rwn(input$IDA_I0_ub != "",
        "Please enter a value for the upper boundary of I(0)")
      rwn(!is_integer(input$IDA_npop),
        "Please enter an integer value for number of particles")
      rwn(!is_integer(input$IDA_ngen),
        "Please enter an integer value for number of generations")
    }

    observeEvent(input$IDA_Start_Opti, {
      # checks
      if (nclicks() != 0 ) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      check_inputs()
      request_cores(1, session$token)
      lb <- convert_all_to_num(
        "lower boundaries",
        input$IDA_kHD_lb, input$IDA_I0_lb, input$IDA_IHD_lb, input$IDA_ID_lb
      )
      ub <- convert_all_to_num(
        "upper boundaries",
        input$IDA_kHD_ub, input$IDA_I0_ub, input$IDA_IHD_ub, input$IDA_ID_ub
      )
      additionalParameters <- convert_all_to_num(
        "Additional Parameters",
        input$IDA_H0, input$IDA_D0, input$IDA_kHD
      )
      npop <- convert_num_to_int(input$IDA_npop)
      ngen <- convert_num_to_int(input$IDA_ngen)
      topo <- input$IDA_topology
      et <- input$IDA_threshold
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
      result <- call_opti_in_bg( "ida", lb, ub, df(),
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

    observeEvent(input$IDA_cancel, {
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
        opti_result(process()$get_result())
        opti_message("")
        send_and_read_info(paste0("release: ", session$token))
        return(NULL)
      }
      # check status
      m <-process()$read_output()
      m <- print_ida_gda(m, NULL, NULL)
      req(is.character(m))
      if(m != "") opti_message(m)
    })

    get_opti_data <- reactive({
      if(class(process())[[1]] == "r_process") {
        req(!process()$is_alive())
      }
      opti_result(process()$get_result())
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

    output$IDA_params <- renderDT({
      req(opti_result_created())
      res <-opti_result()[[2]]
      names(res)[1] <- c("K<sub>a</sub>(HG) [M]")
      exportTestValues(
        df_params = res
      )
      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 3)
    })

    output$IDA_plot <- renderPlot({
      req(opti_result_created())
      opti_result()[[3]]
    })

    output$IDA_metrices <- renderDT({
      req(opti_result_created())
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

    output$IDA_download <- downloadHandler(
      filename = function() {
        paste("result", switch(input$file_type,
          xlsx = ".xlsx",
          csv = ".csv"
        ), sep = "")
      },
      content = function(file) {
        req(opti_result_created())
        result_val <-opti_result()
        if (input$file_type == "xlsx") {
          download_file("IDA", file, result_val)
        } else {
          download_csv("IDA", file, result_val)
        }
      }
    )

    # sensitivity
    # ===============================================================================
    sensi_result_created <- reactiveVal(FALSE)
    sensi_result <- reactiveVal()
    sensi_process <- reactiveVal()
    sensi_cancel_clicked <- reactiveVal(FALSE)
    sensi_setup_done <- reactiveVal(FALSE)

    check_inputs_sensi <- function() {
      rwn(input$IDA_H0 != "",
        "Please enter a value for the Host")
      rwn(input$IDA_D0 != "",
        "Please enter a value for the Dye")
      rwn(input$IDA_kHD != "",
        "Please enter a value for KaHD")
      rwn(!is_integer(input$IDA_sens_bounds),
        "Please enter an integer value for the sensitivity boundary")
      rwn(opti_result_created(),
        "Please run first an optimization") 
    }

    observeEvent(input$IDA_Start_Sensi, {
      # checks
      if (nclicks() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      check_inputs_sensi()
      request_cores(1, session$token)
      additionalParameters <- convert_all_to_num(
        "Additional Parameters",
        input$IDA_H0, input$IDA_D0, input$IDA_kHD
      )
      optim_params <- opti_result()$parameter
      sense_bounds <- input$IDA_sens_bounds
      # clear everything
      sensi_setup_done(FALSE)
      invalid_time(1100)
      sensi_process(NULL)
      sensi_result_created(FALSE)
      sensi_message("Initializing...")
      # start process
      result <- call_sensi_in_bg("ida", optim_params, df(),
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
    observeEvent(input$IDA_cancel_sense, {
      exportTestValues(
        cancel_sense_clicked = TRUE
      )
      req(nclicks() != 0)
      req(!is.null(sensi_process()))
      cancel_sense_clicked(TRUE)
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
        sensi_process()$kill()
        sensi_result(NULL)
        sensi_message("")
        send_and_read_info(paste0("release: ", session$token))
        return(NULL)
      }
      # check status
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
      sensi_result(sensi_process()$get_result())
    })
    
    # observe results
    observe({
      invalidateLater(invalid_time())
      if (sensi_process_done() && !sensi_result_created()) {
        get_sensi_result()
        sensi_result_created(TRUE)
        sensi_process()$kill()
        sensi_process()$wait()
        sensi_process(NULL)
        send_and_read_info(paste0("release: ", session$token))
      } 
    })

    output$IDA_sensi_plot <- renderPlot({
      req(sensi_result_created())
      req(inherits(sensi_result(), "ggplot"))
      exportTestValues(
        sense_plot = {
          sensi_result()
        }
      )
      sensi_result()
    })

    output$IDA_sensi_download <- downloadHandler(
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
    result_val_batch <- reactiveValues(result = NULL, result_splitted = NULL)
    batch_results_created <- reactiveVal(FALSE)
    cancel_batch_clicked <- reactiveVal(FALSE)
    num_rep_batch <- reactiveVal()
    stdout <- reactiveVal(NULL)

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
    }

    observeEvent(input$IDA_Start_Batch, {
      # Check running analysis
      if (nclicks() != 0 ) {
        print_noti("Already running analysis")
        return(NULL)
      }
      # check input
      check_inputs()
      check_inputs_batch()
      lb <- convert_all_to_num(
        "lower boundaries",
        input$IDA_kHD_lb, input$IDA_I0_lb, input$IDA_IHD_lb, input$IDA_ID_lb
      )
      ub <- convert_all_to_num(
        "upper boundaries",
        input$IDA_kHD_ub, input$IDA_I0_ub, input$IDA_IHD_ub, input$IDA_ID_ub
      )
      additionalParameters <- convert_all_to_num(
        "Additional Parameters",
        input$IDA_H0, input$IDA_D0, input$IDA_kHD
      )
      npop <- convert_num_to_int(input$IDA_npop)
      ngen <- convert_num_to_int(input$IDA_ngen)
      topo <- input$IDA_topology
      et <- input$IDA_threshold
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
      stdout (NULL)
      invalid_time(1100)
      setup_batch_done(FALSE)
      batch_results_created(FALSE)
      output$IDA_batch_data_plot <- renderPlot({
        plot.new()
      })
      output$IDA_batch_signal_plot <- renderPlot({
        plot.new()
      })

      output$IDA_batch_params_plot <- renderPlot({
        plot.new()
      })
      output$IDA_batch_metrices_plot <- renderPlot({
        plot.new()
      })

      size <- length(df_list()) * num_rep
      stdout(character(size))
      request_cores(size, session$token)
      nclicks(nclicks() + 1)
      process_list <- vector("list", size)
      seeds <- numeric(size)
      seeds_from <- 1:1e6

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
            seed <- seed
        }
        seeds[i] <- seed
        process_list[[i]] <- callr::r_bg(
          function(case, lb, ub, df, ap,
                   seed, npop, ngen, Topology, errorThreshold) {
            res <- tsf::opti(
              case, lb, ub, df, ap, seed, npop, ngen, Topology, errorThreshold
            )
            return(res)
          },
          args = list(
            "ida", lb, ub, df_list()[[(i - 1) %% length(df_list()) + 1]],
            additionalParameters, seed, npop, ngen, topo, et
          )
        )
      }

      result_val_batch$result <- process_list
      setup_batch_done(TRUE)
      NULL
    })

    batch_process_done <- function() {
      req(setup_batch_done())
      req(length(result_val_batch$result) > 0)
      lapply(result_val_batch$result, function(x) {
        if (x$is_alive()) req(x$get_status() != "running")
      })
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }

    observeEvent(input$IDA_cancel_Batch, {
      exportTestValues(
        cancel_clicked_batch = TRUE
      )
      req(nclicks() != 0)
      req(!is.null(result_val_batch$result))
      cancel_batch_clicked(TRUE)
    })

    # observe status
    observe({
      invalidateLater(invalid_time())
      req(nclicks() != 0)
      req(!is.null(result_val_batch$result))
      # is cancel_batch_clicked
      if (cancel_batch_clicked()) {
        setup_batch_done(TRUE)
        cancel_batch_clicked(FALSE)
        nclicks(0)
        lapply(result_val_batch$result, function(process) {
          process$interrupt()
          process$wait()
        })
        session$sendCustomMessage(
          type = "IDAupdateFieldBatch",
          list(message = "")
        )
        send_and_read_info(paste0("release: ", session$token))
        return(NULL)
      }
      # check status
      # TODO: coordinate printing of status
      counter_dataset <- 0
      counter_rep <- 0
      temp_status <- character(length(result_val_batch$result))
      for (i in seq_along(temp_status)) {
        if (((i - 1) %% num_rep_batch()) == 0) {
          counter_dataset <- counter_dataset + 1
          counter_rep <- 1
        } else {
          counter_rep <- counter_rep + 1
        }
        temp_status[i] <- print_ida_gda(
          result_val_batch$result[[i]]$read_output(),
          counter_dataset,
          counter_rep
        )
      }
      stdout(format_batch_status(stdout(), temp_status))
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
      session$sendCustomMessage(
        type = "IDAupdateFieldBatch",
        list(message = m)
      )
    })

    plot_data <- reactive({
      values <- lapply(result_val_batch$result, function(process) {
        process$get_result()
      })
      result_val_batch$result_splitted <- seperate_batch_results(values)
      lapply(result_val_batch$result, function(process) {
        process$kill()
      })
      send_and_read_info(paste0("release: ", session$token))
      result_val_batch$result <- NULL
    })

    # observe results
    observe({
      invalidateLater(invalid_time())
      if (batch_process_done() && !batch_results_created()) {
        plot_data()
        batch_results_created(TRUE)
        stdout(NULL)
      }
    })

    observeEvent(req(batch_results_created()), {
      output$IDA_batch_data_plot <- renderPlot({
        req(batch_results_created())
        plotStates(result_val_batch$result_splitted, num_rep_batch())[[2]] 
      })

      output$IDA_batch_signal_plot <- renderPlot({
        req(batch_results_created())
        plotStates(result_val_batch$result_splitted, num_rep_batch())[[1]] 
      })

      output$IDA_batch_params_plot <- renderPlot({
        req(batch_results_created())
        plotParams(result_val_batch$result_splitted, num_rep_batch())
      })

      output$IDA_batch_metrices_plot <- renderPlot({
        req(batch_results_created())
        plotMetrices(result_val_batch$result_splitted, num_rep_batch())
      })
    })

    output$IDA_batch_download <- downloadHandler(
      filename = function() {
        "result.xlsx"
      },
      content = function(file) {
        req(batch_results_created())
        download_batch_file(
          "IDA",
          file,
          result_val_batch$result_splitted,
          num_rep_batch()
        )
      }
    )
  })
}
