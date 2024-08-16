idaServer <- function(id, df, df_list, com, com_sense, com_batch,
                      nclicks, nclicks_sense) {
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

    # reactive values
    # ===============================================================================
    result_val <- reactiveVal()
    result_val_sense <- reactiveVal()
    result_val_batch <- reactiveValues(result = NULL, result_splitted = NULL)
    opti_done <- reactiveVal(FALSE)
    sensi_done <- reactiveVal(FALSE)
    batch_done <- reactiveVal(FALSE)

    # helper
    # ===============================================================================
    fl <- reactive({
      flush(com$result)
      return()
    })
    opti_success <- function() {
      req(opti_done())
      req(length(result_val()) == 11)
    }
    sensi_success <- function() {
      req(sensi_done())
      req(inherits(result_val_sense(), "ggplot"))
    }
    batch_success <- function() {
      req(batch_done())
      req(length(result_val_batch$result) > 0)
      lapply(result_val_batch$result, function(x) {
        req(!is.null(x))
      })
      return(TRUE)
    }
    # Optimization
    # ===============================================================================
    check_inputs <- function() {
      rwn(input$IDA_H0 != "", "Please enter a value for the Host")
      rwn(input$IDA_D0 != "", "Please enter a value for the Dye")
      rwn(input$IDA_kHD != "", "Please enter a value for KaHD")
      rwn(!is.na(input$IDA_npop), "Please enter a value for number of particles")
      rwn(!is.na(input$IDA_ngen), "Please enter a value for the number of generations")
      rwn(!is.na(input$IDA_threshold), "Please enter a value for the error threshold")
      rwn(input$IDA_kHD_lb != "", "Please enter a value for the lower boundary of KaHG")
      rwn(input$IDA_kHD_ub != "", "Please enter a value for the upper boundary of KaHG")
      rwn(input$IDA_IHD_lb != "", "Please enter a value for the lower boundary of I(HD)")
      rwn(input$IDA_IHD_ub != "", "Please enter a value for the upper boundary of I(HD)")
      rwn(input$IDA_ID_lb != "", "Please enter a value for the lower boundary of I(D)")
      rwn(input$IDA_ID_ub != "", "Please enter a value for the upper boundary of I(D)")
      rwn(input$IDA_I0_lb != "", "Please enter a value for the lower boundary of I(0)")
      rwn(input$IDA_I0_ub != "", "Please enter a value for the upper boundary of I(0)")
      rwn(!is_integer(input$IDA_npop), "Please enter an integer value for number of particles")
      rwn(!is_integer(input$IDA_ngen), "Please enter an integer value for number of generations")
    }

    observeEvent(input$IDA_Start_Opti, {
      if (nclicks() != 0 | nclicks_sense() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      session$sendCustomMessage(type = "IDAclearField", list(message = NULL))
      nclicks(nclicks() + 1)
      result_val(data.frame(Status = "Running..."))
      com$running()
      session$sendCustomMessage(type = "IDAclearField", list(message = NULL, arg = 1))
      check_inputs()
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
      fl()
      opti_done(FALSE)
      session$sendCustomMessage(
        type = "IDAupdateField",
        list(message = "Initialisation")
      )
      result <- future(
        {
          opti(
            "ida", lb, ub, df, additionalParameters,
            seed,
            npop, ngen, topo, et, com
          )
        },
        seed = TRUE
      )
      session$sendCustomMessage(
        type = "IDAupdateField",
        list(message = "")
      )

      promises::`%...>%`(result, result_val())
      result <- catch(
        result,
        function(e) {
          result_val(NULL)
          print(e$message) # TODO: remove all those print error messages
          print_noti(e$message, type = "error", duration = 0)
        }
      )
      result <- finally(
        result,
        function() {
          com$ready()
          nclicks(0)
          opti_done(TRUE)
        }
      )

      NULL
    })

    observeEvent(input$IDA_cancel, {
      exportTestValues(
        cancel_clicked = TRUE
      )
      com$interrupt()
    })

    observeEvent(input$IDA_status, {
      req(nclicks() != 0)
      m <- com$getData()
      if (length(nchar(m)) == 0) m <- "Still initialising"
      exportTestValues(
        status1 = {
          m
        }
      )
      session$sendCustomMessage(
        type = "IDAupdateField",
        list(message = m)
      )
    })

    output$IDA_params <- renderDT({
      req(opti_success())
      req(!is.null(result_val()[[2]]))
      res <- result_val()[[2]]
      names(res)[1] <- c("K<sub>a</sub>(HG) [M]")
      exportTestValues(
        df_params = res
      )
      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 3)
    })

    output$IDA_plot <- renderPlot({
      req(opti_done())
      req(!is.null(result_val()[[3]]))
      result_val()[[3]]
    })

    output$IDA_metrices <- renderDT({
      req(opti_success())
      req(!is.null(result_val()[[4]]))
      res <- as.data.frame(result_val()[[4]])
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
        req(opti_success())
        result_val <- result_val()
        if (input$file_type == "xlsx") {
          download_file(file, result_val)
        } else {
          download_csv(file, result_val)
        }
      }
    )

    # sensitivity
    # ===============================================================================
    check_inputs_sensi <- function() {
      rwn(input$IDA_H0 != "", "Please enter a value for the Host")
      rwn(input$IDA_D0 != "", "Please enter a value for the Dye")
      rwn(input$IDA_kHD != "", "Please enter a value for KaHD")
      rwn(!is_integer(input$IDA_sens_bounds), "Please enter an integer value for the sensitivity boundary")
    }

    observeEvent(input$IDA_Start_Sensi, {
      if (nclicks_sense() != 0 | nclicks() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      nclicks_sense(nclicks_sense() + 1)
      result_val_sense(data.frame(Status = "Running..."))
      session$sendCustomMessage(type = "IDAclearFieldSense", list(message = NULL, arg = 1))
      com_sense$running()
      check_inputs_sensi()
      req(length(opti_done()))
      additionalParameters <- convert_all_to_num(
        "Additional Parameters",
        input$IDA_H0, input$IDA_D0, input$IDA_kHD
      )
      optim_params <- result_val()$parameter
      sense_bounds <- input$IDA_sens_bounds
      fl()
      sensi_done(FALSE)
      result_sense <- future(
        {
          sensitivity("ida", optim_params, df, additionalParameters,
            sense_bounds,
            runAsShiny = com_sense
          )
        },
        seed = TRUE
      )
      promises::`%...>%`(result_sense, result_val_sense())
      result_sense <- catch(
        result_sense,
        function(e) {
          result_val_sense(NULL)
          print(e$message)
          print_noti(e$message, type = "error", duration = 0)
        }
      )
      result_sense <- finally(
        result_sense,
        function() {
          com_sense$ready()
          nclicks_sense(0)
          sensi_done(TRUE)
        }
      )
      NULL
    })

    observeEvent(input$IDA_cancel_sense, {
      exportTestValues(
        cancel_sense_clicked = TRUE
      )
      com_sense$interrupt()
    })

    observeEvent(input$IDA_status_sense, {
      req(nclicks_sense() != 0)
      exportTestValues(
        status_sense = {
          com_sense$getStatus()
        }
      )
      session$sendCustomMessage(
        type = "IDAupdateFieldSense",
        list(message = com_sense$getStatus())
      )
    })

    output$IDA_sensi <- renderPlot({
      sensi_success()
      exportTestValues(
        sense_plot = {
          result_val_sense()
        }
      )
      result_val_sense()
    })

    output$IDA_sensi_download <- downloadHandler(
      filename = function() {
        "result.xlsx"
      },
      content = function(file) {
        sensi_success()
        wb <- openxlsx::createWorkbook()
        addWorksheet(wb, "Results")
        tempfile_plot <- tempfile(fileext = ".png")
        if (inherits(result_val_sense(), "ggplot")) {
          curr_val <- result_val_sense()
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
    destroy_files <- reactive({
      l <- com_batch$list
      if (length(l) >= 1) {
        lapply(l, function(x) {
          x$destroy()
        })
      }
      com_batch$list <- NULL
      return()
    })

    num_rep_batch <- reactiveVal()
    check_inputs_batch <- function() {
      rwn(!is.na(input$NumRepDataset), "Please provide a number of replicates/dataset")
      rwn(!is_integer(input$NumRepDataset), "Please provide an integer entry for the replicates/dataset")
      rwn(length(df_list) > 0, "The dataset list seems to be empty. Please upload a file")
    }

    observeEvent(input$IDA_Start_Batch, {
      # Check running analysis
      if (nclicks() != 0 | nclicks_sense() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      session$sendCustomMessage(type = "IDAclearFieldBatch", list(message = NULL))
      nclicks(nclicks() + 1)
      result_val(data.frame(Status = "Running..."))
      session$sendCustomMessage(type = "IDAclearFieldBatch", list(message = NULL, arg = 1))
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
      seed_case <- NULL
      if (num_rep > 1 && !is.na(seed)) {
        print_noti("Found number of replications > 1 and a seed was defined.
          Only for the first analysis of each dataset respectivly,
          the seed which will be used.")
      }
      if (is.na(seed)) {
        seed_case <- 1
      } else {
        if (num_rep > 1) {
          seed_case <- 2
        } else {
          seed_case <- 3
        }
      }
      seed_origin <- NULL
      if (seed_case == 3) {
        seed_origin <- seed
      }
      # clear everything
      batch_done(FALSE)
      fl()
      output$IDA_batch_data_plot <- renderPlot({
        plot.new()
      })
      output$IDA_batch_params_plot <- renderPlot({
        plot.new()
      })
      output$IDA_batch_metrices_plot <- renderPlot({
        plot.new()
      })
      session$sendCustomMessage(
        type = "IDAupdateFieldBatch",
        list(message = "Initialisation")
      )
      reactive({
        destroy_files()
      })

      size <- length(df_list) * num_rep
      promises_list <- vector("list", size)
      seeds <- numeric(size)
      seeds_from <- 1:1e6

      com_batch$list <- reactive({
        lapply(
          seq_along(1:size),
          function(x) {
            Communicator$new()
          }
        )
      })

      cl <- com_batch$list()

      # TODO: this is a massive load for the server. Somehow this has to be monitored and controlled
      plan(multicore, workers = 10)
      for (i in seq_len(size)) {
        print(paste0("Iter: ", i)) # TODO: remove
        if (seed_case == 1) {
          seed <- sample(seeds_from, 1)
        } else if (seed_case == 2) {
          if (i %in% seq(1, size, num_rep)) {
            seed <- seed_origin
          } else {
            seed <- sample(seeds_from, 1)
          }
        }
        seeds[i] <- seed
        promises_list[[i]] <- future(
          {
            opti(
              case = "ida", lowerBounds = lb, upperBounds = ub,
              df_list[[(i - 1) %% length(df_list) + 1]],
              additionalParameters,
              seed = seed, npop = npop, ngen = ngen,
              Topology = topo,
              errorThreshold = et, runAsShiny = cl[[i]]
            )
          },
          seed = NULL
        )
      }
      session$sendCustomMessage(
        type = "IDAupdateFieldBatch",
        list(message = "")
      )

      result_val_batch$result <- promises_list

      promises::promise_map(promises_list, function(promise) {
        catch(promise, function(e) {
          result_val_batch$result(NULL)
          print(e$message)
          showNotification(e$message, duration = 0)
        })
      })

      promises::promise_map(promises_list, function(promise) {
        finally(promise, function() {
          nclicks(0)
          batch_done(TRUE)
        })
      })

      NULL
    })

    observeEvent(input$IDA_cancel_Batch, {
      exportTestValues(
        cancel_clicked_batch = TRUE
      )
      req(nclicks() != 0)
      if (is.null(result_val_batch$result) || is.null(com_batch$list)) {
        nclicks(0)
        batch_done(TRUE)
        return(NULL)
      }
      lapply(com_batch$list(), function(com) {
        com$interrupt()
      })
    })

    observeEvent(input$IDA_status_Batch, {
      req(nclicks() != 0)
      req(!is.null(result_val_batch$result))
      req(!is.null(com_batch$list))
      counter_dataset <- 0
      counter_rep <- 0
      temp_status <- character(length(com_batch$list()))
      for (i in seq_along(com_batch$list())) {
        if (((i - 1) %% num_rep_batch()) == 0) {
          counter_dataset <- counter_dataset + 1
          counter_rep <- 1
        } else {
          counter_rep <- counter_rep + 1
        }
        temp_status[i] <- paste0(
          "Dataset Nr.: ", counter_dataset,
          "; Replication Nr.:", counter_rep,
          "; ", com_batch$list()[[i]]$getData()
        )
      }
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
      m <- tryCatch(Reduce(bind, temp_status), error = function(e) {
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
      values <- lapply(result_val_batch$result, function(fut) {
        value(fut)
      })
      result_val_batch$result_splitted <- seperate_batch_results(values)
    })

    observeEvent(req(batch_success()), {
      list <- plot_data()
      p1 <- plotStates(list, num_rep_batch())
      p2 <- plotParams(list, num_rep_batch())
      p3 <- plotMetrices(list, num_rep_batch())
      output$IDA_batch_data_plot <- renderPlot({
        p1
      })
      output$IDA_batch_params_plot <- renderPlot({
        p2
      })
      output$IDA_batch_metrices_plot <- renderPlot({
        p3
      })
    })

    output$IDA_batch_download <- downloadHandler(
      filename = function() {
        "result.xlsx"
      },
      content = function(file) {
        batch_success()
        download_batch_file(
          file,
          result_val_batch$result_splitted,
          num_rep_batch()
        )
      }
    )
  })
}
