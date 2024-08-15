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
    fl <- reactive({
      flush(com$result)
      return()
    })

    # reactive values
    # ===============================================================================
    result_val <- reactiveVal()
    result_val_sense <- reactiveVal()
    result_val_batch <- reactiveValues(result = NULL)
    add_info <- reactiveVal()
    batch_done <- reactiveVal(FALSE)


    # Optimization
    # ===============================================================================
    check_inputs <- function() {
      rwn(input$IDA_H0 != "", "Please enter a value for the Host")
      rwn(input$IDA_D0 != "", "Please enter a value for the Dye")
      rwn(input$IDA_kHD != "", "Please enter a value for KaHD")
      rwn(input$IDA_npop != "", "Please enter a value for number of particles") # TODO: numeric input
      rwn(input$IDA_ngen != "", "Please enter a value for the number of generations") # TODO: numeric input
      rwn(input$IDA_threshold != "", "Please enter a value for the error threshold") # TODO: numeric input
      rwn(input$IDA_kHD_lb != "", "Please enter a value for the lower boundary of KaHG")
      rwn(input$IDA_kHD_ub != "", "Please enter a value for the upper boundary of KaHG")
      rwn(input$IDA_IHD_lb != "", "Please enter a value for the lower boundary of I(HD)")
      rwn(input$IDA_IHD_ub != "", "Please enter a value for the upper boundary of I(HD)")
      rwn(input$IDA_ID_lb != "", "Please enter a value for the lower boundary of I(D)")
      rwn(input$IDA_ID_ub != "", "Please enter a value for the upper boundary of I(D)")
      rwn(input$IDA_I0_lb != "", "Please enter a value for the lower boundary of I(0)")
      rwn(input$IDA_I0_ub != "", "Please enter a value for the upper boundary of I(0)")
    }
    observeEvent(input$IDA_Start_Opti, {
      if (nclicks() != 0 | nclicks_sense() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      session$sendCustomMessage(type = "IDAclearField", list(message = NULL))
      nclicks(nclicks() + 1)
      result_val(data.frame(Status = "Running...")) # TODO: is this required check
      com$running()
      session$sendCustomMessage(type = "IDAclearField", list(message = NULL, arg = 1)) #TODO: check why is this done twice
      check_inputs()
      lb <- convert_all_to_num("lower boundaries", 
        input$IDA_kHD_lb, input$IDA_I0_lb, input$IDA_IHD_lb, input$IDA_ID_lb)
      ub <- convert_all_to_num("upper boundaries",
        input$IDA_kHD_ub, input$IDA_I0_ub, input$IDA_IHD_ub, input$IDA_ID_ub)
      additionalParameters <- convert_all_to_num("Additional Parameters", 
        input$IDA_H0, input$IDA_D0, input$IDA_kHD)
      npop <- input$IDA_npop
      ngen <- input$IDA_ngen
      topo <- input$IDA_topology
      et <- input$IDA_threshold
      seed <- input$Seed
      if (is.na(seed)) seed <- as.numeric(Sys.time())
      fl()
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
      promises::`%...>%`(result, result_val())
      result <- catch(
        result,
        function(e) {
          result_val(NULL)
          print(e$message)
          showNotification(e$message, duration = 0)
        }
      )
      result <- finally(
        result,
        function() {
          com$ready()
          nclicks(0)
        }
      )

      add_info(list(
        lb, ub,
        additionalParameters, npop, ngen, topo, et, seed
      ))

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
      if(length(nchar(m)) == 0) m <- "Initialisation"
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
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[2]]))
      res <- result_val()[[2]]
      names(res)[1] <- c("K<sub>a</sub>(HG) [M]")
      names(res)[2] <- c("I(0)")
      names(res)[3] <- c("I(HD) [1/M]")
      names(res)[4] <- c("I(D) [1/M]")

      exportTestValues(
        df_params = res
      )

      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 3)
    })
    output$IDA_plot <- renderPlot({
      req(length(result_val()) == 11)
      req(!is.null(result_val()[[3]]))
      result_val()[[3]]
    })
    output$IDA_metrices <- renderDT({
      req(length(result_val()) == 11)
      req(!is.null(result_val()[[4]]))
      res <- as.data.frame(result_val()[[4]])
      names(res)[1] <- c("MeanSquareError")
      names(res)[2] <- c("RootMeanSquareError")
      names(res)[3] <- c("MeanAbsoluteError")
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
        req(length(result_val()) == 11)

        if (input$file_type == "xlsx") {
          wb <- openxlsx::createWorkbook()
          addWorksheet(wb, "Results")
          writeData(wb, "Results",
            "Model: IDA",
            startCol = 1,
            startRow = 1
          )

          curr_row <- 3
          curr_val <- result_val()[[1]]
          names(curr_val) <- c(
            "total Guest measured [M]", "Signal measured",
            "Signal simulated", "free Dye simulated [M]", "Host-Dye simulated [M]"
          )
          writeData(wb, "Results", curr_val, startRow = curr_row)
          curr_row <- curr_row + dim(curr_val)[1] + 5

          curr_val <- result_val()[[2]]
          ai <- add_info()
          ai[[1]] <- as.data.frame(t(ai[[1]]))
          ai[[2]] <- as.data.frame(t(ai[[2]]))
          names(ai[[1]]) <- names(curr_val)
          names(ai[[2]]) <- names(curr_val)
          curr_val <- rbind(curr_val, ai[[1]])
          curr_val <- rbind(curr_val, ai[[2]])
          names(curr_val)[1] <- c("Ka(HG) [M]")
          names(curr_val)[2] <- c("I(0)")
          names(curr_val)[3] <- c("I(HD) [1/M]")
          names(curr_val)[4] <- c("I(D) [1/M]")
          curr_val <- cbind(
            info = c("Opti. results", "lower boundaries", "upper boundaries"),
            curr_val
          )
          writeData(wb, "Results", curr_val, startRow = curr_row)
          curr_row <- curr_row + dim(curr_val)[1] + 5

          curr_val <- as.data.frame(result_val()[[4]])
          names(curr_val)[1] <- c("MeanSquareError")
          names(curr_val)[2] <- c("RootMeanSquareError")
          names(curr_val)[3] <- c("MeanAbsoluteError")
          names(curr_val)[4] <- c("R2")
          names(curr_val)[5] <- c("R2 adjusted")
          writeData(wb, "Results", curr_val, startRow = curr_row)
          curr_row <- curr_row + dim(curr_val)[1] + 5

          curr_val <- result_val()[[3]]
          tempfile_plot <- tempfile(fileext = ".png")
          ggsave(tempfile_plot,
            plot = curr_val, width = 10, height = 6
          )
          insertImage(wb, "Results", tempfile_plot, startRow = curr_row)
          curr_row <- curr_row + 15

          str(ai[[3]])
          curr_val <- data.frame(
            Host = ai[[3]][1], Dye = ai[[3]][2], KaHD = ai[[3]][3],
            npop = ai[[4]], ngen = ai[[5]], topology = ai[[6]],
            error_threshold = ai[[7]], seed = ai[[8]]
          )
          names(curr_val)[1] <- c("Host [M]")
          names(curr_val)[2] <- c("Dye [M]")
          names(curr_val)[3] <- c("Ka(HD) [1/M]")
          writeData(
            wb, "Results",
            curr_val,
            startRow = curr_row
          )
          curr_row <- curr_row + 5

          writeData(wb, "Results",
            as.data.frame(R.Version()),
            startRow = curr_row
          )
          curr_row <- curr_row + 5

          writeData(wb, "Results",
            paste0("tsf version: ", packageVersion("tsf")),
            startRow = curr_row
          )

          openxlsx::saveWorkbook(wb, file)
          unlink(tempfile_plot)
        } else {
          # csv file
          write.table("Model: IDA", file)
          curr_val <- result_val()[[1]]
          names(curr_val) <- c(
            "total Host measured [M]", "Signal measured",
            "Signal simulated", "free Dye simulated [M]", "Host-Dye simulated [M]"
          )
          write.table(curr_val, file,
            append = TRUE,
            sep = ",", row.names = FALSE
          )

          curr_val <- result_val()[[2]]
          ai <- add_info()
          ai[[1]] <- as.data.frame(t(ai[[1]]))
          ai[[2]] <- as.data.frame(t(ai[[2]]))
          names(ai[[1]]) <- names(curr_val)
          names(ai[[2]]) <- names(curr_val)
          curr_val <- rbind(curr_val, ai[[1]])
          curr_val <- rbind(curr_val, ai[[2]])
          names(curr_val)[1] <- c("Ka(HG) [M]")
          names(curr_val)[2] <- c("I(0)")
          names(curr_val)[3] <- c("I(HD) [1/M]")
          names(curr_val)[4] <- c("I(D) [1/M]")
          curr_val <- cbind(
            info = c("Opti. results", "lower boundaries", "upper boundaries"),
            curr_val
          )
          write.table(curr_val, file,
            append = TRUE,
            sep = ",", row.names = FALSE
          )

          curr_val <- as.data.frame(result_val()[[4]])
          names(curr_val)[1] <- c("MeanSquareError")
          names(curr_val)[2] <- c("RootMeanSquareError")
          names(curr_val)[3] <- c("MeanAbsoluteError")
          names(curr_val)[4] <- c("R2")
          names(curr_val)[5] <- c("R2 adjusted")
          write.table(curr_val, file,
            append = TRUE,
            sep = ",", row.names = FALSE
          )

          curr_val <- data.frame(
            Host = ai[[3]][1], Dye = ai[[3]][2], KaHD = ai[[3]][3],
            npop = ai[[4]], ngen = ai[[5]], topology = ai[[6]],
            error_threshold = ai[[7]], seed = ai[[8]]
          )
          names(curr_val)[1] <- c("Host [M]")
          names(curr_val)[2] <- c("Dye [M]")
          names(curr_val)[3] <- c("Ka(HD) [1/M]")
          write.table(curr_val, file,
            append = TRUE,
            sep = ",", row.names = FALSE
          )

          curr_val <- as.data.frame(R.Version())
          write.table(curr_val, file,
            append = TRUE,
            sep = ",", row.names = FALSE
          )
        }
      }
    )

    # sensitivity
    # ===============================================================================

    observeEvent(input$IDA_Start_Sensi, {
      if (nclicks_sense() != 0 | nclicks() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      nclicks_sense(nclicks_sense() + 1)
      result_val_sense(data.frame(Status = "Running..."))
      session$sendCustomMessage(type = "IDAclearFieldSense", list(message = NULL, arg = 1))
      com_sense$running()
      req(input$IDA_H0)
      req(input$IDA_D0)
      req(input$IDA_kHD)
      req(input$IDA_sens_bounds)
      req(length(result_val()) == 4)
      additionalParameters <- c(input$IDA_H0, input$IDA_D0, input$IDA_kHD)
      additionalParameters <- convertToNum(additionalParameters)
      req(!("Error" %in% additionalParameters))
      optim_params <- result_val()[[2]]
      sense_bounds <- input$IDA_sens_bounds
      fl()
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
          showNotification(e$message, duration = 0)
        }
      )
      result_sense <- finally(
        result_sense,
        function() {
          com_sense$ready()
          nclicks_sense(0)
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
      req(inherits(result_val_sense(), "ggplot"))
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
      if(length(l) >= 1) {
        lapply(l, function(x) {
          x$destroy()
        })
      }
      com_batch$list <- NULL
      return()
    })
    
    num_rep_batch <- reactiveVal()

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
      req(input$NumRepDataset)
      lb <- c(input$IDA_kHD_lb, input$IDA_I0_lb, input$IDA_IHD_lb, input$IDA_ID_lb)
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
      # check seed case
      seed <- input$Seed
      num_rep <- as.integer(input$NumRepDataset)
      num_rep_batch(num_rep)
      seed_case <- NULL
      if(num_rep > 1 && !is.na(seed)) {
        showNotification("Found number of replications > 1 and a seed was defined.
          Only for the first analysis of each dataset respectivly,
          the seed which will be used.")
      }
      if (is.na(seed)) {
        seed_case <- 1
      } else {
        if(num_rep > 1) {
          seed_case <- 2
        } else {
          seed_case <- 3
        }
      }
      seed_origin <- NULL
      if(seed_case == 3) {
       seed_origin <- seed
      }
      # clear everything
      batch_done(FALSE)
      fl()
      output$IDA_batch <- renderPlot({
        plot.new()
      })
      session$sendCustomMessage(
        type = "IDAupdateFieldBatch",
        list(message = "Initialisation")
      )
      reactive({destroy_files()})

      size <- length(df_list) * num_rep
      promises_list <- vector("list", size)
      seeds <- numeric(size)
      seeds_from <- 1:1e6

      com_batch$list <- reactive({lapply(seq_along(1:size),
        function(x) {
        Communicator$new()
      })})

      cl <- com_batch$list()

      for (i in seq_len(size)) {
        if(seed_case == 1) {
          seed <- sample(seeds_from, 1)
        } else if(seed_case == 2) {
          if(i %in% seq(1, size, num_rep)) {
            seed <- seed_origin
          } else {
            seed <- sample(seeds_from, 1)
          }
        }
        seeds[i] <- seed
        promises_list[[i]] <- future({
          opti(
            case = "ida", lowerBounds = lb, upperBounds = ub,
            df_list[[(i - 1) %% length(df_list) + 1]],
            additionalParameters,
            seed = seed, npop = npop, ngen = ngen,
            Topology = topo,
            errorThreshold = et, runAsShiny = cl[[i]]
          )
        }, seed = NULL)
      }
      session$sendCustomMessage(
        type = "IDAupdateFieldBatch",
        list(message = "")
      )

      result_val_batch$result <- promises_list

      promises::promise_map(promises_list, function(promise) {
        catch(promise, function(e) {
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

      add_info(list(
        lb, ub,
        additionalParameters,
        npop, ngen, topo, et, seeds
      ))

      NULL
    })

    observeEvent(input$IDA_cancel_Batch, {
      exportTestValues(
        cancel_clicked_batch = TRUE
      )
      lapply(com_batch$list(), function(com) {
        com$interrupt()
      })
    })
    observeEvent(input$IDA_status_Batch, {
      req(nclicks() != 0)
      counter_dataset <- 0
      counter_rep <- 0
      temp_status <- character(length(com_batch$list()))
      for (i in seq_along(com_batch$list())) {
        if ( ((i - 1) %% num_rep_batch()) == 0) {
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
        if(is.null(a) && is.null(b)) return("Initialisation")
        if(is.null(a)) return(b)
        if(is.null(b)) return(a)
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
      seperate_batch_results(values)
    })

    observeEvent(req(batch_done()), {
      list <- plot_data()
      p <- plotStates(list, num_rep_batch()) / 
        plotParams(list, num_rep_batch()) + plotMetrices(list, num_rep_batch())
      output$IDA_batch <- renderPlot({
        p
      })
    })

  })

}
