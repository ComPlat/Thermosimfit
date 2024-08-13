idaUI <- function(id) {
  tabItem(
    tabName = "IDA",
    tags$script(
      "Shiny.addCustomMessageHandler('IDAupdateField', function(message) {
              var result = message.message;
              $('#IDA-IDA_output').append(result + '\\n');
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAclearField', function(message) {
              $('#IDA-IDA_output').empty();
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAupdateFieldSense', function(message) {
              var result = message.message;
              $('#IDA-IDA_output_sense').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAclearFieldSense', function(message) {
              $('#IDA-IDA_output_sense').empty();
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAupdateFieldBatch', function(message) {
              var result = message.message;
              $('#IDA-IDA_output_Batch').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAclearFieldBatch', function(message) {
              $('#IDA-IDA_output_Batch').empty();
            });"
    ),
    fluidRow(
      box(
        textInput(NS(id, "IDA_H0"), "Host conc. [M]", value = 0),
        textInput(NS(id, "IDA_D0"), "Dye conc. [M]", value = "0"),
        textInput(NS(id, "IDA_kHD"), HTML("K<sub>a</sub>(HD) [1/M]"), value = "0"),
        box(
          title = "Advanced options",
          collapsible = TRUE, collapsed = TRUE,
          box(
            numericInput(NS(id, "IDA_npop"), "Number of particles", value = 40),
            numericInput(NS(id, "IDA_ngen"), "Number of generations", value = 1000)
          ),
          box(
            selectInput(NS(id, "IDA_topology"), "Topology of particle swarm",
              c(
                "star" = "star",
                "random arbitrary neighberhood" = "random"
              ),
              selected = "random",
              selectize = FALSE
            ),
            numericInput(NS(id, "IDA_threshold"),
              "Threshold of the error",
              value = 0.00001
            ),
            numericInput(NS(id, "Seed"), "Seed which should be set", value = NULL)
          ),
          width = 12
        ),
        width = 6,
        title = "Parameter", solidHeader = TRUE,
        status = "warning", height = 575
      ),
      box(
        box(
          textInput(NS(id, "IDA_kHD_lb"), HTML("K<sub>a</sub>(HG) value lower boundary [1/M]"), value = 10),
          textInput(NS(id, "IDA_kHD_ub"), HTML("K<sub>a</sub>(HG) value upper boundary [1/M]"), value = 1e08)
        ),
        box(
          textInput(NS(id, "IDA_I0_lb"), "I(0) value lower boundary", value = 0),
          textInput(NS(id, "IDA_I0_ub"), "I(0) value upper boundary", value = 1e08)
        ),
        box(
          textInput(NS(id, "IDA_IHD_lb"),
            label = tagList(
              "I(HD) value lower boundary [1/M]",
              actionButton(NS(id, "AdviceUBIHD"), "Help",
                icon = icon("question-circle"),
                style = "background-color:transparent; border:none;"
              )
            ), value = 0
          ),
          textInput(NS(id, "IDA_IHD_ub"), "I(HD) value upper boundary [1/M]", value = 1e08)
        ),
        box(
          textInput(NS(id, "IDA_ID_lb"), "I(D) value lower boundary [1/M]", value = 0),
          textInput(NS(id, "IDA_ID_ub"), "I(D) value upper boundary [1/M]", value = 1e08)
        ),
        width = 6,
        title = tagList(
          "Boundaries",
          actionButton(NS(id, "helpButton"), "Help",
            icon = icon("question-circle"),
            style = "background-color:transparent; border:none;"
          )
        ),
        solidHeader = TRUE,
        status = "warning", height = 575
      )
    ),
    fluidRow(
      tabBox(
        id = NS(id, "ResultPanel"),
        tabPanel(
          "Optimization",
          fluidRow(
            box(
              box(
                actionButton(NS(id, "IDA_Start_Opti"), "Start Optimization"),
                actionButton(NS(id, "IDA_cancel"), "Stop Optimization"),
                actionButton(NS(id, "IDA_status"), "Get Status"),
                downloadButton(NS(id, "IDA_download"), "Save result of optimization"),
                selectInput(NS(id, "file_type"), "Choose file type:",
                  choices = c("Excel" = "xlsx", "CSV" = "csv")
                ),
                verbatimTextOutput(NS(id, "IDA_output")),
                width = 12
              ),
              box(
                br(),
                DT::DTOutput(NS(id, "IDA_params")),
                DT::DTOutput(NS(id, "IDA_metrices")),
                plotOutput(NS(id, "IDA_plot")),
                width = 7, solidHeader = TRUE, status = "warning"
              ),
              width = 12, title = "Optimization", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "Sensitivity analysis",
          fluidRow(
            box(
              box(
                numericInput(NS(id, "IDA_sens_bounds"), "+/- boundary in [%]", value = 15),
                actionButton(NS(id, "IDA_Start_Sensi"), "Start Sensitivity analysis"),
                actionButton(NS(id, "IDA_cancel_sense"), "Cancel"),
                actionButton(NS(id, "IDA_status_sense"), "Get Status"),
                downloadButton(NS(id, "IDA_sensi_download"), "Save result of sensitivity analysis"),
                verbatimTextOutput(NS(id, "IDA_output_sense")),
                width = 12
              ),
              box(
                br(),
                plotOutput(NS(id, "IDA_sensi")),
                width = 7, solidHeader = TRUE, status = "warning"
              ),
              width = 12, title = "Sensitivity analysis", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "Batch processing",
          fluidRow(
            box(
              box(
                actionButton(NS(id, "IDA_Start_Batch"), "Start batch analysis"),
                actionButton(NS(id, "IDA_cancel_Batch"), "Cancel"),
                actionButton(NS(id, "IDA_status_Batch"), "Get Status"),
                downloadButton(NS(id, "IDA_batch_download"), "Save result of batch analysis"),
                verbatimTextOutput(NS(id, "IDA_output_Batch")),
                width = 12
              ),
              box(
                br(),
                plotOutput(NS(id, "IDA_batch"), width = "1500px", height = "400px"),
                width = 12, solidHeader = TRUE, status = "warning"
              ),
              width = 12, title = "Batch analysis", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        width = 12
      )
    )
  )
}



idaServer <- function(id, df, df_list, com, com_sense, com_batch,
                      nclicks, nclicks_sense, iter) {
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
    result_val <- reactiveVal()
    result_val_sense <- reactiveVal()
    result_val_batch <- reactiveValues(result = NULL)
    add_info <- reactiveVal()
    iter <- reactiveVal()
    batch_done <- reactiveVal(FALSE)

    fl <- reactive({
      flush(com$result)
      return()
    })

    observeEvent(input$IDA_Start_Opti, {
         if (nclicks() != 0 | nclicks_sense() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      session$sendCustomMessage(type = "IDAclearField", list(message = NULL))
      nclicks(nclicks() + 1)
      result_val(data.frame(Status = "Running..."))
      com$running()
      session$sendCustomMessage(type = "IDAclearField", list(message = NULL, arg = 1))
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
      seed <- input$Seed
      if (is.na(seed)) seed <- as.numeric(Sys.time())
      fl()
      iter <- 1
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
      exportTestValues(
        status1 = {
          com$getData()
        }
      )
      i <- iter()

      if (!is.null(i)) {
        if (i != extract_iter(m)) {
          session$sendCustomMessage(
            type = "IDAupdateField",
            list(message = m)
          )
          iter(extract_iter(m))
        }
      } else {
        if (is.null(extract_iter(m))) {
          session$sendCustomMessage(
            type = "IDAupdateField",
            list(message = "Initialisation")
          )
        }
        exportTestValues(
          status2 = "Initialisation"
        )

        iter(extract_iter(m))
      }
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
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[3]]))
      result_val()[[3]]
    })
    output$IDA_metrices <- renderDT({
      req(length(result_val()) == 4)
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
        caption = "Error Metrics: Comparison of in silico signal and seasured signal"
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
        req(length(result_val()) == 4)

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
    current_df <- reactiveVal()

    observeEvent(input$IDA_Start_Batch, {
      if (nclicks() != 0 | nclicks_sense() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      session$sendCustomMessage(type = "IDAclearFieldBatch", list(message = NULL))
      nclicks(nclicks() + 1)
      result_val(data.frame(Status = "Running..."))
      # com_batch$running() # TODO:has to be different handled
      session$sendCustomMessage(type = "IDAclearFieldBatch", list(message = NULL, arg = 1))
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
      seed <- input$Seed
      if (is.na(seed)) seed <- as.numeric(Sys.time())
      fl()
      output$IDA_batch <- renderPlot({
        plot.new()
      })
      iter <- 1

      promises_list <- vector("list", length(df_list))
      com_batch$list <- reactive({lapply(seq_along(df_list), function(x) {
        Communicator$new()
      })})
      cl <-com_batch$list()
      for (i in seq_along(df_list)) {
        if (!is.null(seed)) seed <- as.numeric(Sys.time())
        promises_list[[i]] <- future({
          opti(
            case = "ida", lowerBounds = lb, upperBounds = ub,
            df_list[[i]], additionalParameters,
            seed = seed, npop = npop, ngen = ngen,
            Topology = topo,
            errorThreshold = et, runAsShiny = cl[[i]]
          )
        }, seed = NULL)
      }

    result_val_batch$result <- promises_list

      promises::promise_map(promises_list, function(promise) {
        catch(promise, function(e) {
          print(e$message)
          showNotification(e$message, duration = 0)
        })
      })

      promises::promise_map(promises_list, function(promise) {
        finally(promise, function() {
          # TODO: set all com batch to ready
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
      lapply(com_batch$list(), function(com) {
        com$interrupt()
      })
    })
    observeEvent(input$IDA_status_Batch, {
      # req(nclicks() != 0) # TODO: uncomment
      temp_status <- lapply(com_batch$list(), function(com) {
        paste0("Dataset Nr.: ", parent.frame()$i[], " ", com$getData())
      })
      bind <- function(a, b) {
        if(is.null(a) && is.null(b)) return("")
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

    observeEvent(req(batch_done()), 
      {
      req(batch_done())
      batch_done(FALSE)
      output$IDA_batch <- renderPlot({
        list <- plot_data()
        plotStates(list) + plotParams(list) + plotMetrices(list)
      })
    })

  })




}
