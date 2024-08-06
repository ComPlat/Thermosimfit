dbaUI <- function(id) {
  tabItem(
    tabName = "DBA",
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateField', function(message) {
              var result = message.message;
              $('#DBA-DBA_output').append(result + '\\n');
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAclearField', function(message) {
              $('#DBA-DBA_output').empty();
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldSense', function(message) {
              var result = message.message;
              $('#DBA-DBA_output_sense').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAclearFieldSense', function(message) {
              $('#DBA-DBA_output_sense').empty();
            });"
    ),
    fluidRow(
      box(
        textInput(NS(id, "DBA_D0"), "Dye conc. [M]", value = 0),
        box(
          title = "Advanced options",
          collapsible = TRUE, collapsed = TRUE,
          box(
            numericInput(NS(id, "DBA_npop"), "Number of particles", value = 40),
            numericInput(NS(id, "DBA_ngen"), "Number of generations", value = 1000)
          ),
          box(
            selectInput(NS(id, "DBA_topology"), "Topology of particle swarm",
              c(
                "star" = "star",
                "random arbitrary neighberhood" = "random"
              ),
              selected = "random",
              selectize = FALSE
            ),
            numericInput(NS(id, "DBA_threshold"), "Threshold of the error", value = 0.00001),
            numericInput(NS(id, "Seed"), "Seed which should be set", value = NULL)
          ),
          width = 12
        ),
        width = 6,
        title = "Parameter", solidHeader = TRUE,
        status = "warning", height = 475
      ),
      box(
        box(
          textInput(NS(id, "DBA_kHD_lb"), HTML("K<sub>a</sub>(HD) value lower boundary [1/M]"), value = 10),
          textInput(NS(id, "DBA_kHD_ub"), HTML("K<sub>a</sub>(HD) value upper boundary [1/M]"), value = 1e08)
        ),
        box(
          textInput(NS(id, "DBA_I0_lb"), "I(0) value lower boundary", value = 0),
          textInput(NS(id, "DBA_I0_ub"), "I(0) value upper boundary", value = 1e08)
        ),
        box(
          textInput(NS(id, "DBA_IHD_lb"),
            label = tagList(
              "I(HD) value lower boundary [1/M]",
              actionButton(NS(id, "AdviceUBIHD"), "Help",
                icon = icon("question-circle"),
                style = "background-color:transparent; border:none;"
              )
            ), value = 0
          ),
          textInput(NS(id, "DBA_IHD_ub"), "I(HD) value upper boundary [1/M]", value = 1e08)
        ),
        box(
          textInput(NS(id, "DBA_ID_lb"), "I(D) value lower boundary [1/M]", value = 0),
          textInput(NS(id, "DBA_ID_ub"), "I(D) value upper boundary [1/M]", value = 1e08)
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
        status = "warning", height = 475
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
                actionButton(NS(id, "DBA_Start_Opti"), "Start Optimization"),
                actionButton(NS(id, "DBA_cancel"), "Stop Optimization"),
                actionButton(NS(id, "DBA_status"), "Get Status"),
                downloadButton(NS(id, "DBA_download"), "Save result of optimization"),
                selectInput(NS(id, "file_type"), "Choose file type:",
                  choices = c("Excel" = "xlsx", "CSV" = "csv")
                ),
                verbatimTextOutput(NS(id, "DBA_output")),
                width = 12
              ),
              box(
                br(),
                DT::DTOutput(NS(id, "DBA_params")),
                DT::DTOutput(NS(id, "DBA_metrices")),
                plotOutput(NS(id, "DBA_plot")),
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
                numericInput(NS(id, "DBA_sens_bounds"), "+/- boundary in [%]", value = 15),
                actionButton(NS(id, "DBA_Start_Sensi"), "Start Sensitivity analysis"),
                actionButton(NS(id, "DBA_cancel_sense"), "Cancel"),
                actionButton(NS(id, "DBA_status_sense"), "Get Status"),
                downloadButton(NS(id, "DBA_sensi_download"), "Save result of sensitivity analysis"),
                verbatimTextOutput(NS(id, "DBA_output_sense")),
                width = 12
              ),
              box(
                br(),
                plotOutput(NS(id, "DBA_sensi")),
                width = 7, solidHeader = TRUE, status = "warning"
              ),
              width = 12, title = "Sensitivity analysis", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        width = 12
      )
    )
  )
}



dbaServer <- function(id, df, com, com_sense, nclicks, nclicks_sense) {
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

    result_val <- reactiveVal()
    result_val_sense <- reactiveVal()
    add_info <- reactiveVal()
    iter <- reactiveVal()

    fl <- reactive({
      flush(com$result)
      return()
    })

    observeEvent(input$DBA_Start_Opti, {
      if (nclicks() != 0 | nclicks_sense() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      session$sendCustomMessage(type = "DBAclearField", list(message = NULL))

      nclicks(nclicks() + 1)
      result_val(data.frame(Status = "Running..."))
      com$running()
      session$sendCustomMessage(type = "DBAclearField", list(message = NULL, arg = 1))
      req(input$DBA_D0)
      req(input$DBA_npop)
      req(input$DBA_ngen)
      req(input$DBA_threshold)
      req(input$DBA_kHD_lb)
      req(input$DBA_kHD_ub)
      req(input$DBA_IHD_lb)
      req(input$DBA_IHD_ub)
      req(input$DBA_ID_lb)
      req(input$DBA_ID_ub)
      req(input$DBA_I0_lb)
      req(input$DBA_I0_ub)
      lb <- c(input$DBA_kHD_lb, input$DBA_I0_lb, input$DBA_IHD_lb, input$DBA_ID_lb)
      lb <- convertToNum(lb)
      req(!("Error" %in% lb))
      ub <- c(input$DBA_kHD_ub, input$DBA_I0_ub, input$DBA_IHD_ub, input$DBA_ID_ub)
      ub <- convertToNum(ub)
      req(!("Error" %in% ub))
      additionalParameters <- c(input$DBA_D0)
      additionalParameters <- convertToNum(additionalParameters)
      req(!("Error" %in% additionalParameters))
      npop <- input$DBA_npop
      ngen <- input$DBA_ngen
      topo <- input$DBA_topology
      et <- input$DBA_threshold
      seed <- input$Seed
      if (is.na(seed)) seed <- as.numeric(Sys.time())
      fl()
      result <- future(
        {
          opti(
            "dba_dye_const", lb, ub, df, additionalParameters,
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
    observeEvent(input$DBA_cancel, {
      exportTestValues(
        cancel_clicked = TRUE
      )
      com$interrupt()
    })
    observeEvent(input$DBA_status, {
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
            type = "DBAupdateField",
            list(message = m)
          )
          iter(extract_iter(m))
        }
      } else {
        if (is.null(extract_iter(m))) {
          session$sendCustomMessage(
            type = "DBAupdateField",
            list(message = "Initialisation")
          )
        }
        exportTestValues(
          status2 = "Initialisation"
        )
        iter(extract_iter(m))
      }
    })
    output$DBA_params <- renderDT({
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[2]]))
      res <- result_val()[[2]]
      names(res)[1] <- c("K<sub>a</sub>(HD) [M]")
      names(res)[2] <- c("I(0)")
      names(res)[3] <- c("I(HD)")
      names(res)[4] <- c("I(D)")

      exportTestValues(
        df_params = res
      )

      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 3)
    })
    output$DBA_plot <- renderPlot({
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[3]]))
      result_val()[[3]]
    })
    output$DBA_metrices <- renderDT({
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
    output$DBA_download <- downloadHandler(
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
            "Model: DBA dye const",
            startCol = 1,
            startRow = 1
          )

          curr_row <- 3
          curr_val <- result_val()[[1]]
          names(curr_val) <- c(
            "total Host measured [M]", "Signal measured",
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
          names(curr_val)[1] <- c("Ka(HD) [M]")
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

          curr_val <- data.frame(
            Dye = ai[[3]],
            npop = ai[[4]], ngen = ai[[5]], topology = ai[[6]],
            error_threshold = ai[[7]], seed = ai[[8]]
          )
          names(curr_val)[1] <- c("Dye [M]")
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
          write.table("Model: DBA dye const", file)
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
          names(curr_val)[1] <- c("Ka(HD) [M]")
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
            Dye = ai[[3]],
            npop = ai[[4]], ngen = ai[[5]], topology = ai[[6]],
            error_threshold = ai[[7]], seed = ai[[8]]
          )
          names(curr_val)[1] <- c("Dye [M]")
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



    observeEvent(input$DBA_Start_Sensi, {
      if (nclicks_sense() != 0 | nclicks() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      nclicks_sense(nclicks_sense() + 1)
      result_val_sense(data.frame(Status = "Running..."))
      session$sendCustomMessage(type = "DBAclearFieldSense", list(message = NULL, arg = 1))
      com_sense$running()
      req(input$DBA_D0)
      req(input$DBA_sens_bounds)
      req(length(result_val()) == 4)
      additionalParameters <- c(input$DBA_D0)
      additionalParameters <- convertToNum(additionalParameters)
      req(!("Error" %in% additionalParameters))
      optim_params <- result_val()[[2]]
      sense_bounds <- input$DBA_sens_bounds
      fl()
      result_sense <- future(
        {
          sensitivity("dba_dye_const", optim_params, df, additionalParameters,
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
    observeEvent(input$DBA_cancel_sense, {
      exportTestValues(
        cancel_sense_clicked = TRUE
      )
      com_sense$interrupt()
    })
    observeEvent(input$DBA_status_sense, {
      req(nclicks_sense() != 0)
      exportTestValues(
        status_sense = {
          com_sense$getStatus()
        }
      )
      session$sendCustomMessage(
        type = "DBAupdateFieldSense",
        list(message = com_sense$getStatus())
      )
    })
    output$DBA_sensi <- renderPlot({
      req(inherits(result_val_sense(), "ggplot"))
      exportTestValues(
        sense_plot = {
          result_val_sense()
        }
      )
      result_val_sense()
    })
    output$DBA_sensi_download <- downloadHandler(
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
  })
}
