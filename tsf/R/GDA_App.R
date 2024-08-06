gdaUI <- function(id) {
  tabItem(
    tabName = "GDA",
    tags$script(
      "Shiny.addCustomMessageHandler('GDAupdateField', function(message) {
              var result = message.message;
              $('#GDA-GDA_output').append(result + '\\n');
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('GDAclearField', function(message) {
              $('#GDA-GDA_output').empty();
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('GDAupdateFieldSense', function(message) {
              var result = message.message;
              $('#GDA-GDA_output_sense').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('GDAclearFieldSense', function(message) {
              $('#GDA-GDA_output_sense').empty();
            });"
    ),
    fluidRow(
      box(
        textInput(NS(id, "GDA_H0"), "Host conc. [M]", value = 0),
        textInput(NS(id, "GDA_G0"), "Guest conc. [M]", value = "0"),
        textInput(NS(id, "GDA_kHD"), HTML("K<sub>a</sub>(HD) [1/M]"), value = "0"),
        box(
          title = "Advanced options",
          collapsible = TRUE, collapsed = TRUE,
          box(
            numericInput(NS(id, "GDA_npop"), "Number of particles", value = 40),
            numericInput(NS(id, "GDA_ngen"), "Number of generations", value = 1000)
          ),
          box(
            selectInput(NS(id, "GDA_topology"), "Topology of particle swarm",
              c(
                "star" = "star",
                "random arbitrary neighberhood" = "random"
              ),
              selected = "random",
              selectize = FALSE
            ),
            numericInput(NS(id, "GDA_threshold"), "Threshold of the error", value = 0.00001),
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
          textInput(NS(id, "GDA_kHD_lb"), HTML("K<sub>a</sub>(HG) value lower boundary [1/M]"), value = 10),
          textInput(NS(id, "GDA_kHD_ub"), HTML("K<sub>a</sub>(HG) value upper boundary [1/M]"), value = 1e08)
        ),
        box(
          textInput(NS(id, "GDA_I0_lb"), "I(0) value lower boundary", value = 0),
          textInput(NS(id, "GDA_I0_ub"), "I(0) value upper boundary", value = 1e08)
        ),
        box(
          textInput(NS(id, "GDA_IHD_lb"), label = tagList(
            "I(HD) value lower boundary [1/M]",
            actionButton(NS(id, "AdviceUBIHD"), "Help",
              icon = icon("question-circle"),
              style = "background-color:transparent; border:none;"
            )
          ), value = 0),
          textInput(NS(id, "GDA_IHD_ub"), "I(HD) value upper boundary [1/M]", value = 1e08)
        ),
        box(
          textInput(NS(id, "GDA_ID_lb"), "I(D) value lower boundary [1/M]", value = 0),
          textInput(NS(id, "GDA_ID_ub"), "I(D) value upper boundary [1/M]", value = 1e08)
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
                actionButton(NS(id, "GDA_Start_Opti"), "Start Optimization"),
                actionButton(NS(id, "GDA_cancel"), "Stop Optimization"),
                actionButton(NS(id, "GDA_status"), "Get Status"),
                downloadButton(NS(id, "GDA_download"), "Save result of optimization"),
                selectInput(NS(id, "file_type"), "Choose file type:",
                  choices = c("Excel" = "xlsx", "CSV" = "csv")
                ),
                verbatimTextOutput(NS(id, "GDA_output")),
                width = 12
              ),
              box(
                br(),
                DT::DTOutput(NS(id, "GDA_params")),
                DT::DTOutput(NS(id, "GDA_metrices")),
                plotOutput(NS(id, "GDA_plot")),
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
                numericInput(NS(id, "GDA_sens_bounds"), "+/- boundary in [%]", value = 15),
                actionButton(NS(id, "GDA_Start_Sensi"), "Start Sensitivity analysis"),
                actionButton(NS(id, "GDA_cancel_sense"), "Cancel"),
                actionButton(NS(id, "GDA_status_sense"), "Get Status"),
                downloadButton(NS(id, "GDA_sensi_download"), "Save result of sensitivity analysis"),
                verbatimTextOutput(NS(id, "GDA_output_sense")),
                width = 12
              ),
              box(
                br(),
                plotOutput(NS(id, "GDA_sensi")),
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



gdaServer <- function(id, df, com, com_sense, nclicks, nclicks_sense) {
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
    observeEvent(input$GDA_Start_Opti, {
      if (nclicks() != 0 | nclicks_sense() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      session$sendCustomMessage(type = "GDAclearField", list(message = NULL))
      nclicks(nclicks() + 1)
      result_val(data.frame(Status = "Running..."))
      com$running()
      session$sendCustomMessage(type = "GDAclearField", list(message = NULL, arg = 1))
      req(input$GDA_H0)
      req(input$GDA_G0)
      req(input$GDA_kHD)
      req(input$GDA_npop)
      req(input$GDA_ngen)
      req(input$GDA_threshold)
      req(input$GDA_kHD_lb)
      req(input$GDA_kHD_ub)
      req(input$GDA_IHD_lb)
      req(input$GDA_IHD_ub)
      req(input$GDA_ID_lb)
      req(input$GDA_ID_ub)
      req(input$GDA_I0_lb)
      req(input$GDA_I0_ub)
      lb <- c(input$GDA_kHD_lb, input$GDA_I0_lb, input$GDA_IHD_lb, input$GDA_ID_lb)
      lb <- convertToNum(lb)
      req(!("Error" %in% lb))
      ub <- c(input$GDA_kHD_ub, input$GDA_I0_ub, input$GDA_IHD_ub, input$GDA_ID_ub)
      ub <- convertToNum(ub)
      req(!("Error" %in% ub))
      additionalParameters <- c(input$GDA_H0, input$GDA_G0, input$GDA_kHD)
      additionalParameters <- convertToNum(additionalParameters)
      req(!("Error" %in% additionalParameters))
      npop <- input$GDA_npop
      ngen <- input$GDA_ngen
      topo <- input$GDA_topology
      et <- input$GDA_threshold
      seed <- input$Seed
      if (is.na(seed)) seed <- as.numeric(Sys.time())
      fl()
      result <- future(
        {
          opti(
            "gda", lb, ub, df, additionalParameters,
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
    observeEvent(input$GDA_cancel, {
      exportTestValues(
        cancel_clicked = TRUE
      )
      com$interrupt()
    })
    observeEvent(input$GDA_status, {
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
            type = "GDAupdateField",
            list(message = m)
          )
          iter(extract_iter(m))
        }
      } else {
        if (is.null(extract_iter(m))) {
          session$sendCustomMessage(
            type = "GDAupdateField",
            list(message = "Initialisation")
          )
        }
        exportTestValues(
          status2 = "Initialisation"
        )
        iter(extract_iter(m))
      }
    })
    output$GDA_params <- renderDT({
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[2]]))
      res <- result_val()[[2]]
      names(res)[1] <- c("K<sub>a</sub>(HG) [M]")
      names(res)[2] <- c("I(0)")
      names(res)[3] <- c("I(HD)")
      names(res)[4] <- c("I(D)")
      exportTestValues(
        df_params = res
      )
      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 3)
    })
    output$GDA_plot <- renderPlot({
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[3]]))
      result_val()[[3]]
    })
    output$GDA_metrices <- renderDT({
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
    output$GDA_download <- downloadHandler(
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
            "Model: GDA",
            startCol = 1,
            startRow = 1
          )

          curr_row <- 3
          curr_val <- result_val()[[1]]
          names(curr_val) <- c(
            "total Dye measured [M]", "Signal measured",
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
          names(curr_val)[2] <- c("Guest [M]")
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
          write.table("Model: GDA", file)
          curr_val <- result_val()[[1]]
          names(curr_val) <- c(
            "total Dye measured [M]", "Signal measured",
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
          names(curr_val)[2] <- c("Guest [M]")
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



    observeEvent(input$GDA_Start_Sensi, {
      if (nclicks_sense() != 0 | nclicks() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      nclicks_sense(nclicks_sense() + 1)
      result_val_sense(data.frame(Status = "Running..."))
      session$sendCustomMessage(type = "GDAclearFieldSense", list(message = NULL, arg = 1))
      com_sense$running()
      req(input$GDA_H0)
      req(input$GDA_G0)
      req(input$GDA_kHD)
      req(input$GDA_sens_bounds)
      req(length(result_val()) == 4)
      additionalParameters <- c(input$GDA_H0, input$GDA_G0, input$GDA_kHD)
      additionalParameters <- convertToNum(additionalParameters)
      req(!("Error" %in% additionalParameters))
      optim_params <- result_val()[[2]]
      sense_bounds <- input$GDA_sens_bounds
      fl()
      result_sense <- future(
        {
          sensitivity("gda", optim_params, df, additionalParameters,
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
    observeEvent(input$GDA_cancel_sense, {
      exportTestValues(
        cancel_sense_clicked = TRUE
      )
      com_sense$interrupt()
    })
    observeEvent(input$GDA_status_sense, {
      req(nclicks_sense() != 0)
      exportTestValues(
        status_sense = {
          com_sense$getStatus()
        }
      )
      session$sendCustomMessage(
        type = "GDAupdateFieldSense",
        list(message = com_sense$getStatus())
      )
    })
    output$GDA_sensi <- renderPlot({
      req(inherits(result_val_sense(), "ggplot"))
      exportTestValues(
        sense_plot = {
          result_val_sense()
        }
      )
      result_val_sense()
    })
    output$GDA_sensi_download <- downloadHandler(
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
