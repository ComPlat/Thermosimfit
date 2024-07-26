hgUI <- function(id) {
  tabItem(
    tabName = "HG",
    tags$script(
      "Shiny.addCustomMessageHandler('HGupdateField', function(message) {
              var result = message.message;
              $('#HG-HG_output').append(result + '\\n');
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('HGclearField', function(message) {
              $('#HG-HG_output').empty();
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('HGupdateFieldSense', function(message) {
              var result = message.message;
              $('#HG-HG_output_sense').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('HGclearFieldSense', function(message) {
              $('#HG-HG_output_sense').empty();
            });"
    ),
    fluidRow(
      box(
        textInput(NS(id, "HG_H0"), "Host conc. [M]", value = 0),
        box(
          title = "Particle swarm options",
          collapsible = TRUE, collapsed = TRUE,
          box(
            numericInput(NS(id, "HG_npop"), "Number of particles", value = 40),
            numericInput(NS(id, "HG_ngen"), "Number of generations", value = 200)
          ),
          box(
            selectInput(NS(id, "HG_topology"), "Topology of particle swarm",
              c(
                "star" = "star",
                "random arbitrary neighberhood" = "random"
              ),
              selectize = FALSE
            ),
            numericInput(NS(id, "HG_threshold"), "Threshold of the error", value = 0.00001)
          ),
          width = 12
        ),
        width = 6,
        title = "Parameter", solidHeader = TRUE,
        status = "warning", height = 450
      ),
      box(
        box(
          textInput(NS(id, "HG_kHD_lb"), HTML("K<sub>a</sub>(HD) value lower boundary [1/M]"), value = 0),
          textInput(NS(id, "HG_kHD_ub"), HTML("K<sub>a</sub>(HD) value upper boundary [1/M]"), value = 1e09)
        ),
        box(
          textInput(NS(id, "HG_I0_lb"), "I(0) value lower boundary", value = 0),
          textInput(NS(id, "HG_I0_ub"), "I(0) value upper boundary", value = 1)
        ),
        box(
          textInput(NS(id, "HG_IHD_lb"), "I(HD) value lower boundary [1/M]", value = 0),
          textInput(NS(id, "HG_IHD_ub"), "I(HD) value upper boundary [1/M]", value = 1e06)
        ),
        box(
          textInput(NS(id, "HG_ID_lb"), "I(D) value lower boundary [1/M]", value = 0),
          textInput(NS(id, "HG_ID_ub"), "I(D) value upper boundary [1/M]", value = 1e06)
        ),
        width = 6, title = "Boundaries", solidHeader = TRUE,
        status = "warning", height = 450
      )
    ),
    fluidRow(
      tabBox(
        tabPanel(
          "Optimization",
          fluidRow(
            box(
              box(
                actionButton(NS(id, "HG_Start_Opti"), "Start Optimization"),
                actionButton(NS(id, "HG_cancel"), "Cancel"),
                actionButton(NS(id, "HG_status"), "Get Status"),
                downloadButton(NS(id, "HG_download"), "Save result of optimization"),
                verbatimTextOutput(NS(id, "HG_output")),
                width = 12
              ),
              box(
                br(),
                DT::DTOutput(NS(id, "HG_params")),
                DT::DTOutput(NS(id, "HG_metrices")),
                plotOutput(NS(id, "HG_plot")),
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
                numericInput(NS(id, "HG_sens_bounds"), "+/- boundary in [%]", value = 15),
                actionButton(NS(id, "HG_Start_Sensi"), "Start Sensitivity analysis"),
                actionButton(NS(id, "HG_cancel_sense"), "Cancel"),
                actionButton(NS(id, "HG_status_sense"), "Get Status"),
                downloadButton(NS(id, "HG_sensi_download"), "Save result of sensitivity analysis"),
                verbatimTextOutput(NS(id, "HG_output_sense")),
                width = 12
              ),
              box(
                br(),
                plotOutput(NS(id, "HG_sensi")),
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



hgServer <- function(id, df, com, com_sense, nclicks, nclicks_sense) {
  moduleServer(id, function(input, output, session) {
    result_val <- reactiveVal()
    result_val_sense <- reactiveVal()
    iter <- reactiveVal()

    observeEvent(input$HG_Start_Opti, {
      if (nclicks() != 0 | nclicks_sense() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      session$sendCustomMessage(type = "HGclearField", list(message = NULL))

      nclicks(nclicks() + 1)
      result_val(data.frame(Status = "Running..."))
      com$running()
      session$sendCustomMessage(type = "HGclearField", list(message = NULL, arg = 1))
      req(input$HG_H0)
      req(input$HG_npop)
      req(input$HG_ngen)
      req(input$HG_threshold)
      req(input$HG_kHD_lb)
      req(input$HG_kHD_ub)
      req(input$HG_IHD_lb)
      req(input$HG_IHD_ub)
      req(input$HG_ID_lb)
      req(input$HG_ID_ub)
      req(input$HG_I0_lb)
      req(input$HG_I0_ub)
      lb <- c(input$HG_kHD_lb, input$HG_I0_lb, input$HG_IHD_lb, input$HG_ID_lb)
      lb <- convertToNum(lb)
      req(!("Error" %in% lb))
      ub <- c(input$HG_kHD_ub, input$HG_I0_ub, input$HG_IHD_ub, input$HG_ID_ub)
      ub <- convertToNum(ub)
      req(!("Error" %in% ub))
      additionalParameters <- c(input$HG_H0)
      additionalParameters <- convertToNum(additionalParameters)
      req(!("Error" %in% additionalParameters))
      npop <- input$HG_npop
      ngen <- input$HG_ngen
      topo <- input$HG_topology
      et <- input$HG_threshold
      flush(com$result)
      result <- future(
        {
          opti(
            "hg", lb, ub, df, additionalParameters,
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

      NULL
    })
    observeEvent(input$HG_cancel, {
      com$interrupt()
    })
    observeEvent(input$HG_status, {
      req(nclicks() != 0)
      m <- com$getData()
      i <- iter()

      if (!is.null(i)) {
        if (i != extract_iter(m)) {
          session$sendCustomMessage(
            type = "HGupdateField",
            list(message = m)
          )
          iter(extract_iter(m))
        }
      } else {
        if (is.null(extract_iter(m))) {
          session$sendCustomMessage(
            type = "HGupdateField",
            list(message = "Initialisation")
          )
        }
        iter(extract_iter(m))
      }
    })
    output$HG_params <- renderDT({
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[2]]))
      res <- result_val()[[2]]
      names(res)[1] <- c("K<sub>a</sub>(HD) [M]")
      names(res)[2] <- c("I(0)")
      names(res)[3] <- c("I(HD)")
      names(res)[4] <- c("I(D)")

      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 3)
    })
    output$HG_plot <- renderPlot({
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[3]]))
      result_val()[[3]]
    })
    output$HG_metrices <- renderDT({
      req(length(result_val()) == 4)
      req(!is.null(result_val()[[4]]))
      res <- as.data.frame(result_val()[[4]])
      names(res)[1] <- c("MeanSquareError")
      names(res)[2] <- c("RootMeanSquareError")
      names(res)[3] <- c("MeanAbsoluteError")
      names(res)[4] <- c("R<sup>2</sup>")
      names(res)[5] <- c("R<sup>2</sup> adjusted")
      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 6)
    })
    output$HG_download <- downloadHandler(
      filename = function() {
        "result.xlsx"
      },
      content = function(file) {
        req(length(result_val()) == 4)

        wb <- openxlsx::createWorkbook()
        addWorksheet(wb, "Results")
        curr_row <- 1
        curr_val <- result_val()[[1]]
        writeData(wb, "Results", curr_val, startRow = curr_row)
        curr_row <- curr_row + dim(curr_val)[1] + 5

        curr_val <- result_val()[[2]]
        names(curr_val)[1] <- c("Ka(HG) [M]")
        names(curr_val)[2] <- c("I(0)")
        names(curr_val)[3] <- c("I(HD) [1/M]")
        names(curr_val)[4] <- c("I(D) [1/M]")
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
        openxlsx::saveWorkbook(wb, file)
        unlink(tempfile_plot)
      }
    )



    observeEvent(input$HG_Start_Sensi, {
      if (nclicks_sense() != 0 | nclicks() != 0) {
        showNotification("Already running analysis")
        return(NULL)
      }
      nclicks_sense(nclicks_sense() + 1)
      result_val_sense(data.frame(Status = "Running..."))
      session$sendCustomMessage(type = "HGclearFieldSense", list(message = NULL, arg = 1))
      com_sense$running()
      req(input$HG_H0)
      req(input$HG_sens_bounds)
      req(length(result_val()) == 4)
      additionalParameters <- c(input$HG_H0)
      additionalParameters <- convertToNum(additionalParameters)
      req(!("Error" %in% additionalParameters))
      optim_params <- result_val()[[2]]
      sense_bounds <- input$HG_sens_bounds
      flush(com$result)
      result_sense <- future(
        {
          sensitivity("hg", optim_params, df, additionalParameters,
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
    observeEvent(input$HG_cancel_sense, {
      com_sense$interrupt()
    })
    observeEvent(input$HG_status_sense, {
      req(nclicks_sense() != 0)
      session$sendCustomMessage(
        type = "HGupdateFieldSense",
        list(message = com_sense$getStatus())
      )
    })
    output$HG_sensi <- renderPlot({
      req(inherits(result_val_sense(), "ggplot"))
      result_val_sense()
    })
    output$HG_sensi_download <- downloadHandler(
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
