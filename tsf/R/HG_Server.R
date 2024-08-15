hgServer <- function(id, df, com, com_sense, nclicks, nclicks_sense) {
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

    fl <- reactive({
      flush(com$result)
      return()
    })
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
      seed <- input$Seed
      if (is.na(seed)) seed <- as.numeric(Sys.time())
      fl()
      result <- future(
        {
          opti(
            "dba_host_const", lb, ub, df, additionalParameters,
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
    observeEvent(input$HG_cancel, {
      exportTestValues(
        cancel_clicked = TRUE
      )
      com$interrupt()
    })
    observeEvent(input$HG_status, {
      req(nclicks() != 0)
      m <- com$getData()
      if(length(nchar(m)) == 0) m <- "Initialisation"
      exportTestValues(
        status1 = {
          m
        }
      )
      session$sendCustomMessage(
        type = "HGupdateField",
        list(message = m)
      )
    })
    output$HG_params <- renderDT({
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
      exportTestValues(
        df_metrices = res
      )
      datatable(res,
        escape = FALSE,
        caption = "Error Metrics: Comparison of in silico signal and measured signal"
      ) |>
        formatSignif(columns = 1:ncol(res), digits = 6)
    })
    output$HG_download <- downloadHandler(
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
            "Model: DBA host const",
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
            Host = ai[[3]],
            npop = ai[[4]], ngen = ai[[5]], topology = ai[[6]],
            error_threshold = ai[[7]], seed = ai[[8]]
          )
          names(curr_val)[1] <- c("Host [M]")
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
          write.table("Model: DBA host const", file)
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
            Host = ai[[3]],
            npop = ai[[4]], ngen = ai[[5]], topology = ai[[6]],
            error_threshold = ai[[7]], seed = ai[[8]]
          )
          names(curr_val)[1] <- c("Host [M]")
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
      fl()
      result_sense <- future(
        {
          sensitivity("dba_host_const", optim_params, df, additionalParameters,
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
      exportTestValues(
        cancel_sense_clicked = TRUE
      )
      com_sense$interrupt()
    })
    observeEvent(input$HG_status_sense, {
      req(nclicks_sense() != 0)
      exportTestValues(
        status_sense = {
          com_sense$getStatus()
        }
      )
      session$sendCustomMessage(
        type = "HGupdateFieldSense",
        list(message = com_sense$getStatus())
      )
    })
    output$HG_sensi <- renderPlot({
      req(inherits(result_val_sense(), "ggplot"))
      exportTestValues(
        sense_plot = {
          result_val_sense()
        }
      )
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
