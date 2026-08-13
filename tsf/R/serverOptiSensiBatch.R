server_opti_sensi_batch <- function(id, df_reactive, df_list_reactive, nclicks) {
  df <- reactive({df_reactive$df})
  nsigs <- reactive({df_reactive$nsigs})
  df_list <- reactive({df_list_reactive$data_frames})
  moduleServer(id, function(input, output, session) {

    # Render parameter boundaries
    # ===============================================================================
    parameter_state <- reactiveValues(idx = 1L, Is = list(), confirmed = list(), Is_old = list())

    I_inputs <- reactive({
      list(
        lb_I0  = input$I0_lb, ub_I0  = input$I0_ub,
        lb_IHD = input$IHD_lb, ub_IHD = input$IHD_ub,
        lb_ID  = input$ID_lb, ub_ID  = input$ID_ub
      )
    })

    output[["Confirmed_I"]] <- renderUI({
      class <- "label label-default"
      status <- "Not confirmed"
      if (length(parameter_state$Is) >= parameter_state$idx) {
        if (parameter_state$confirmed[[parameter_state$idx]]) {
          old <- parameter_state$Is_old[[parameter_state$idx]]
          current <- I_inputs()
          if (all(unlist(Map(`==`, old, current)))) {
            class <- "label label-success"
            status <- "confirmed"
          } else {
            class <- "label label-info"
            status <- "edited"
          }
        }
      }
      tags$span(
        status,
        class = class,
        style = "font-size:12px; padding:2px 6px; border-radius:999px; margin-left:.5rem;"
      )
    })

    output[["BOUNDS_I"]] <- renderUI({
      lb_I0 <- 0; ub_I0 <- 10^8;
      lb_IHD <- 0; ub_IHD <- 10^8;
      lb_ID <- 0; ub_ID <- 10^8;

      if (length(parameter_state$Is) >= parameter_state$idx) {
        Is <- parameter_state$Is[[parameter_state$idx]]
        lb_I0 <- Is[["lb_I0"]]
        ub_I0 <- Is[["ub_I0"]]
        lb_IHD <- Is[["lb_IHD"]]
        ub_IHD <- Is[["ub_IHD"]]
        lb_ID <- Is[["lb_ID"]]
        ub_ID <- Is[["ub_ID"]]
      }

      div(
        box(
          title = div(class = "d-flex justify-content-between align-items-center",
            h4(sprintf("I parameters — signal %s", parameter_state$idx), class = "m-0"),
            uiOutput(NS(id, "Confirmed_I"), inline = TRUE)
          ),
          textInput(NS(id, "I0_lb"), "I(0) value lower boundary", value = lb_I0) |> tagAppendAttributes(title = "Lower bound for I(0). Use 0 if unknown."),
          textInput(NS(id, "I0_ub"), "I(0) value upper boundary", value = ub_I0) |> tagAppendAttributes(title = "Upper bound for I(0). Use 10^8 if unknown."),
          textInput(NS(id, "IHD_lb"),
            label = tagList(
              "I(HD) value lower boundary [1/M]",
              actionButton(NS(id, "AdviceUBIHD"), "Help",
                icon = icon("question-circle"),
                style = "background-color:transparent; border:none;"
              )
            ), value = lb_IHD
          ) |> tagAppendAttributes(title = "Lower bound for I(HD). Use 0 if unknown."),
          textInput(NS(id, "IHD_ub"), "I(HD) value upper boundary [1/M]", value = ub_IHD) |> tagAppendAttributes(title = "Upper bound for I(HD). Use 10^8 if unknown."),
          textInput(NS(id, "ID_lb"), "I(D) value lower boundary [1/M]", value = lb_ID) |> tagAppendAttributes(title = "Lower bound for I(D). Use 0 if unknown."),
          textInput(NS(id, "ID_ub"), "I(D) value upper boundary [1/M]", value = ub_ID) |> tagAppendAttributes(title = "Upper bound for I(D). Use 10^8 if unknown."),
          actionButton(
            inputId = NS(id, "Confirm"),
            label = "Confirm boundaries",
            class = "add-button df-button"
          ),
          actionButton(
            inputId = NS(id, "PreviousIParameter"),
            label = "Previous boundaries",
            class = "add-button df-button"
          ),
          actionButton(
            inputId = NS(id, "NextIParameter"),
            label = "Next boundaries",
            class = "add-button df-button"
          )
        )
      )
    })

    observeEvent(input$Confirm, ignoreInit = TRUE, {
      parameter_state$Is[[parameter_state$idx]] <- I_inputs()
      parameter_state$Is_old[[parameter_state$idx]] <- parameter_state$Is[[parameter_state$idx]]
      parameter_state$confirmed[[parameter_state$idx]] <- TRUE
    })

    observeEvent(input$NextIParameter, ignoreInit = TRUE, {
      if (parameter_state$idx < nsigs()) {
        parameter_state$idx <-parameter_state$idx + 1L
      } else {
        showNotification("Already at the last signal",
          type = "error", duration = 20
        )
      }
    })

    observeEvent(input$PreviousIParameter, ignoreInit = TRUE, {
      if (parameter_state$idx > 1L) {
        parameter_state$idx <-parameter_state$idx - 1L
      } else {
        showNotification("Already at the first signal",
          type = "error", duration = 20
        )
      }
    })

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

    # Batch-PSO has its own independent I0/IHD/ID boundary-setting flow,
    # a full mirror of the one above rather than sharing parameter_state -
    # otherwise there's no visibility on the Batch-PSO tab that these
    # boundaries even exist or need to be set.
    parameter_state_batch <- reactiveValues(idx = 1L, Is = list(), confirmed = list(), Is_old = list())

    I_inputs_batch <- reactive({
      list(
        lb_I0  = input$I0_lb_batch, ub_I0  = input$I0_ub_batch,
        lb_IHD = input$IHD_lb_batch, ub_IHD = input$IHD_ub_batch,
        lb_ID  = input$ID_lb_batch, ub_ID  = input$ID_ub_batch
      )
    })

    output[["Confirmed_I_batch"]] <- renderUI({
      class <- "label label-default"
      status <- "Not confirmed"
      if (length(parameter_state_batch$Is) >= parameter_state_batch$idx) {
        if (parameter_state_batch$confirmed[[parameter_state_batch$idx]]) {
          old <- parameter_state_batch$Is_old[[parameter_state_batch$idx]]
          current <- I_inputs_batch()
          if (all(unlist(Map(`==`, old, current)))) {
            class <- "label label-success"
            status <- "confirmed"
          } else {
            class <- "label label-info"
            status <- "edited"
          }
        }
      }
      tags$span(
        status,
        class = class,
        style = "font-size:12px; padding:2px 6px; border-radius:999px; margin-left:.5rem;"
      )
    })

    output[["BOUNDS_I_batch"]] <- renderUI({
      lb_I0 <- 0; ub_I0 <- 10^8;
      lb_IHD <- 0; ub_IHD <- 10^8;
      lb_ID <- 0; ub_ID <- 10^8;

      if (length(parameter_state_batch$Is) >= parameter_state_batch$idx) {
        Is <- parameter_state_batch$Is[[parameter_state_batch$idx]]
        lb_I0 <- Is[["lb_I0"]]
        ub_I0 <- Is[["ub_I0"]]
        lb_IHD <- Is[["lb_IHD"]]
        ub_IHD <- Is[["ub_IHD"]]
        lb_ID <- Is[["lb_ID"]]
        ub_ID <- Is[["ub_ID"]]
      }

      div(
        box(
          title = div(class = "d-flex justify-content-between align-items-center",
            h4(sprintf("I parameters — signal %s", parameter_state_batch$idx), class = "m-0"),
            uiOutput(NS(id, "Confirmed_I_batch"), inline = TRUE)
          ),
          textInput(NS(id, "I0_lb_batch"), "I(0) value lower boundary", value = lb_I0) |> tagAppendAttributes(title = "Lower bound for I(0). Use 0 if unknown."),
          textInput(NS(id, "I0_ub_batch"), "I(0) value upper boundary", value = ub_I0) |> tagAppendAttributes(title = "Upper bound for I(0). Use 10^8 if unknown."),
          textInput(NS(id, "IHD_lb_batch"),
            label = tagList(
              "I(HD) value lower boundary [1/M]",
              actionButton(NS(id, "AdviceUBIHD_batch"), "Help",
                icon = icon("question-circle"),
                style = "background-color:transparent; border:none;"
              )
            ), value = lb_IHD
          ) |> tagAppendAttributes(title = "Lower bound for I(HD). Use 0 if unknown."),
          textInput(NS(id, "IHD_ub_batch"), "I(HD) value upper boundary [1/M]", value = ub_IHD) |> tagAppendAttributes(title = "Upper bound for I(HD). Use 10^8 if unknown."),
          textInput(NS(id, "ID_lb_batch"), "I(D) value lower boundary [1/M]", value = lb_ID) |> tagAppendAttributes(title = "Lower bound for I(D). Use 0 if unknown."),
          textInput(NS(id, "ID_ub_batch"), "I(D) value upper boundary [1/M]", value = ub_ID) |> tagAppendAttributes(title = "Upper bound for I(D). Use 10^8 if unknown."),
          actionButton(
            inputId = NS(id, "Confirm_batch"),
            label = "Confirm boundaries",
            class = "add-button df-button"
          ),
          actionButton(
            inputId = NS(id, "PreviousIParameter_batch"),
            label = "Previous boundaries",
            class = "add-button df-button"
          ),
          actionButton(
            inputId = NS(id, "NextIParameter_batch"),
            label = "Next boundaries",
            class = "add-button df-button"
          )
        )
      )
    })

    observeEvent(input$Confirm_batch, ignoreInit = TRUE, {
      parameter_state_batch$Is[[parameter_state_batch$idx]] <- I_inputs_batch()
      parameter_state_batch$Is_old[[parameter_state_batch$idx]] <- parameter_state_batch$Is[[parameter_state_batch$idx]]
      parameter_state_batch$confirmed[[parameter_state_batch$idx]] <- TRUE
    })

    observeEvent(input$NextIParameter_batch, ignoreInit = TRUE, {
      if (parameter_state_batch$idx < nsigs()) {
        parameter_state_batch$idx <- parameter_state_batch$idx + 1L
      } else {
        showNotification("Already at the last signal",
          type = "error", duration = 20
        )
      }
    })

    observeEvent(input$PreviousIParameter_batch, ignoreInit = TRUE, {
      if (parameter_state_batch$idx > 1L) {
        parameter_state_batch$idx <- parameter_state_batch$idx - 1L
      } else {
        showNotification("Already at the first signal",
          type = "error", duration = 20
        )
      }
    })

    observeEvent(input$helpButton_batch, {
      showModal(modalDialog(
        title = "Help",
        HTML("Conduct two optimizations. First with wide boundaries. \n
          Afterwards chose narrow boundaries based on the result of the first optimization."),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    observeEvent(input$AdviceUBIHD_batch, {
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
    opti_result_signal_idx <- reactiveVal(1L)

    # NOTE: Start of model specific code
    # ===============================================================================
    check_inputs <- function() {
      rwn(!is.na(input$npop),
        "Please enter a value for number of particles")
      rwn(!is.na(input$ngen),
        "Please enter a value for the number of generations")
      rwn(is_integer(input$npop),
        "Please enter an integer value for number of particles")
      rwn(is_integer(input$ngen),
        "Please enter an integer value for number of generations")
      rwn(!is.na(input$threshold),
        "Please enter a value for the error threshold")

      Is <- parameter_state$Is
      rwn(length(Is) == nsigs(), "The I parameter boundaries are not defined for all signals")
      lapply(Is, function(I) {
        rwn(!is.null(I), "Not all I parameter boundaries are set")
        rwn(I$lb_I0 != "", sprintf("Please set the lower bound for I0 for signal Nr.%s", parent.frame()$i[]))
        rwn(I$ub_I0 != "", sprintf("Please set the upper bound for I0 for signal Nr.%s", parent.frame()$i[]))
        rwn(I$lb_IHD != "", sprintf("Please set the lower bound for IHD for signal Nr.%s", parent.frame()$i[]))
        rwn(I$ub_IHD != "", sprintf("Please set the upper bound for IHD for signal Nr.%s", parent.frame()$i[]))
        rwn(I$lb_ID != "", sprintf("Please set the lower bound for ID for signal Nr.%s", parent.frame()$i[]))
        rwn(I$ub_ID != "", sprintf("Please set the upper bound for ID for signal Nr.%s", parent.frame()$i[]))
      })

      if (id == "HG") {
        rwn(input$H0 != "", "Please enter a value for the Host")
        rwn(input$kHD_lb != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub != "",
          "Please enter a value for the upper boundary of KaHD")
      } else if (id == "DBA") {
        rwn(input$D0 != "", "Please enter a value for the Dye")
        rwn(input$kHD_lb != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub != "",
          "Please enter a value for the upper boundary of KaHD")
      } else if (id == "IDA") {
        rwn(input$H0 != "", "Please enter a value for the Host")
        rwn(input$D0 != "", "Please enter a value for the Dye")
        rwn(input$kHD != "", "Please enter a value for KaHD")
        rwn(input$kHG_lb != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub != "",
          "Please enter a value for the upper boundary of KaHG")
      } else if(id == "GDA") {
        rwn(input$H0 != "", "Please enter a value for the Host")
        rwn(input$G0 != "", "Please enter a value for the Guest")
        rwn(input$kHD != "", "Please enter a value for KaHD")
        rwn(input$kHG_lb != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub != "",
          "Please enter a value for the upper boundary of KaHG")
      }
    }

    check_inputs_sensi <- function() { 
      if (id == "HG") {
        rwn(input$H0 != "",
          "Please enter a value for the Host")
        rwn(is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(opti_result_created(),
          "Please run first an optimization") 
      } else if (id == "DBA") {
        rwn(input$D0 != "",
          "Please enter a value for the Dye")
        rwn(is_integer(input$sens_bounds),
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
        rwn(is_integer(input$sens_bounds),
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
        rwn(is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(opti_result_created(),
          "Please run first an optimization") 
      }
    }

    create_lb <- function() {
      is <- parameter_state$Is
      par_names <- c("I(0)", "I(HD) [1/M]", "I(D) [1/M]")
      if (length(is) == 1) {
        i_lbs <- is[[1]][c(1, 3, 5)]
        names(i_lbs) <- par_names
      } else {
        i_lbs <- lapply(1:3, function(i) {
          i <- unlist(is[[i]])
          lbs <- i[c(1, 3, 5)]
          names(lbs) <- vapply(1:3, function(n) {
            paste0("Sig. Nr.", n, " ", par_names[n])
          }, character(1))
          lbs
        }) |> unlist()
      }
      lb <- ""
      if (id == "HG" || id == "DBA") {
        lb <- convert_all_to_num(
          "lower boundaries",
          input$kHD_lb, i_lbs
        )
      } else if (id == "IDA" || id == "GDA") {
        lb <- convert_all_to_num(
          "lower boundaries",
          input$kHG_lb, i_lbs
        )
      }
      return(lb)
    }

    create_ub <- function() {
      is <- parameter_state$Is
      par_names <- c("I(0)", "I(HD) [1/M]", "I(D) [1/M]")
      if (length(is) == 1) {
        i_ubs <- is[[1]][c(2, 4, 6)]
        names(i_ubs) <- par_names
      } else {
        i_ubs <- lapply(1:3, function(i) {
          i <- unlist(is[[i]])
          ubs <- i[c(2, 4, 6)]
          names(ubs) <- vapply(1:3, function(n) {
            paste0("Sig. Nr.", n, " ", par_names[n])
          }, character(1))
          ubs
        }) |> unlist()
      }
      ub <- ""
      if (id == "HG" || id == "DBA") {
        ub <- convert_all_to_num(
          "upper boundaries",
          input$kHD_ub, i_ubs
        )
      } else if (id == "IDA" || id == "GDA") {
        ub <- convert_all_to_num(
          "upper boundaries",
          input$kHG_ub, i_ubs
        )
      }
      return(ub)
    }

    # Batch-PSO has its own fully independent H0/D0/kHD/bounds/npop/ngen/
    # topology/threshold/error_calc_fct/Seed (the "_batch"-suffixed inputs),
    # separate from the single-run Optimization tab's. The I0/IHD/ID
    # boundaries are the one exception, shared with the Optimization tab
    # since they're per-signal state rather than per-optimizer-run.
    create_lb_batch <- function() {
      is <- parameter_state_batch$Is
      par_names <- c("I(0)", "I(HD) [1/M]", "I(D) [1/M]")
      if (length(is) == 1) {
        i_lbs <- is[[1]][c(1, 3, 5)]
        names(i_lbs) <- par_names
      } else {
        i_lbs <- lapply(1:3, function(i) {
          i <- unlist(is[[i]])
          lbs <- i[c(1, 3, 5)]
          names(lbs) <- vapply(1:3, function(n) {
            paste0("Sig. Nr.", n, " ", par_names[n])
          }, character(1))
          lbs
        }) |> unlist()
      }
      lb <- ""
      if (id == "HG" || id == "DBA") {
        lb <- convert_all_to_num(
          "lower boundaries",
          input$kHD_lb_batch, i_lbs
        )
      } else if (id == "IDA" || id == "GDA") {
        lb <- convert_all_to_num(
          "lower boundaries",
          input$kHG_lb_batch, i_lbs
        )
      }
      return(lb)
    }

    create_ub_batch <- function() {
      is <- parameter_state_batch$Is
      par_names <- c("I(0)", "I(HD) [1/M]", "I(D) [1/M]")
      if (length(is) == 1) {
        i_ubs <- is[[1]][c(2, 4, 6)]
        names(i_ubs) <- par_names
      } else {
        i_ubs <- lapply(1:3, function(i) {
          i <- unlist(is[[i]])
          ubs <- i[c(2, 4, 6)]
          names(ubs) <- vapply(1:3, function(n) {
            paste0("Sig. Nr.", n, " ", par_names[n])
          }, character(1))
          ubs
        }) |> unlist()
      }
      ub <- ""
      if (id == "HG" || id == "DBA") {
        ub <- convert_all_to_num(
          "upper boundaries",
          input$kHD_ub_batch, i_ubs
        )
      } else if (id == "IDA" || id == "GDA") {
        ub <- convert_all_to_num(
          "upper boundaries",
          input$kHG_ub_batch, i_ubs
        )
      }
      return(ub)
    }

    create_additional_parameters_batch <- function() {
      if (id == "HG") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_batch
        )
        return(additionalParameters)
      } else if (id == "DBA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$D0_batch
        )
        return(additionalParameters)
      } else if (id == "IDA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_batch, input$D0_batch, input$kHD_batch
        )
        return(additionalParameters)
      } else if (id == "GDA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_batch, input$G0_batch, input$kHD_batch
        )
        return(additionalParameters)
      }
    }

    create_npop_batch <- function() {
      convert_num_to_int(input$npop_batch)
    }

    create_ngen_batch <- function() {
      convert_num_to_int(input$ngen_batch)
    }

    create_topology_batch <- function() {
      input$topology_batch
    }

    create_error_threshold_batch <- function() {
      input$threshold_batch
    }

    # VAPRO only searches the nonlinear binding constant (Ka/Kg); I0/IHD/ID
    # are profiled out via NNLS, so no I-parameter boundaries are needed.
    # It has its own fully independent H0/D0/kHD/bounds inputs (the
    # "_vapro"-suffixed ones), separate from the PSO tab's.
    check_inputs_vapro <- function() {
      rwn(!is.na(input$nGrid),
        "Please enter a value for the number of VAPRO grid points")
      rwn(is_integer(input$nGrid),
        "Please enter an integer value for the number of VAPRO grid points")

      if (id == "HG") {
        rwn(input$H0_vapro != "", "Please enter a value for the Host")
        rwn(input$kHD_lb_vapro != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub_vapro != "",
          "Please enter a value for the upper boundary of KaHD")
      } else if (id == "DBA") {
        rwn(input$D0_vapro != "", "Please enter a value for the Dye")
        rwn(input$kHD_lb_vapro != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub_vapro != "",
          "Please enter a value for the upper boundary of KaHD")
      } else if (id == "IDA") {
        rwn(input$H0_vapro != "", "Please enter a value for the Host")
        rwn(input$D0_vapro != "", "Please enter a value for the Dye")
        rwn(input$kHD_vapro != "", "Please enter a value for KaHD")
        rwn(input$kHG_lb_vapro != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub_vapro != "",
          "Please enter a value for the upper boundary of KaHG")
      } else if (id == "GDA") {
        rwn(input$H0_vapro != "", "Please enter a value for the Host")
        rwn(input$G0_vapro != "", "Please enter a value for the Guest")
        rwn(input$kHD_vapro != "", "Please enter a value for KaHD")
        rwn(input$kHG_lb_vapro != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub_vapro != "",
          "Please enter a value for the upper boundary of KaHG")
      }
    }

    create_lb_vapro <- function() {
      if (id == "HG" || id == "DBA") {
        return(convert_all_to_num("lower boundary", input$kHD_lb_vapro))
      } else {
        return(convert_all_to_num("lower boundary", input$kHG_lb_vapro))
      }
    }

    create_ub_vapro <- function() {
      if (id == "HG" || id == "DBA") {
        return(convert_all_to_num("upper boundary", input$kHD_ub_vapro))
      } else {
        return(convert_all_to_num("upper boundary", input$kHG_ub_vapro))
      }
    }

    create_nGrid <- function() {
      convert_num_to_int(input$nGrid)
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

    create_additional_parameters_vapro <- function() {
      if (id == "HG") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_vapro
        )
        return(additionalParameters)
      } else if (id == "DBA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$D0_vapro
        )
        return(additionalParameters)
      } else if (id == "IDA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_vapro, input$D0_vapro, input$kHD_vapro
        )
        return(additionalParameters)
      } else if (id == "GDA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_vapro, input$G0_vapro, input$kHD_vapro
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

    get_update_field_vapro <- function() {
      if (id == "HG") {
        return("HGupdateFieldVapro")
      } else if (id == "DBA") {
        return("DBAupdateFieldVapro")
      } else if (id == "IDA") {
        return("IDAupdateFieldVapro")
      } else if (id == "GDA") {
        return("GDAupdateFieldVapro")
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
      ecf <- input$error_calc_fct
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
      result <- call_opti_in_bg(get_Model(), lb, ub, df(),
        additionalParameters, seed, npop, ngen, topo, et, ecf
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
      m <- Reduce(function(a, b) {
        paste0(a, "\n", b)
      }, m)
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

    output$host_dye_plot <- renderPlot({
      correct_results()
      opti_result()[[4]]
    })

    output$metrices <- renderDT({
      correct_results()
      res <- as.data.frame(opti_result()[[5]])
      names(res)[3] <- c("R<sup>2</sup>")
      names(res)[4] <- c("R<sup>2</sup> adjusted")
      exportTestValues(
        df_metrices = res
      )
      datatable(res,
        escape = FALSE,
        caption = "Error Metrics: Comparison of in silico signal and measured signal"
      )
    })

    output$signal_plot <- renderPlot({
      correct_results()
      opti_result()[[3]][[opti_result_signal_idx()]]
    })
    observeEvent(input$next_signal_plot, ignoreInit = TRUE, {
      correct_results()
      orsi <- opti_result_signal_idx()
      if (orsi < nsigs()) {
        opti_result_signal_idx(orsi + 1L)
      } else {
        showNotification("Already at the last signal",
          type = "error", duration = 20
        )
      }
    })

    observeEvent(input$previous_signal_plot, ignoreInit = TRUE, {
      correct_results()
      orsi <- opti_result_signal_idx()
      if (orsi > 1L) {
        opti_result_signal_idx(orsi - 1L)
      } else {
        showNotification("Already at the first signal",
          type = "error", duration = 20
        )
      }
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

    # VAPRO Optimization
    # ===============================================================================
    # VAPRO is a deterministic grid search (no stochastic generations to show
    # progress across), so unlike PSO there is no cancel button and no live
    # status feed - just launch, poll until done, render.
    vapro_opti_result_created <- reactiveVal(FALSE)
    vapro_opti_result <- reactiveVal()
    vapro_process <- reactiveVal()
    vapro_setup_done <- reactiveVal(FALSE)
    vapro_opti_result_signal_idx <- reactiveVal(1L)

    vapro_message <- function(message) {
      session$sendCustomMessage(
        type = get_update_field_vapro(),
        list(message = message)
      )
      return(NULL)
    }

    observeEvent(input$Start_Vapro_Opti, {
      if (nclicks() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      check_inputs_vapro()
      request_cores(1, session$token)
      lb <- create_lb_vapro()
      ub <- create_ub_vapro()
      additionalParameters <- create_additional_parameters_vapro()
      nGrid <- create_nGrid()
      ecf <- input$error_calc_fct_vapro
      # clear everything
      vapro_setup_done(FALSE)
      vapro_opti_result_created(FALSE)
      vapro_process(NULL)
      invalid_time(1100)
      nclicks(nclicks() + 1)
      vapro_message("Running VAPRO optimization...")

      # start process
      result <- call_opti_vapro_in_bg(get_Model(), lb, ub, df(), additionalParameters, nGrid, ecf)
      vapro_process(result)
      vapro_setup_done(TRUE)
      NULL
    })

    vapro_process_done <- function() {
      req(vapro_setup_done())
      req(length(vapro_process()) > 0)
      if (vapro_process()$is_alive()) {
        req(vapro_process()$get_status() != "running")
        req(vapro_process()$get_status() != "sleeping")
      }
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }

    correct_results_vapro <- function() {
      req(vapro_opti_result_created())
      req(!is.null(vapro_opti_result()))
    }

    get_vapro_opti_data <- reactive({
      if (class(vapro_process())[[1]] == "r_process") {
        req(!vapro_process()$is_alive())
      }
      print_error(vapro_process()$read_error())
      try(vapro_opti_result(vapro_process()$get_result()))
      try({
        if (is.null(vapro_opti_result()) || inherits(vapro_opti_result(), "ErrorClass")) {
          if (inherits(vapro_opti_result(), "ErrorClass")) {
            print_error(vapro_opti_result()$message)
          }
          vapro_opti_result(NULL)
          vapro_message("")
          send_and_read_info(paste0("release: ", session$token))
          vapro_process()$wait()
          vapro_process()$kill()
          vapro_process(NULL)
          return(NULL)
        }
      })
      vapro_message("")
      vapro_process()$kill()
      send_and_read_info(paste0("release: ", session$token))
      vapro_process(NULL)
    })

    # observe results
    observe({
      invalidateLater(invalid_time())
      if (vapro_process_done() && !vapro_opti_result_created()) {
        get_vapro_opti_data()
        vapro_opti_result_created(TRUE)
      }
    })

    output$params_vapro <- renderDT({
      correct_results_vapro()
      res <- vapro_opti_result()[[2]]
      names(res)[1] <- get_K_param()
      datatable(res, escape = FALSE) |>
        formatSignif(columns = 1:ncol(res), digits = 3)
    })

    output$host_dye_plot_vapro <- renderPlot({
      correct_results_vapro()
      vapro_opti_result()[[4]]
    })

    output$metrices_vapro <- renderDT({
      correct_results_vapro()
      res <- as.data.frame(vapro_opti_result()[[5]])
      names(res)[3] <- c("R<sup>2</sup>")
      names(res)[4] <- c("R<sup>2</sup> adjusted")
      datatable(res,
        escape = FALSE,
        caption = "Error Metrics: Comparison of in silico signal and measured signal"
      )
    })

    output$signal_plot_vapro <- renderPlot({
      correct_results_vapro()
      vapro_opti_result()[[3]][[vapro_opti_result_signal_idx()]]
    })
    observeEvent(input$next_signal_plot_vapro, ignoreInit = TRUE, {
      correct_results_vapro()
      orsi <- vapro_opti_result_signal_idx()
      if (orsi < nsigs()) {
        vapro_opti_result_signal_idx(orsi + 1L)
      } else {
        showNotification("Already at the last signal",
          type = "error", duration = 20
        )
      }
    })

    observeEvent(input$previous_signal_plot_vapro, ignoreInit = TRUE, {
      correct_results_vapro()
      orsi <- vapro_opti_result_signal_idx()
      if (orsi > 1L) {
        vapro_opti_result_signal_idx(orsi - 1L)
      } else {
        showNotification("Already at the first signal",
          type = "error", duration = 20
        )
      }
    })

    output$download_vapro <- downloadHandler(
      filename = function() {
        paste("result_vapro", switch(input$file_type_vapro,
          xlsx = ".xlsx",
          csv = ".csv"
        ), sep = "")
      },
      content = function(file) {
        correct_results_vapro()
        result_val <- vapro_opti_result()
        if (input$file_type_vapro == "xlsx") {
          download_file_vapro(get_Model_capital(), file, result_val)
        } else {
          download_csv_vapro(get_Model_capital(), file, result_val)
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
      ecf <- input$error_calc_fct
      # clear everything
      sensi_setup_done(FALSE)
      invalid_time(1100)
      sensi_process(NULL)
      sensi_result_created(FALSE)
      sensi_message("Initializing...")
      # start process
      result <- call_sensi_in_bg(get_Model(), optim_params, df(),
        additionalParameters, sense_bounds, error_calc_fct = input$error_calc_fct)
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

    output$sensi_table <- renderDT({
      req(sensi_result_created())
      exportTestValues(
        sense_plot = {
          sensi_result()
        }
      )
      sensi_result()
    })

    output$sensi_download <- downloadHandler(
      filename = function() "result.csv",
      content = function(file) {
        req(sensi_result_created())
        write.table(sensi_result(), file, sep = ",", row.names = FALSE, col.names = TRUE)
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
    batch_state <- reactiveValues(current_dataset = 1L, current_signal = 1L)

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
        is_integer(input$NumRepDataset),
        "Please provide an integer entry for the replicates/dataset"
      )
      rwn(
        length(df_list()) > 0,
        "The dataset list seems to be empty. Please upload a file"
      )
      rwn( # TODO: update also other server code
        is_integer(input$NumCores),
        "Please provide an integer entry for number of cores"
      )

      rwn(!is.na(input$npop_batch),
        "Please enter a value for number of particles")
      rwn(!is.na(input$ngen_batch),
        "Please enter a value for the number of generations")
      rwn(is_integer(input$npop_batch),
        "Please enter an integer value for number of particles")
      rwn(is_integer(input$ngen_batch),
        "Please enter an integer value for number of generations")
      rwn(!is.na(input$threshold_batch),
        "Please enter a value for the error threshold")

      Is <- parameter_state_batch$Is
      rwn(length(Is) == nsigs(), "The I parameter boundaries are not defined for all signals")
      lapply(Is, function(I) {
        rwn(!is.null(I), "Not all I parameter boundaries are set")
        rwn(I$lb_I0 != "", sprintf("Please set the lower bound for I0 for signal Nr.%s", parent.frame()$i[]))
        rwn(I$ub_I0 != "", sprintf("Please set the upper bound for I0 for signal Nr.%s", parent.frame()$i[]))
        rwn(I$lb_IHD != "", sprintf("Please set the lower bound for IHD for signal Nr.%s", parent.frame()$i[]))
        rwn(I$ub_IHD != "", sprintf("Please set the upper bound for IHD for signal Nr.%s", parent.frame()$i[]))
        rwn(I$lb_ID != "", sprintf("Please set the lower bound for ID for signal Nr.%s", parent.frame()$i[]))
        rwn(I$ub_ID != "", sprintf("Please set the upper bound for ID for signal Nr.%s", parent.frame()$i[]))
      })

      if (id == "HG") {
        rwn(input$H0_batch != "", "Please enter a value for the Host")
        rwn(input$kHD_lb_batch != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub_batch != "",
          "Please enter a value for the upper boundary of KaHD")
      } else if (id == "DBA") {
        rwn(input$D0_batch != "", "Please enter a value for the Dye")
        rwn(input$kHD_lb_batch != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub_batch != "",
          "Please enter a value for the upper boundary of KaHD")
      } else if (id == "IDA") {
        rwn(input$H0_batch != "", "Please enter a value for the Host")
        rwn(input$D0_batch != "", "Please enter a value for the Dye")
        rwn(input$kHD_batch != "", "Please enter a value for KaHD")
        rwn(input$kHG_lb_batch != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub_batch != "",
          "Please enter a value for the upper boundary of KaHG")
      } else if (id == "GDA") {
        rwn(input$H0_batch != "", "Please enter a value for the Host")
        rwn(input$G0_batch != "", "Please enter a value for the Guest")
        rwn(input$kHD_batch != "", "Please enter a value for KaHD")
        rwn(input$kHG_lb_batch != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub_batch != "",
          "Please enter a value for the upper boundary of KaHG")
      }
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
      check_inputs_batch()
      lb <- create_lb_batch()
      ub <- create_ub_batch()
      additionalParameters <- create_additional_parameters_batch()
      npop <- create_npop_batch()
      ngen <- create_ngen_batch()
      topo <- create_topology_batch()
      ecf <- input$error_calc_fct_batch
      et <- create_error_threshold_batch()
      num_cores <- get_num_core()
      # check seed case
      seed <- input$Seed_batch
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
        npop, ngen, topo, ecf, et,
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
        state_plots <- plotStatesBatch(values, get_Model())
        i_param_plots <- plotIParamsBatch(values, num_rep_batch())
        ka_plots <- plotKaBatch(values, num_rep_batch())
        metrices_plots <- plotMetricesBatch(values, num_rep_batch())
        hd_d_plots <- plotDAndHDBatch(values, num_rep_batch())

        result_batch(structure(list(
          values, state_plots = state_plots,
          i_param_plots = i_param_plots,
          ka_plots = ka_plots,
          metrices_plots = metrices_plots,
          hd_d_plots = hd_d_plots
        ), class = "BatchResult"))
        task_queue(NULL)
      }
    })

    output$Ka_main_plot <- renderPlot({
      req(inherits(result_batch(), "BatchResult"))
      result_batch()$ka_plots[[1]]
    })
    output$Ka_dataset_plot <- renderPlot({
      req(inherits(result_batch(), "BatchResult"))
      result_batch()$ka_plots[[2]][[batch_state$current_dataset]]
    })
    output$hd_d_dataset_plot <- renderPlot({
      req(inherits(result_batch(), "BatchResult"))
      result_batch()$hd_d_plots[[batch_state$current_dataset]]
    })
    output$I_dataset_signal_plot <- renderPlot({
      req(inherits(result_batch(), "BatchResult"))
      result_batch()$i_param_plots[[batch_state$current_dataset]][[batch_state$current_signal]]
    })
    output$Signal_dataset_signal_plot <- renderPlot({
      req(inherits(result_batch(), "BatchResult"))
      result_batch()$state_plots[[batch_state$current_dataset]][[batch_state$current_signal]]
    })

    observeEvent(input$previous_dataset, ignoreInit = TRUE, {
      if (batch_state$current_dataset > 1L) {
        batch_state$current_dataset <- batch_state$current_dataset - 1L
        output$title_batch <- renderText(paste0("Dataset Nr.", batch_state$current_dataset))
      } else {
        showNotification("Already at the first dataset", type = "error", duration = 20)
      }
    })
    observeEvent(input$next_dataset, ignoreInit = TRUE, {
      s <- length(df_list())
      if (batch_state$current_dataset < s) {
        batch_state$current_dataset<-batch_state$current_dataset + 1L
        output$title_batch <- renderText(paste0("Dataset Nr.", batch_state$current_dataset))
      } else {
        showNotification("Already at the last dataset", type = "error", duration = 20)
      }
    })

    observeEvent(input$previous_signal_batch, ignoreInit = TRUE, {
      if (batch_state$current_signal > 1L) {
        batch_state$current_signal <- batch_state$current_signal - 1L
      } else {
        showNotification("Already at the first signal", type = "error", duration = 20)
      }
    })
    observeEvent(input$next_signal_batch, ignoreInit = TRUE, {
      if (batch_state$current_signal < nsigs()) {
        batch_state$current_signal <-batch_state$current_signal + 1L
      } else {
        showNotification("Already at the last signal", type = "error", duration = 20)
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
