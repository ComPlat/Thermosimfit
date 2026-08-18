server_opti_sensi_batch <- function(id, df_reactive, df_list_reactive, nclicks, task, progress_file) {
  df <- reactive({df_reactive$df})
  nsigs <- reactive({df_reactive$nsigs})
  df_list <- reactive({df_list_reactive$data_frames})
  moduleServer(id, function(input, output, session) {

    # Render parameter boundaries
    # ===============================================================================
    # Optimization and Batch-PSO each get an independently-editable copy of
    # this UI (Shiny can't render one uiOutput id into two DOM locations -
    # duplicate ids mean only one ever updates), but the markup/logic that
    # builds each copy is written once and parameterized by id_suffix
    # ("" for Optimization, "_batch" for Batch-PSO) rather than copy-pasted.
    parameter_state <- reactiveValues(idx = 1L, Is = list(), confirmed = list(), Is_old = list())
    parameter_state_batch <- reactiveValues(idx = 1L, Is = list(), confirmed = list(), Is_old = list())

    build_i_inputs <- function(id_suffix) {
      list(
        lb_I0  = input[[paste0("I0_lb", id_suffix)]],  ub_I0  = input[[paste0("I0_ub", id_suffix)]],
        lb_IHD = input[[paste0("IHD_lb", id_suffix)]], ub_IHD = input[[paste0("IHD_ub", id_suffix)]],
        lb_ID  = input[[paste0("ID_lb", id_suffix)]],  ub_ID  = input[[paste0("ID_ub", id_suffix)]]
      )
    }

    render_confirmed_i_badge <- function(state, id_suffix) {
      class <- "label label-default"
      status <- "Not confirmed"
      if (length(state$Is) >= state$idx) {
        if (state$confirmed[[state$idx]]) {
          old <- state$Is_old[[state$idx]]
          current <- build_i_inputs(id_suffix)
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
    }

    render_bounds_i <- function(state, id_suffix) {
      lb_I0 <- 0; ub_I0 <- 10^8;
      lb_IHD <- 0; ub_IHD <- 10^8;
      lb_ID <- 0; ub_ID <- 10^8;

      if (length(state$Is) >= state$idx) {
        Is <- state$Is[[state$idx]]
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
            h4(sprintf("I parameters — signal %s", state$idx), class = "m-0"),
            uiOutput(NS(id, paste0("Confirmed_I", id_suffix)), inline = TRUE)
          ),
          textInput(NS(id, paste0("I0_lb", id_suffix)), "I(0) value lower boundary", value = lb_I0) |> tagAppendAttributes(title = "Lower bound for I(0). Use 0 if unknown."),
          textInput(NS(id, paste0("I0_ub", id_suffix)), "I(0) value upper boundary", value = ub_I0) |> tagAppendAttributes(title = "Upper bound for I(0). Use 10^8 if unknown."),
          textInput(NS(id, paste0("IHD_lb", id_suffix)),
            label = tagList(
              "I(HD) value lower boundary [1/M]",
              actionButton(NS(id, paste0("AdviceUBIHD", id_suffix)), "Help",
                icon = icon("question-circle"),
                style = "background-color:transparent; border:none;"
              )
            ), value = lb_IHD
          ) |> tagAppendAttributes(title = "Lower bound for I(HD). Use 0 if unknown."),
          textInput(NS(id, paste0("IHD_ub", id_suffix)), "I(HD) value upper boundary [1/M]", value = ub_IHD) |> tagAppendAttributes(title = "Upper bound for I(HD). Use 10^8 if unknown."),
          textInput(NS(id, paste0("ID_lb", id_suffix)), "I(D) value lower boundary [1/M]", value = lb_ID) |> tagAppendAttributes(title = "Lower bound for I(D). Use 0 if unknown."),
          textInput(NS(id, paste0("ID_ub", id_suffix)), "I(D) value upper boundary [1/M]", value = ub_ID) |> tagAppendAttributes(title = "Upper bound for I(D). Use 10^8 if unknown."),
          actionButton(
            inputId = NS(id, paste0("Confirm", id_suffix)),
            label = "Confirm boundaries",
            class = "add-button df-button"
          ),
          actionButton(
            inputId = NS(id, paste0("PreviousIParameter", id_suffix)),
            label = "Previous boundaries",
            class = "add-button df-button"
          ),
          actionButton(
            inputId = NS(id, paste0("NextIParameter", id_suffix)),
            label = "Next boundaries",
            class = "add-button df-button"
          )
        )
      )
    }

    setup_bounds_i_observers <- function(state, id_suffix) {
      observeEvent(input[[paste0("Confirm", id_suffix)]], ignoreInit = TRUE, {
        state$Is[[state$idx]] <- build_i_inputs(id_suffix)
        state$Is_old[[state$idx]] <- state$Is[[state$idx]]
        state$confirmed[[state$idx]] <- TRUE
      })

      observeEvent(input[[paste0("NextIParameter", id_suffix)]], ignoreInit = TRUE, {
        if (state$idx < nsigs()) {
          state$idx <- state$idx + 1L
        } else {
          showNotification("Already at the last signal",
            type = "error", duration = 20
          )
        }
      })

      observeEvent(input[[paste0("PreviousIParameter", id_suffix)]], ignoreInit = TRUE, {
        if (state$idx > 1L) {
          state$idx <- state$idx - 1L
        } else {
          showNotification("Already at the first signal",
            type = "error", duration = 20
          )
        }
      })
    }

    setup_bounds_i_help_modals <- function(id_suffix) {
      observeEvent(input[[paste0("helpButton", id_suffix)]], {
        showModal(modalDialog(
          title = "Help",
          HTML("Conduct two optimizations. First with wide boundaries. \n
            Afterwards chose narrow boundaries based on the result of the first optimization."),
          easyClose = TRUE,
          footer = NULL
        ))
      })
      observeEvent(input[[paste0("AdviceUBIHD", id_suffix)]], {
        showModal(modalDialog(
          title = "Help",
          HTML("Set upper boundary to IHD * conc ≈ Signal"),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }

    output[["Confirmed_I"]] <- renderUI({ render_confirmed_i_badge(parameter_state, "") })
    output[["Confirmed_I_batch"]] <- renderUI({ render_confirmed_i_badge(parameter_state_batch, "_batch") })

    output[["BOUNDS_I"]] <- renderUI({ render_bounds_i(parameter_state, "") })
    output[["BOUNDS_I_BATCH"]] <- renderUI({ render_bounds_i(parameter_state_batch, "_batch") })

    setup_bounds_i_observers(parameter_state, "")
    setup_bounds_i_observers(parameter_state_batch, "_batch")

    setup_bounds_i_help_modals("")
    setup_bounds_i_help_modals("_batch")

    # Optimization
    # ===============================================================================
    invalid_time <- reactiveVal(1100)
   
    opti_result_created <- reactiveVal(FALSE)
    opti_result <- reactiveVal()
    process <- reactiveVal()
    cancel_clicked <- reactiveVal(FALSE)
    setup_done <- reactiveVal(FALSE)
    opti_result_signal_idx <- reactiveVal(1L)
    last_progress_pos <- reactiveVal(0)

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
        rwn(sensi_source_available(),
          "Please run first a PSO or VAPRO optimization matching the selected parameter source")
      } else if (id == "DBA") {
        rwn(input$D0 != "",
          "Please enter a value for the Dye")
        rwn(is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(sensi_source_available(),
          "Please run first a PSO or VAPRO optimization matching the selected parameter source")
      } else if (id == "IDA") {
        rwn(input$H0 != "",
          "Please enter a value for the Host")
        rwn(input$D0 != "",
          "Please enter a value for the Dye")
        rwn(input$kHD != "",
          "Please enter a value for KaHD")
        rwn(is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(sensi_source_available(),
          "Please run first a PSO or VAPRO optimization matching the selected parameter source")
      } else if (id == "GDA") {
        rwn(input$H0 != "",
          "Please enter a value for the Host")
        rwn(input$G0 != "",
          "Please enter a value for the Guest")
        rwn(input$kHD != "",
          "Please enter a value for KaHD")
        rwn(is_integer(input$sens_bounds),
          "Please enter an integer value for the sensitivity boundary")
        rwn(sensi_source_available(),
          "Please run first a PSO or VAPRO optimization matching the selected parameter source")
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

    # VAPRO-Batch: per-file opti_vapro_bootstrap, no replicate-seed axis
    # (deterministic) and no I-bounds (profiled via NNLS) - own fully
    # independent H0/D0/kHD/bounds/nGrid/nBoot/error_calc_fct inputs.
    check_inputs_vapro_batch <- function() {
      rwn(length(df_list()) > 0,
        "The dataset list seems to be empty. Please upload a file")
      rwn(!is.na(input$nGrid_vapro_batch),
        "Please enter a value for the number of VAPRO grid points")
      rwn(is_integer(input$nGrid_vapro_batch),
        "Please enter an integer value for the number of VAPRO grid points")

      if (id == "HG") {
        rwn(input$H0_vapro_batch != "", "Please enter a value for the Host")
        rwn(input$kHD_lb_vapro_batch != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub_vapro_batch != "",
          "Please enter a value for the upper boundary of KaHD")
      } else if (id == "DBA") {
        rwn(input$D0_vapro_batch != "", "Please enter a value for the Dye")
        rwn(input$kHD_lb_vapro_batch != "",
          "Please enter a value for the lower boundary of KaHD")
        rwn(input$kHD_ub_vapro_batch != "",
          "Please enter a value for the upper boundary of KaHD")
      } else if (id == "IDA") {
        rwn(input$H0_vapro_batch != "", "Please enter a value for the Host")
        rwn(input$D0_vapro_batch != "", "Please enter a value for the Dye")
        rwn(input$kHD_vapro_batch != "", "Please enter a value for KaHD")
        rwn(input$kHG_lb_vapro_batch != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub_vapro_batch != "",
          "Please enter a value for the upper boundary of KaHG")
      } else if (id == "GDA") {
        rwn(input$H0_vapro_batch != "", "Please enter a value for the Host")
        rwn(input$G0_vapro_batch != "", "Please enter a value for the Guest")
        rwn(input$kHD_vapro_batch != "", "Please enter a value for KaHD")
        rwn(input$kHG_lb_vapro_batch != "",
          "Please enter a value for the lower boundary of KaHG")
        rwn(input$kHG_ub_vapro_batch != "",
          "Please enter a value for the upper boundary of KaHG")
      }
    }

    create_lb_vapro_batch <- function() {
      if (id == "HG" || id == "DBA") {
        return(convert_all_to_num("lower boundary", input$kHD_lb_vapro_batch))
      } else {
        return(convert_all_to_num("lower boundary", input$kHG_lb_vapro_batch))
      }
    }

    create_ub_vapro_batch <- function() {
      if (id == "HG" || id == "DBA") {
        return(convert_all_to_num("upper boundary", input$kHD_ub_vapro_batch))
      } else {
        return(convert_all_to_num("upper boundary", input$kHG_ub_vapro_batch))
      }
    }

    create_additional_parameters_vapro_batch <- function() {
      if (id == "HG") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_vapro_batch
        )
        return(additionalParameters)
      } else if (id == "DBA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$D0_vapro_batch
        )
        return(additionalParameters)
      } else if (id == "IDA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_vapro_batch, input$D0_vapro_batch, input$kHD_vapro_batch
        )
        return(additionalParameters)
      } else if (id == "GDA") {
        additionalParameters <- convert_all_to_num(
          "Additional Parameters",
          input$H0_vapro_batch, input$G0_vapro_batch, input$kHD_vapro_batch
        )
        return(additionalParameters)
      }
    }

    create_nGrid_vapro_batch <- function() {
      convert_num_to_int(input$nGrid_vapro_batch)
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

    get_update_field_vapro_batch <- function() {
      if (id == "HG") {
        return("HGupdateFieldVaproBatch")
      } else if (id == "DBA") {
        return("DBAupdateFieldVaproBatch")
      } else if (id == "IDA") {
        return("IDAupdateFieldVaproBatch")
      } else if (id == "GDA") {
        return("GDAupdateFieldVaproBatch")
      }
    }

    get_update_field_uncertainty <- function() {
      if (id == "HG") {
        return("HGupdateFieldUncertainty")
      } else if (id == "DBA") {
        return("DBAupdateFieldUncertainty")
      } else if (id == "IDA") {
        return("IDAupdateFieldUncertainty")
      } else if (id == "GDA") {
        return("GDAupdateFieldUncertainty")
      }
    }

    get_update_field_vapro_uncertainty <- function() {
      if (id == "HG") {
        return("HGupdateFieldVaproUncertainty")
      } else if (id == "DBA") {
        return("DBAupdateFieldVaproUncertainty")
      } else if (id == "IDA") {
        return("IDAupdateFieldVaproUncertainty")
      } else if (id == "GDA") {
        return("GDAupdateFieldVaproUncertainty")
      }
    }
    # NOTE: End of model specific code
    # ===============================================================================

    sensi_source_available <- function() {
      source <- input$sensi_param_source
      if (is.null(source)) return(FALSE)
      if (identical(source, "vapro")) vapro_opti_result_created() else opti_result_created()
    }

    output$sensi_source_ui <- renderUI({
      choices <- c()
      if (opti_result_created()) choices["PSO"] <- "pso"
      if (vapro_opti_result_created()) choices["VAPRO"] <- "vapro"
      if (length(choices) == 0) {
        return(helpText("Run a PSO or VAPRO optimization first to enable sensitivity analysis."))
      }
      current <- isolate(input$sensi_param_source)
      selected <- if (!is.null(current) && current %in% choices) current else choices[[1]]
      radioButtons(NS(id, "sensi_param_source"), "Parameter source:",
        choices = choices, selected = selected, inline = TRUE
      )
    })

    get_opti_result <- function() {
      if (identical(input$sensi_param_source, "vapro")) {
        vapro_opti_result()$parameter
      } else {
        opti_result()$parameter
      }
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

      writeLines(character(0), progress_file)
      last_progress_pos(0)
      e <- try(task$start(
        function(case, lb, ub, df, ap, seed, npop, ngen, topo, et, ecf, progress_file) {
          con <- file(progress_file, open = "at")
          sink(con, type = "output")
          on.exit({
            sink(type = "output")
            close(con)
          }, add = TRUE)
          tsf::opti(case, lb, ub, df, ap, seed, npop, ngen, topo, et, ecf)
        },
        args = list(
          case = get_Model(), lb = lb, ub = ub, df = df(),
          ap = additionalParameters, seed = seed, npop = npop,
          ngen = ngen, topo = topo, et = et, ecf = ecf,
          progress_file = progress_file
        )
      ), silent = TRUE)
      if (inherits(e, "try-error")) {
        print_noti("Background task is still busy, please retry shortly", type = "warning")
        return(NULL)
      }

      # clear everything
      setup_done(FALSE)
      opti_result_created(FALSE)
      process(task)
      invalid_time(1100)
      nclicks(nclicks() + 1)
      opti_message("Running optimization...")
      setup_done(TRUE)
      NULL
    })

    process_done <- function() {
      req(setup_done())
      req(length(process()) > 0)
      req(process()$is_done())
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
      if (cancel_clicked()) {
        setup_done(TRUE)
        cancel_clicked(FALSE)
        nclicks(0)
        process()$cancel()
        e <- try(opti_result(process()$get_result()))
        if (inherits(e, "try-error")) {
          opti_result(NULL)
          opti_result_created(FALSE)
        }
        process(NULL)
        opti_message("")
        return(NULL)
      }
      # live status: sink() inside the background call writes each
      # generation's progress into progress_file, read back here
      # incrementally like the old read_output() polling did.
      info <- file.info(progress_file)
      req(!is.na(info$size))
      req(info$size > last_progress_pos())
      con <- file(progress_file, "rb")
      on.exit(close(con))
      seek(con, last_progress_pos())
      raw_new <- readChar(con, info$size - last_progress_pos(), useBytes = TRUE)
      last_progress_pos(info$size)
      m <- print_status(raw_new, get_Model())
      req(is.character(m))
      req(length(m) > 0)
      m <- Reduce(function(a, b) paste0(a, "\n", b), m)
      if (m != "") opti_message(m)
    })

    get_opti_data <- reactive({
      req(process()$is_done())
      process()$collect()
      e <- try(opti_result(process()$get_result()))
      if (inherits(e, "try-error") || is.null(opti_result())) {
        opti_message("")
        opti_result(NULL)
        process(NULL)
        return(NULL)
      }
      opti_message("")
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
      lb <- create_lb_vapro()
      ub <- create_ub_vapro()
      additionalParameters <- create_additional_parameters_vapro()
      nGrid <- create_nGrid()
      ecf <- input$error_calc_fct_vapro

      e <- try(task$start(
        function(case, lb, ub, df, ap, nGrid, ecf) {
          tsf::opti_vapro(case, lb, ub, df, ap, nGrid, ecf)
        },
        args = list(
          case = get_Model(), lb = lb, ub = ub, df = df(),
          ap = additionalParameters, nGrid = nGrid, ecf = ecf
        )
      ), silent = TRUE)
      if (inherits(e, "try-error")) {
        print_noti("Background task is still busy, please retry shortly", type = "warning")
        return(NULL)
      }

      # clear everything
      vapro_setup_done(FALSE)
      vapro_opti_result_created(FALSE)
      vapro_process(task)
      invalid_time(1100)
      nclicks(nclicks() + 1)
      vapro_message("Running VAPRO optimization...")
      vapro_setup_done(TRUE)
      NULL
    })

    vapro_process_done <- function() {
      req(vapro_setup_done())
      req(length(vapro_process()) > 0)
      req(vapro_process()$is_done())
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }

    correct_results_vapro <- function() {
      req(vapro_opti_result_created())
      req(!is.null(vapro_opti_result()))
    }

    get_vapro_opti_data <- reactive({
      req(vapro_process()$is_done())
      vapro_process()$collect()
      e <- try(vapro_opti_result(vapro_process()$get_result()))
      is_error <- inherits(e, "try-error") || is.null(vapro_opti_result()) ||
        inherits(vapro_opti_result(), "ErrorClass")
      if (is_error) {
        if (inherits(vapro_opti_result(), "ErrorClass")) {
          print_error(vapro_opti_result()$message)
        }
        vapro_opti_result(NULL)
        vapro_message("")
        vapro_process(NULL)
        return(NULL)
      }
      vapro_message("")
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
    last_sensi_progress_pos <- reactiveVal(0)
    sensi_progress_count <- reactiveVal(0L)

    observeEvent(input$Start_Sensi, {
      # checks
      if (nclicks() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      check_inputs_sensi()
      additionalParameters <- create_additional_parameters()
      optim_params <- get_opti_result()
      sense_bounds <- get_sens_bounds()
      ecf <- input$error_calc_fct

      writeLines(character(0), progress_file)
      last_sensi_progress_pos(0)
      sensi_progress_count(0L)
      e <- try(task$start(
        function(case, optim_params, df, ap, sense_bounds, error_calc_fct, progress_file) {
          con <- file(progress_file, open = "at")
          sink(con, type = "output")
          on.exit({
            sink(type = "output")
            close(con)
          }, add = TRUE)
          tsf::sensitivity(case, optim_params, df, ap, sense_bounds, error_calc_fct = error_calc_fct)
        },
        args = list(
          case = get_Model(), optim_params = optim_params, df = df(),
          ap = additionalParameters, sense_bounds = sense_bounds,
          error_calc_fct = ecf, progress_file = progress_file
        )
      ), silent = TRUE)
      if (inherits(e, "try-error")) {
        print_noti("Background task is still busy, please retry shortly", type = "warning")
        return(NULL)
      }

      # clear everything
      sensi_setup_done(FALSE)
      sensi_process(task)
      sensi_result_created(FALSE)
      invalid_time(1100)
      nclicks(nclicks() + 1)
      sensi_message("Initializing...")
      sensi_setup_done(TRUE)
      NULL
    })

    sensi_process_done <- function() {
      req(sensi_setup_done())
      req(length(sensi_process()) > 0)
      req(sensi_process()$is_done())
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
        sensi_process()$cancel()
        sensi_result(NULL)
        sensi_message("")
        sensi_process(NULL)
        return(NULL)
      }
      # live status, same file-based mechanism as the Optimization tab
      info <- file.info(progress_file)
      req(!is.na(info$size))
      req(info$size > last_sensi_progress_pos())
      con <- file(progress_file, "rb")
      on.exit(close(con))
      seek(con, last_sensi_progress_pos())
      raw_new <- readChar(con, info$size - last_sensi_progress_pos(), useBytes = TRUE)
      last_sensi_progress_pos(info$size)
      lines <- strsplit(raw_new, "\n")[[1]]
      lines <- gsub('"', "", lines)
      lines <- gsub("\\[.*?\\] ", "", lines)
      nums <- suppressWarnings(as.integer(lines))
      nums <- nums[!is.na(nums)]
      if (length(nums) > 0) {
        sensi_progress_count(max(nums))
      }
      req(sensi_progress_count() > 0)
      sensi_message(paste0("Completed: ", sensi_progress_count(), "%"))
    })

    get_sensi_result <- reactive({
      req(sensi_process()$is_done())
      sensi_process()$collect()
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
            sensi_process(NULL)
            return(NULL)
          }
        })
        sensi_result_created(TRUE)
        sensi_process(NULL)
      }
    })

    output$sensi_plot <- renderPlot({
      req(sensi_result_created())
      exportTestValues(
        sense_plot = {
          sensi_result()
        }
      )
      plot_sensitivity_result(sensi_result())
    })

    output$sensi_download <- downloadHandler(
      filename = function() "result.zip",
      content = function(file) {
        req(sensi_result_created())
        file <- normalizePath(file, mustWork = FALSE)
        tmp_dir <- tempfile()
        dir.create(tmp_dir)
        old_wd <- setwd(tmp_dir)
        on.exit(setwd(old_wd))
        write.table(sensi_result(), "result.csv", sep = ",", row.names = FALSE, col.names = TRUE)
        ggsave("result.png", plot = plot_sensitivity_result(sensi_result()), width = 8, height = 5)
        utils::zip(file, c("result.csv", "result.png"))
      }
    )

    # Batch analysis
    # ===============================================================================
    setup_batch_done <- reactiveVal(FALSE)
    batch_results_created <- reactiveVal(FALSE)
    cancel_batch_clicked <- reactiveVal(FALSE)
    num_rep_batch <- reactiveVal()
    batch_process <- reactiveVal(NULL)
    last_batch_progress_pos <- reactiveVal(0)
    batch_progress_count <- reactiveVal(0L)
    batch_total <- reactiveVal(1L)
    result_batch <- reactiveVal()
    result_batch_raw <- reactiveVal()
    batch_state <- reactiveValues(current_dataset = 1L, current_signal = 1L)

    batch_progress_text <- function(done, total) {
      pct <- if (total > 0) round(100 * done / total) else 0
      bar_done <- strrep("=", done)
      bar_todo <- strrep(" ", max(total - done, 0))
      sprintf("[%s%s] %d%% (%d/%d jobs)", bar_done, bar_todo, pct, done, total)
    }

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
      # check seed case
      seed <- input$Seed_batch
      num_rep <- as.integer(input$NumRepDataset)
      num_rep_batch(num_rep)
      seed_case <- determine_seed_case(seed, num_rep)
      seed_origin <- NULL
      if (seed_case == 3) {
        seed_origin <- seed
      }
      size <- length(df_list()) * num_rep
      seeds <- numeric(size)
      seeds_from <- 1:1e6

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

      # 2. Create message lists for each job
      messages <- character(size)
      counter_messages <- 1
      for (i in seq_len(length(df_list()))) {
        for (j in seq_len(num_rep)) {
          messages[counter_messages] <-
            paste0("Dataset = ", i, "; Replicate = ", j)
          counter_messages <- counter_messages + 1
        }
      }
      dfs <- rep(df_list(), each = num_rep)

      writeLines(character(0), progress_file)
      last_batch_progress_pos(0)
      e <- try(task$start(
        function(case, lb, ub, dfs, ap, seeds, npop, ngen, topo, ecf, et, messages, progress_file) {
          con <- file(progress_file, open = "at")
          sink(con, type = "output")
          on.exit({
            sink(type = "output")
            close(con)
          }, add = TRUE)
          tsf:::run_batch_sequential_pso(case, lb, ub, dfs, ap, seeds, npop, ngen, topo, ecf, et, messages)
        },
        args = list(
          case = get_Model(), lb = lb, ub = ub, dfs = dfs, ap = additionalParameters,
          seeds = seeds, npop = npop, ngen = ngen, topo = topo, ecf = ecf, et = et,
          messages = messages, progress_file = progress_file
        )
      ), silent = TRUE)
      if (inherits(e, "try-error")) {
        print_noti("Background task is still busy, please retry shortly", type = "warning")
        return(NULL)
      }

      # clear everything
      result_batch(NULL)
      invalid_time(1100)
      setup_batch_done(FALSE)
      batch_results_created(FALSE)
      batch_process(task)
      batch_progress_count(0L)
      batch_total(size)
      nclicks(nclicks() + 1)
      batch_message(batch_progress_text(0L, size))
      setup_batch_done(TRUE)
      NULL
    })

    batch_process_done <- function() {
      req(setup_batch_done())
      req(!is.null(batch_process()))
      req(batch_process()$is_done())
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
      info <- file.info(progress_file)
      m <- ""
      if (!is.na(info$size) && info$size > last_batch_progress_pos()) {
        con <- file(progress_file, "rb")
        seek(con, last_batch_progress_pos())
        raw_new <- readChar(con, info$size - last_batch_progress_pos(), useBytes = TRUE)
        close(con)
        last_batch_progress_pos(info$size)

        job_matches <- regmatches(raw_new, gregexpr("BATCH_JOB_DONE \\d+/\\d+", raw_new))[[1]]
        if (length(job_matches) > 0) {
          done <- sub("BATCH_JOB_DONE (\\d+)/\\d+", "\\1", job_matches[length(job_matches)])
          batch_progress_count(as.integer(done))
        }

        status_only <- gsub("BATCH_JOB_DONE \\d+/\\d+\\n?", "", raw_new)
        status <- print_status(status_only, get_Model())
        if (is.character(status) && length(status) > 0) {
          m <- Reduce(function(a, b) paste0(a, "\n", b), status)
        }
      }
      progress_bar <- batch_progress_text(batch_progress_count(), batch_total())
      full <- if (nchar(m) > 0) paste0(m, "\n", progress_bar) else progress_bar
      batch_message(full)
    }

    # observe status
    observe({
      invalidateLater(invalid_time())
      req(nclicks() != 0)
      req(!is.null(batch_process()))
      # is cancel_batch_clicked
      if (cancel_batch_clicked()) {
        batch_process()$cancel()
        setup_batch_done(TRUE)
        cancel_batch_clicked(FALSE)
        nclicks(0)
        batch_process(NULL)
        batch_message("")
        return(NULL)
      }
      update_status()
    })

    get_data <- reactive({
      req(batch_process()$is_done())
      batch_process()$collect()
      raw <- try(batch_process()$get_result())
      if (inherits(raw, "try-error")) {
        batch_message("")
        print_error("Error in background process")
        result_batch_raw(NULL)
        return(NULL)
      }
      raw <- Filter(Negate(is.null), raw)
      result_batch_raw(seperate_batch_results(raw))
    })

    # observe results
    observe({
      invalidateLater(invalid_time())
      if (batch_process_done() && !batch_results_created()) {
        get_data()
        batch_results_created(TRUE)
        # NOTE: clear status
        session$sendCustomMessage(
          type = get_update_field_batch(),
          list(message = "")
        )
        values <- result_batch_raw()
        req(!is.null(values))
        e <- try({
          state_plots <- plotStatesBatch(values, get_Model())
          i_param_plots <- plotIParamsBatch(values, num_rep_batch())
          ka_plots <- plotKaBatch(values, num_rep_batch())
          metrices_plots <- plotMetricesBatch(values, num_rep_batch())
          hd_d_plots <- plotDAndHDBatch(values, num_rep_batch())
        })
        if (inherits(e, "try-error")) {
          print_noti("Run failed", type = "warning")
        } else {
          result_batch(structure(list(
            values, state_plots = state_plots,
            i_param_plots = i_param_plots,
            ka_plots = ka_plots,
            metrices_plots = metrices_plots,
            hd_d_plots = hd_d_plots
          ), class = "BatchResult"))
          batch_process(NULL)
        }
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

    # VAPRO-Batch
    # ===============================================================================
    vapro_batch_result_created <- reactiveVal(FALSE)
    vapro_batch_result <- reactiveVal()
    vapro_batch_process <- reactiveVal()
    vapro_batch_setup_done <- reactiveVal(FALSE)
    vapro_batch_total <- reactiveVal(1L)
    vapro_batch_progress_count <- reactiveVal(0L)
    vapro_batch_dataset_idx <- reactiveVal(1L)
    last_vapro_batch_progress_pos <- reactiveVal(0)

    vapro_batch_message <- function(message) {
      session$sendCustomMessage(
        type = get_update_field_vapro_batch(),
        list(message = message)
      )
      return(NULL)
    }

    vapro_batch_progress_text <- function(done, total) {
      pct <- if (total > 0) round(100 * done / total) else 0
      bar_done <- strrep("=", done)
      bar_todo <- strrep(" ", max(total - done, 0))
      sprintf("[%s%s] %d%% (%d/%d datasets)", bar_done, bar_todo, pct, done, total)
    }

    observeEvent(input$Start_Vapro_Batch, {
      if (nclicks() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      check_inputs_vapro_batch()
      lb <- create_lb_vapro_batch()
      ub <- create_ub_vapro_batch()
      additionalParameters <- create_additional_parameters_vapro_batch()
      nGrid <- create_nGrid_vapro_batch()
      ecf <- input$error_calc_fct_vapro_batch
      dfs <- df_list()

      writeLines(character(0), progress_file)
      last_vapro_batch_progress_pos(0)
      e <- try(task$start(
        function(case, lb, ub, dfs, ap, nGrid, ecf, progress_file) {
          con <- file(progress_file, open = "at")
          sink(con, type = "output")
          on.exit({
            sink(type = "output")
            close(con)
          }, add = TRUE)
          results <- vector("list", length(dfs))
          for (i in seq_along(dfs)) {
            results[[i]] <- tryCatch(
              tsf::opti_vapro(case, lb, ub, dfs[[i]], ap, nGrid, ecf),
              error = function(e) NULL
            )
            cat(i, "\n")
            flush(stdout())
          }
          results
        },
        args = list(
          case = get_Model(), lb = lb, ub = ub, dfs = dfs,
          ap = additionalParameters, nGrid = nGrid, ecf = ecf,
          progress_file = progress_file
        )
      ), silent = TRUE)
      if (inherits(e, "try-error")) {
        print_noti("Background task is still busy, please retry shortly", type = "warning")
        return(NULL)
      }

      vapro_batch_setup_done(FALSE)
      vapro_batch_result_created(FALSE)
      vapro_batch_process(task)
      vapro_batch_progress_count(0L)
      vapro_batch_total(length(dfs))
      vapro_batch_dataset_idx(1L)
      invalid_time(1100)
      nclicks(nclicks() + 1)
      vapro_batch_message(vapro_batch_progress_text(0L, length(dfs)))
      vapro_batch_setup_done(TRUE)
      NULL
    })

    vapro_batch_process_done <- function() {
      req(vapro_batch_setup_done())
      req(length(vapro_batch_process()) > 0)
      req(vapro_batch_process()$is_done())
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }

    correct_results_vapro_batch <- function() {
      req(vapro_batch_result_created())
      req(!is.null(vapro_batch_result()))
    }

    get_vapro_batch_data <- reactive({
      req(vapro_batch_process()$is_done())
      vapro_batch_process()$collect()
      e <- try(vapro_batch_result(vapro_batch_process()$get_result()))
      is_error <- inherits(e, "try-error") || is.null(vapro_batch_result()) ||
        inherits(vapro_batch_result(), "ErrorClass")
      if (is_error) {
        if (inherits(vapro_batch_result(), "ErrorClass")) {
          print_error(vapro_batch_result()$message)
        }
        vapro_batch_result(NULL)
        vapro_batch_message("")
        vapro_batch_process(NULL)
        return(NULL)
      }
      vapro_batch_message("")
      vapro_batch_process(NULL)
    })

    # poll: live progress text while running
    observe({
      invalidateLater(invalid_time())
      req(nclicks() != 0)
      req(!is.null(vapro_batch_process()))
      info <- file.info(progress_file)
      if (!is.na(info$size) && info$size > last_vapro_batch_progress_pos()) {
        con <- file(progress_file, "rb")
        seek(con, last_vapro_batch_progress_pos())
        raw_new <- readChar(con, info$size - last_vapro_batch_progress_pos(), useBytes = TRUE)
        close(con)
        last_vapro_batch_progress_pos(info$size)
        lines <- strsplit(raw_new, "\n")[[1]]
        nums <- suppressWarnings(as.integer(lines))
        nums <- nums[!is.na(nums)]
        if (length(nums) > 0) {
          vapro_batch_progress_count(max(nums))
        }
      }
      vapro_batch_message(vapro_batch_progress_text(vapro_batch_progress_count(), vapro_batch_total()))
    })

    # observe results
    observe({
      invalidateLater(invalid_time())
      if (vapro_batch_process_done() && !vapro_batch_result_created()) {
        get_vapro_batch_data()
        vapro_batch_result_created(TRUE)
      }
    })

    output$title_vapro_batch <- renderText(paste0("Dataset Nr.", vapro_batch_dataset_idx()))
    output$dataset_label_vapro_batch <- renderText(paste0("Dataset Nr.", vapro_batch_dataset_idx()))

    output$Ka_main_plot_vapro_batch <- renderPlot({
      correct_results_vapro_batch()
      results <- vapro_batch_result()
      rows <- lapply(seq_along(results), function(i) {
        res <- results[[i]]
        if (is.null(res)) {
          return(NULL)
        }
        data.frame(dataset = i, param_name = names(res$parameter)[1], value = res$parameter[[1]])
      })
      rows <- Filter(Negate(is.null), rows)
      req(length(rows) > 0)
      plot_df <- Reduce(rbind, rows)
      ggplot(plot_df, aes(x = factor(dataset), y = value)) +
        geom_point(size = 3) +
        labs(x = "Dataset", y = plot_df$param_name[1]) +
        theme_minimal()
    })

    output$summary_table_vapro_batch <- renderDT({
      correct_results_vapro_batch()
      res <- vapro_batch_result()[[vapro_batch_dataset_idx()]]
      req(!is.null(res))
      params <- res$parameter
      names(params)[1] <- get_K_param()
      datatable(params, escape = FALSE, caption = "VAPRO parameter estimates") |>
        formatSignif(columns = 1:ncol(params), digits = 3)
    })

    observeEvent(input$previous_dataset_vapro_batch, ignoreInit = TRUE, {
      if (vapro_batch_dataset_idx() > 1L) {
        vapro_batch_dataset_idx(vapro_batch_dataset_idx() - 1L)
      } else {
        showNotification("Already at the first dataset", type = "error", duration = 20)
      }
    })
    observeEvent(input$next_dataset_vapro_batch, ignoreInit = TRUE, {
      if (vapro_batch_dataset_idx() < vapro_batch_total()) {
        vapro_batch_dataset_idx(vapro_batch_dataset_idx() + 1L)
      } else {
        showNotification("Already at the last dataset", type = "error", duration = 20)
      }
    })

    output$vapro_batch_download <- downloadHandler(
      filename = function() "result_vapro_batch.csv",
      content = function(file) {
        correct_results_vapro_batch()
        rows <- lapply(seq_along(vapro_batch_result()), function(i) {
          res <- vapro_batch_result()[[i]]
          if (is.null(res)) {
            return(NULL)
          }
          cbind(dataset = i, res$parameter, res$metrices)
        })
        rows <- Filter(Negate(is.null), rows)
        out <- Reduce(rbind, rows)
        write.table(out, file, sep = ",", row.names = FALSE)
      }
    )

    # PSO Uncertainty
    # ===============================================================================
    # Two independent routes to a PSO uncertainty estimate: "batch" pools the
    # best-by-error repetitions of an already-run Batch-PSO analysis across
    # datasets (real inter-file measurement noise, but needs that run first,
    # same as Sensitivity needing a prior Optimization run); "direct" is a
    # residual bootstrap around the Optimization tab's fit (self-contained,
    # synthetic noise only). Both background results are normalized to the
    # same param/estimate/lower/upper shape before storage so the render
    # code below doesn't need to know which method produced them.
    uncertainty_result_created <- reactiveVal(FALSE)
    uncertainty_result <- reactiveVal()
    uncertainty_process <- reactiveVal()
    uncertainty_setup_done <- reactiveVal(FALSE)
    cancel_uncertainty_clicked <- reactiveVal(FALSE)

    uncertainty_message <- function(message) {
      session$sendCustomMessage(
        type = get_update_field_uncertainty(),
        list(message = message)
      )
      return(NULL)
    }

    check_inputs_uncertainty <- function() {
      if (identical(input$uncertainty_method, "direct")) {
        rwn(opti_result_created(),
          "Please run an optimization first")
        rwn(!is.na(input$unc_direct_nBoot),
          "Please enter a value for the number of bootstrap replicates")
        rwn(is_integer(input$unc_direct_nBoot),
          "Please enter an integer value for the number of bootstrap replicates")
      } else {
        rwn(batch_results_created(),
          "Please run a batch analysis first")
        rwn(!is.na(input$unc_best_pct),
          "Please enter a value for the best-by-error percentage")
        rwn(!is.na(input$unc_n_boot),
          "Please enter a value for the number of bootstrap replicates")
        rwn(is_integer(input$unc_n_boot),
          "Please enter an integer value for the number of bootstrap replicates")
      }
    }

    observeEvent(input$Start_Uncertainty, {
      if (nclicks() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      check_inputs_uncertainty()
      method <- input$uncertainty_method

      e <- if (identical(method, "direct")) {
        lb <- create_lb()
        ub <- create_ub()
        ap <- create_additional_parameters()
        npop <- create_npop()
        ngen <- create_ngen()
        nBoot <- convert_num_to_int(input$unc_direct_nBoot)
        seed <- input$unc_direct_seed
        if (is.na(seed)) seed <- as.numeric(Sys.time())
        try(task$start(
          function(case, lb, ub, df, ap, nBoot, npop, ngen, seed) {
            res <- tsf::opti_bootstrap(case, lb, ub, df, ap,
              nBoot = nBoot, npop = npop, ngen = ngen, seed = seed,
              showProgress = FALSE
            )
            if (inherits(res, "ErrorClass")) {
              return(res)
            }
            data.frame(
              param = res$summary$param, estimate = res$summary$q500,
              lower = res$summary$q025, upper = res$summary$q975,
              row.names = NULL
            )
          },
          args = list(
            case = get_Model(), lb = lb, ub = ub, df = df(), ap = ap,
            nBoot = nBoot, npop = npop, ngen = ngen, seed = seed
          )
        ), silent = TRUE)
      } else {
        values <- result_batch_raw()
        best_pct <- input$unc_best_pct
        n_boot <- convert_num_to_int(input$unc_n_boot)
        try(task$start(
          function(values, best_pct, n_boot) {
            res <- tsf::pso_uncertainty_batch(values, best_pct = best_pct, n_boot = n_boot)
            if (inherits(res, "ErrorClass")) {
              return(res)
            }
            data.frame(
              param = names(res$mode), estimate = res$mode,
              lower = res$lower_ci, upper = res$upper_ci,
              row.names = NULL
            )
          },
          args = list(values = values, best_pct = best_pct, n_boot = n_boot)
        ), silent = TRUE)
      }
      if (inherits(e, "try-error")) {
        print_noti("Background task is still busy, please retry shortly", type = "warning")
        return(NULL)
      }

      uncertainty_setup_done(FALSE)
      uncertainty_result_created(FALSE)
      uncertainty_process(task)
      invalid_time(1100)
      nclicks(nclicks() + 1)
      uncertainty_message("Running uncertainty estimation... this can take a while.")
      uncertainty_setup_done(TRUE)
      NULL
    })

    observeEvent(input$cancel_Uncertainty, {
      req(nclicks() != 0)
      cancel_uncertainty_clicked(TRUE)
    })

    observe({
      invalidateLater(invalid_time())
      req(nclicks() != 0)
      req(!is.null(uncertainty_process()))
      if (cancel_uncertainty_clicked()) {
        uncertainty_process()$cancel()
        uncertainty_setup_done(TRUE)
        cancel_uncertainty_clicked(FALSE)
        nclicks(0)
        uncertainty_process(NULL)
        uncertainty_message("")
      }
    })

    uncertainty_process_done <- function() {
      req(uncertainty_setup_done())
      req(!is.null(uncertainty_process()))
      req(uncertainty_process()$is_done())
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }

    get_uncertainty_data <- reactive({
      req(uncertainty_process()$is_done())
      uncertainty_process()$collect()
      e <- try(uncertainty_result(uncertainty_process()$get_result()))
      is_error <- inherits(e, "try-error") || is.null(uncertainty_result()) ||
        inherits(uncertainty_result(), "ErrorClass")
      if (is_error) {
        if (inherits(uncertainty_result(), "ErrorClass")) {
          print_error(uncertainty_result()$message)
        }
        uncertainty_result(NULL)
        uncertainty_message("")
        uncertainty_process(NULL)
        return(NULL)
      }
      uncertainty_message("")
      uncertainty_process(NULL)
    })

    observe({
      invalidateLater(invalid_time())
      if (uncertainty_process_done() && !uncertainty_result_created()) {
        get_uncertainty_data()
        uncertainty_result_created(TRUE)
      }
    })

    correct_results_uncertainty <- function() {
      req(uncertainty_result_created())
      req(!is.null(uncertainty_result()))
    }

    output$uncertainty_plot <- renderPlot({
      correct_results_uncertainty()
      plot_uncertainty_result(uncertainty_result())
    })

    output$uncertainty_table <- renderDT({
      correct_results_uncertainty()
      datatable(uncertainty_result(), rownames = FALSE) |>
        formatSignif(columns = c("estimate", "lower", "upper"), digits = 3)
    })

    output$uncertainty_download <- downloadHandler(
      filename = function() "result_uncertainty.csv",
      content = function(file) {
        correct_results_uncertainty()
        write.table(uncertainty_result(), file, sep = ",", row.names = FALSE, col.names = TRUE)
      }
    )

    # VAPRO Uncertainty
    # ===============================================================================
    # Residual bootstrap around the VAPRO Optimization tab's fit
    # (opti_vapro_bootstrap, self-contained like the PSO "direct" method) -
    # reuses that tab's lb/ub/additionalParameters/nGrid, same as Sensitivity
    # reuses a prior Optimization run. Always runs the ast2ast engine (fast,
    # matches the rest of the app's default) - that engine only supports
    # Rel. Error, so error_calc_fct isn't exposed here.
    vapro_uncertainty_result_created <- reactiveVal(FALSE)
    vapro_uncertainty_result <- reactiveVal()
    vapro_uncertainty_process <- reactiveVal()
    vapro_uncertainty_setup_done <- reactiveVal(FALSE)
    cancel_vapro_uncertainty_clicked <- reactiveVal(FALSE)

    vapro_uncertainty_message <- function(message) {
      session$sendCustomMessage(
        type = get_update_field_vapro_uncertainty(),
        list(message = message)
      )
      return(NULL)
    }

    observeEvent(input$Start_Vapro_Uncertainty, {
      if (nclicks() != 0) {
        print_noti("Already running analysis", type = "warning")
        return(NULL)
      }
      rwn(vapro_opti_result_created(),
        "Please run a VAPRO optimization first")
      rwn(!is.na(input$vapro_unc_nBoot),
        "Please enter a value for the number of bootstrap replicates")
      rwn(is_integer(input$vapro_unc_nBoot),
        "Please enter an integer value for the number of bootstrap replicates")

      lb <- create_lb_vapro()
      ub <- create_ub_vapro()
      ap <- create_additional_parameters_vapro()
      nGrid <- create_nGrid()
      nBoot <- convert_num_to_int(input$vapro_unc_nBoot)

      e <- try(task$start(
        function(case, lb, ub, df, ap, nBoot, nGrid) {
          res <- tsf::opti_vapro_bootstrap(case, lb, ub, df, ap,
            nBoot = nBoot, nGrid = nGrid, engine = "ast2ast", showProgress = FALSE
          )
          if (inherits(res, "ErrorClass")) {
            return(res)
          }
          data.frame(
            param = res$summary$param, estimate = res$summary$q500,
            lower = res$summary$q025, upper = res$summary$q975,
            row.names = NULL
          )
        },
        args = list(
          case = get_Model(), lb = lb, ub = ub, df = df(), ap = ap,
          nBoot = nBoot, nGrid = nGrid
        )
      ), silent = TRUE)
      if (inherits(e, "try-error")) {
        print_noti("Background task is still busy, please retry shortly", type = "warning")
        return(NULL)
      }

      vapro_uncertainty_setup_done(FALSE)
      vapro_uncertainty_result_created(FALSE)
      vapro_uncertainty_process(task)
      invalid_time(1100)
      nclicks(nclicks() + 1)
      vapro_uncertainty_message("Running uncertainty estimation... this can take a while.")
      vapro_uncertainty_setup_done(TRUE)
      NULL
    })

    observeEvent(input$cancel_Vapro_Uncertainty, {
      req(nclicks() != 0)
      cancel_vapro_uncertainty_clicked(TRUE)
    })

    observe({
      invalidateLater(invalid_time())
      req(nclicks() != 0)
      req(!is.null(vapro_uncertainty_process()))
      if (cancel_vapro_uncertainty_clicked()) {
        vapro_uncertainty_process()$cancel()
        vapro_uncertainty_setup_done(TRUE)
        cancel_vapro_uncertainty_clicked(FALSE)
        nclicks(0)
        vapro_uncertainty_process(NULL)
        vapro_uncertainty_message("")
      }
    })

    vapro_uncertainty_process_done <- function() {
      req(vapro_uncertainty_setup_done())
      req(!is.null(vapro_uncertainty_process()))
      req(vapro_uncertainty_process()$is_done())
      invalid_time(invalid_time() + 1000)
      nclicks(0)
      return(TRUE)
    }

    get_vapro_uncertainty_data <- reactive({
      req(vapro_uncertainty_process()$is_done())
      vapro_uncertainty_process()$collect()
      e <- try(vapro_uncertainty_result(vapro_uncertainty_process()$get_result()))
      is_error <- inherits(e, "try-error") || is.null(vapro_uncertainty_result()) ||
        inherits(vapro_uncertainty_result(), "ErrorClass")
      if (is_error) {
        if (inherits(vapro_uncertainty_result(), "ErrorClass")) {
          print_error(vapro_uncertainty_result()$message)
        }
        vapro_uncertainty_result(NULL)
        vapro_uncertainty_message("")
        vapro_uncertainty_process(NULL)
        return(NULL)
      }
      vapro_uncertainty_message("")
      vapro_uncertainty_process(NULL)
    })

    observe({
      invalidateLater(invalid_time())
      if (vapro_uncertainty_process_done() && !vapro_uncertainty_result_created()) {
        get_vapro_uncertainty_data()
        vapro_uncertainty_result_created(TRUE)
      }
    })

    correct_results_vapro_uncertainty <- function() {
      req(vapro_uncertainty_result_created())
      req(!is.null(vapro_uncertainty_result()))
    }

    output$vapro_uncertainty_plot <- renderPlot({
      correct_results_vapro_uncertainty()
      plot_uncertainty_result(vapro_uncertainty_result())
    })

    output$vapro_uncertainty_table <- renderDT({
      correct_results_vapro_uncertainty()
      datatable(vapro_uncertainty_result(), rownames = FALSE) |>
        formatSignif(columns = c("estimate", "lower", "upper"), digits = 3)
    })

    output$vapro_uncertainty_download <- downloadHandler(
      filename = function() "result_vapro_uncertainty.csv",
      content = function(file) {
        correct_results_vapro_uncertainty()
        write.table(vapro_uncertainty_result(), file, sep = ",", row.names = FALSE, col.names = TRUE)
      }
    )

  })
}
