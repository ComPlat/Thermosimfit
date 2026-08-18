dbaUI <- function(id) {
  tabItem(
    tabName = "DBA",
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateField', function(message) {
              var result = message.message;
              $('#DBA-output').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldVapro', function(message) {
              var result = message.message;
              $('#DBA-output_vapro').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldSense', function(message) {
              var result = message.message;
              $('#DBA-output_sense').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldBatch', function(message) {
              var result = message.message;
              $('#DBA-output_Batch').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldVaproBatch', function(message) {
              var result = message.message;
              $('#DBA-output_Vapro_Batch').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldUncertainty', function(message) {
              var result = message.message;
              $('#DBA-output_uncertainty').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldVaproUncertainty', function(message) {
              var result = message.message;
              $('#DBA-output_vapro_uncertainty').html(result);
            });"
    ),
    fluidRow(
      tabBox(
        id = NS(id, "ResultPanel"),
        tabPanel(
          "Optimization",
          fluidRow(
            box(
              textInput(NS(id, "D0"), "Dye conc. [M]", value = 0),
              box(
                title = "Advanced options",
                collapsible = TRUE, collapsed = TRUE,
                box(
                  numericInput(NS(id, "npop"), "Number of particles", value = 40),
                  numericInput(NS(id, "ngen"), "Number of generations", value = 1000),
                  selectInput(NS(id, "topology"), "Topology of particle swarm",
                    c(
                      "star" = "star",
                      "random arbitrary neighberhood" = "random"
                    ),
                    selected = "random",
                    selectize = FALSE
                  )
                ),
                box(
                  numericInput(NS(id, "threshold"), "Threshold of the error", value = 0.00001),
                  selectInput(NS(id, "error_calc_fct"), "Function to calculate the error:",
                    c(
                      "rel. Error" = "rel. Error",
                      "RMSE" = "RMSE",
                      "SSE" = "SSE",
                      "Huber" = "Huber"
                    )
                  ),
                  numericInput(NS(id, "Seed"), "Seed which should be set", value = NULL)
                ),
                width = 12
              ),
              width = 6,
              title = "Parameter", solidHeader = TRUE,
              status = "warning", height = 700
            ),
            box(
              box(
                textInput(NS(id, "kHD_lb"), HTML("K<sub>a</sub>(HD) value lower boundary [1/M]"), value = 10),
                textInput(NS(id, "kHD_ub"), HTML("K<sub>a</sub>(HD) value upper boundary [1/M]"), value = 1e08)
              ),
              uiOutput(NS(id, "BOUNDS_I")),
              width = 6,
              title = tagList(
                "Boundaries",
                actionButton(NS(id, "helpButton"), "Help",
                  icon = icon("question-circle"),
                  style = "background-color:transparent; border:none;"
                )
              ),
              solidHeader = TRUE,
              status = "warning", height = 700
            )
          ),
          fluidRow(
            box(
              box(
                actionButton(NS(id, "Start_Opti"), "Start optimization"),
                actionButton(NS(id, "cancel"), "Stop optimization"),
                downloadButton(NS(id, "download"), "Save result of optimization"),
                selectInput(NS(id, "file_type"), "Choose file type:",
                  choices = c("Excel" = "xlsx", "CSV" = "csv")
                ),
                verbatimTextOutput(NS(id, "output")),
                width = 12
              ),
              box(
                br(),
                DT::DTOutput(NS(id, "params")),
                DT::DTOutput(NS(id, "metrices")),
                plotOutput(NS(id, "host_dye_plot")),
                plotOutput(NS(id, "signal_plot")),
                actionButton(
                  inputId = NS(id, "previous_signal_plot"),
                  label = "Previous Signal",
                  class = "add-button df-button"
                ),
                actionButton(
                  inputId = NS(id, "next_signal_plot"),
                  label = "Next Signal",
                  class = "add-button df-button"
                ),
                width = 7, solidHeader = TRUE, status = "warning"
              ),
              width = 12, title = "Optimization", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "VAPRO Optimization",
          fluidRow(
            box(
              textInput(NS(id, "D0_vapro"), "Dye conc. [M]", value = 0),
              box(
                title = "Advanced options",
                collapsible = TRUE, collapsed = TRUE,
                numericInput(NS(id, "nGrid"), "Number of VAPRO grid points", value = 1000),
                selectInput(NS(id, "error_calc_fct_vapro"), "Function to calculate the error:",
                  c(
                    "rel. Error" = "rel. Error",
                    "RMSE" = "RMSE",
                    "SSE" = "SSE",
                    "Huber" = "Huber"
                  )
                ),
                width = 12
              ),
              width = 6,
              title = "Parameter", solidHeader = TRUE,
              status = "warning", height = 700
            ),
            box(
              textInput(NS(id, "kHD_lb_vapro"), HTML("K<sub>a</sub>(HD) value lower boundary [1/M]"), value = 10),
              textInput(NS(id, "kHD_ub_vapro"), HTML("K<sub>a</sub>(HD) value upper boundary [1/M]"), value = 1e08),
              width = 6,
              title = "Boundaries",
              solidHeader = TRUE,
              status = "warning", height = 700
            )
          ),
          fluidRow(
            box(
              box(
                actionButton(NS(id, "Start_Vapro_Opti"), "Start VAPRO optimization"),
                downloadButton(NS(id, "download_vapro"), "Save result of optimization"),
                selectInput(NS(id, "file_type_vapro"), "Choose file type:",
                  choices = c("Excel" = "xlsx", "CSV" = "csv")
                ),
                verbatimTextOutput(NS(id, "output_vapro")),
                width = 12
              ),
              box(
                br(),
                DT::DTOutput(NS(id, "params_vapro")),
                DT::DTOutput(NS(id, "metrices_vapro")),
                plotOutput(NS(id, "host_dye_plot_vapro")),
                plotOutput(NS(id, "signal_plot_vapro")),
                actionButton(
                  inputId = NS(id, "previous_signal_plot_vapro"),
                  label = "Previous Signal",
                  class = "add-button df-button"
                ),
                actionButton(
                  inputId = NS(id, "next_signal_plot_vapro"),
                  label = "Next Signal",
                  class = "add-button df-button"
                ),
                width = 7, solidHeader = TRUE, status = "warning"
              ),
              width = 12, title = "VAPRO Optimization", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "Sensitivity analysis",
          fluidRow(
            box(
              box(
                uiOutput(NS(id, "sensi_source_ui")),
                numericInput(NS(id, "sens_bounds"), "+/- boundary in [%]", value = 15),
                actionButton(NS(id, "Start_Sensi"), "Start sensitivity analysis"),
                actionButton(NS(id, "cancel_sense"), "Cancel"),
                downloadButton(NS(id, "sensi_download"), "Save result of sensitivity analysis"),
                verbatimTextOutput(NS(id, "output_sense")),
                width = 12
              ),
              box(
                br(),
                plotOutput(NS(id, "sensi_plot")),
                width = 10, solidHeader = TRUE, status = "warning"
              ),
              width = 12, title = "Sensitivity analysis", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "Batch-PSO",
          fluidRow(
            box(
              textInput(NS(id, "D0_batch"), "Dye conc. [M]", value = 0),
              box(
                title = "Advanced options",
                collapsible = TRUE, collapsed = TRUE,
                box(
                  numericInput(NS(id, "npop_batch"), "Number of particles", value = 40),
                  numericInput(NS(id, "ngen_batch"), "Number of generations", value = 1000),
                  selectInput(NS(id, "topology_batch"), "Topology of particle swarm",
                    c(
                      "star" = "star",
                      "random arbitrary neighberhood" = "random"
                    ),
                    selected = "random",
                    selectize = FALSE
                  )
                ),
                box(
                  numericInput(NS(id, "threshold_batch"), "Threshold of the error", value = 0.00001),
                  selectInput(NS(id, "error_calc_fct_batch"), "Function to calculate the error:",
                    c(
                      "rel. Error" = "rel. Error",
                      "RMSE" = "RMSE",
                      "SSE" = "SSE",
                      "Huber" = "Huber"
                    )
                  ),
                  numericInput(NS(id, "Seed_batch"), "Seed which should be set", value = NULL)
                ),
                width = 12
              ),
              width = 6,
              title = "Parameter", solidHeader = TRUE,
              status = "warning", height = 700
            ),
            box(
              textInput(NS(id, "kHD_lb_batch"), HTML("K<sub>a</sub>(HD) value lower boundary [1/M]"), value = 10),
              textInput(NS(id, "kHD_ub_batch"), HTML("K<sub>a</sub>(HD) value upper boundary [1/M]"), value = 1e08),
              uiOutput(NS(id, "BOUNDS_I_BATCH")),
              width = 6,
              title = tagList(
                "Boundaries",
                actionButton(NS(id, "helpButton_batch"), "Help",
                  icon = icon("question-circle"),
                  style = "background-color:transparent; border:none;"
                )
              ),
              solidHeader = TRUE,
              status = "warning", height = 700
            )
          ),
          fluidRow(
            box(
              box(
                numericInput(NS(id, "NumRepDataset"),
                  min = 1, max = 200,
                  "How often should each dataset be analysed (using different seeds)",
                  value = 1
                ),
                actionButton(NS(id, "Start_Batch"), "Start batch analysis"),
                actionButton(NS(id, "cancel_Batch"), "Stop optimization"),
                downloadButton(NS(id, "batch_download"), "Save result of batch analysis"),
                verbatimTextOutput(NS(id, "output_Batch")),
                width = 12
              ),

              # TOP: dataset overview
              box(
                title = div(class = "titlebar",
                  div(
                    span("Batch overview — Ka (global)", class = "crumb"),
                    span(textOutput(NS(id, "title_batch"), container = span), class = "muted ms-2")
                  ),
                  div(class = "tools",
                    actionButton(NS(id, "previous_dataset"), "Previous dataset", class = "btn btn-default btn-xs"),
                    actionButton(NS(id, "next_dataset"),     "Next dataset",     class = "btn btn-primary btn-xs")
                  )
                ),
                status = "primary", solidHeader = TRUE, background = "blue", width = 12,
                plotOutput(NS(id, "Ka_main_plot"), height = 320)
              ),

              # MIDDLE: per-dataset details
              box(
                title = div(class = "titlebar",
                  span("Dataset details — Ka & HD/D", class = "crumb"),
                  span(textOutput(NS(id, "dataset_label"), container = span), class = "muted")
                ),
                status = "info", solidHeader = TRUE, width = 12, class = "info-fill",
                fluidRow(
                  column(6, plotOutput(NS(id, "Ka_dataset_plot"), height = 380)),
                  column(6, plotOutput(NS(id, "hd_d_dataset_plot"), height = 380))
                )
              ),

              # BOTTOM: per-signal details
              box(
                title = div(class = "titlebar",
                  div(
                    span("Signal details — Intensities", class = "crumb"),
                    span(textOutput(NS(id, "signal_label"), container = span), class = "muted ms-2")
                  ),
                  div(class = "tools",
                    actionButton(NS(id, "previous_signal_batch"), "Previous signal", class = "btn btn-default btn-xs"),
                    actionButton(NS(id, "next_signal_batch"),     "Next signal",     class = "btn btn-success btn-xs")
                  )
                ),
                status = "success", solidHeader = TRUE, width = 12, class = "success-fill",
                fluidRow(
                  column(6, plotOutput(NS(id, "I_dataset_signal_plot"),      height = 360)),
                  column(6, plotOutput(NS(id, "Signal_dataset_signal_plot"), height = 360))
                )
              ),

              width = 12, title = "Batch analysis", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "VAPRO-Batch",
          fluidRow(
            box(
              textInput(NS(id, "D0_vapro_batch"), "Dye conc. [M]", value = 0),
              box(
                title = "Advanced options",
                collapsible = TRUE, collapsed = TRUE,
                numericInput(NS(id, "nGrid_vapro_batch"), "Number of VAPRO grid points", value = 1000),                selectInput(NS(id, "error_calc_fct_vapro_batch"), "Function to calculate the error:",
                  c(
                    "rel. Error" = "rel. Error",
                    "RMSE" = "RMSE",
                    "SSE" = "SSE",
                    "Huber" = "Huber"
                  )
                ),
                width = 12
              ),
              width = 6,
              title = "Parameter", solidHeader = TRUE,
              status = "warning", height = 700
            ),
            box(
              textInput(NS(id, "kHD_lb_vapro_batch"), HTML("K<sub>a</sub>(HD) value lower boundary [1/M]"), value = 10),
              textInput(NS(id, "kHD_ub_vapro_batch"), HTML("K<sub>a</sub>(HD) value upper boundary [1/M]"), value = 1e08),
              width = 6,
              title = "Boundaries",
              solidHeader = TRUE,
              status = "warning", height = 700
            )
          ),
          fluidRow(
            box(
              box(
                actionButton(NS(id, "Start_Vapro_Batch"), "Start VAPRO batch analysis"),
                downloadButton(NS(id, "vapro_batch_download"), "Save result of VAPRO batch analysis"),
                verbatimTextOutput(NS(id, "output_Vapro_Batch")),
                width = 12
              ),

              # TOP: dataset overview
              box(
                title = div(class = "titlebar",
                  div(
                    span("Batch overview — Ka (global)", class = "crumb"),
                    span(textOutput(NS(id, "title_vapro_batch"), container = span), class = "muted ms-2")
                  ),
                  div(class = "tools",
                    actionButton(NS(id, "previous_dataset_vapro_batch"), "Previous dataset", class = "btn btn-default btn-xs"),
                    actionButton(NS(id, "next_dataset_vapro_batch"),     "Next dataset",     class = "btn btn-primary btn-xs")
                  )
                ),
                status = "primary", solidHeader = TRUE, background = "blue", width = 12,
                plotOutput(NS(id, "Ka_main_plot_vapro_batch"), height = 320)
              ),

              # BOTTOM: per-dataset parameter estimates
              box(
                title = div(class = "titlebar",
                  span("Dataset details — parameter estimates", class = "crumb"),
                  span(textOutput(NS(id, "dataset_label_vapro_batch"), container = span), class = "muted")
                ),
                status = "info", solidHeader = TRUE, width = 12, class = "info-fill",
                DT::DTOutput(NS(id, "summary_table_vapro_batch"))
              ),

              width = 12, title = "VAPRO Batch analysis", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "Uncertainty",
          fluidRow(
            box(
              radioButtons(NS(id, "uncertainty_method"), "Method:",
                choices = c(
                  "Batch (pools repeated PSO runs across datasets - real measurement noise, needs a completed Batch analysis)" = "batch",
                  "Direct bootstrap (perturbs the Optimization fit's residuals - synthetic noise, needs a completed Optimization)" = "direct"
                ),
                selected = "batch"
              ),
              conditionalPanel(
                condition = paste0("input['", id, "-uncertainty_method'] == 'batch'"),
                numericInput(NS(id, "unc_best_pct"),
                  "Keep best % (by error) of each dataset's runs before pooling",
                  min = 1, max = 100, value = 50
                ),
                numericInput(NS(id, "unc_n_boot"), "Number of KDE bootstrap resamples", value = 1000)
              ),
              conditionalPanel(
                condition = paste0("input['", id, "-uncertainty_method'] == 'direct'"),
                numericInput(NS(id, "unc_direct_nBoot"), "Number of bootstrap replicates", value = 500),
                numericInput(NS(id, "unc_direct_seed"), "Seed which should be set", value = NULL)
              ),
              actionButton(NS(id, "Start_Uncertainty"), "Start uncertainty estimation"),
              actionButton(NS(id, "cancel_Uncertainty"), "Stop uncertainty estimation"),
              downloadButton(NS(id, "uncertainty_download"), "Save result of uncertainty estimation"),
              verbatimTextOutput(NS(id, "output_uncertainty")),
              width = 12
            )
          ),
          fluidRow(
            box(
              plotOutput(NS(id, "uncertainty_plot"), height = 400),
              width = 12, solidHeader = TRUE, status = "warning"
            ),
            box(
              DT::DTOutput(NS(id, "uncertainty_table")),
              width = 12, solidHeader = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "VAPRO Uncertainty",
          fluidRow(
            box(
              numericInput(NS(id, "vapro_unc_nBoot"), "Number of bootstrap replicates", value = 500),
              actionButton(NS(id, "Start_Vapro_Uncertainty"), "Start uncertainty estimation"),
              actionButton(NS(id, "cancel_Vapro_Uncertainty"), "Stop uncertainty estimation"),
              downloadButton(NS(id, "vapro_uncertainty_download"), "Save result of uncertainty estimation"),
              verbatimTextOutput(NS(id, "output_vapro_uncertainty")),
              width = 12
            )
          ),
          fluidRow(
            box(
              plotOutput(NS(id, "vapro_uncertainty_plot"), height = 400),
              width = 12, solidHeader = TRUE, status = "warning"
            ),
            box(
              DT::DTOutput(NS(id, "vapro_uncertainty_table")),
              width = 12, solidHeader = TRUE, status = "warning"
            )
          )
        ),
        width = 12
      )
    )
  )
}
