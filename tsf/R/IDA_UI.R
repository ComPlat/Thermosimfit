idaUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "IDA",
    tags$script(
      "Shiny.addCustomMessageHandler('IDAupdateField', function(message) {
      var result = message.message;
      $('#IDA-output').html(result);
      });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAclearField', function(message) {
      $('#IDA-output').empty();
      });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAupdateFieldSense', function(message) {
      var result = message.message;
      $('#IDA-output_sense').html(result);
      });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAclearFieldSense', function(message) {
      $('#IDA-output_sense').empty();
      });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAupdateFieldBatch', function(message) {
      var result = message.message;
      $('#IDA-output_Batch').html(result);
      });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('IDAclearFieldBatch', function(message) {
      $('#IDA-output_Batch').empty();
      });"
    ),
    fluidRow(
      box(
        textInput(NS(id, "H0"), "Host conc. [M]", value = 0),
        textInput(NS(id, "D0"), "Dye conc. [M]", value = "0"),
        textInput(NS(id, "kHD"), HTML("K<sub>a</sub>(HD) [1/M]"), value = "0"),
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
            numericInput(NS(id, "threshold"),
              "Threshold of the error",
              value = 0.00001
            ),
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
        status = "warning", height = 660
      ),
      box(
        box(
          textInput(NS(id, "kHG_lb"), HTML("K<sub>a</sub>(HG) value lower boundary [1/M]"), value = 10),
          textInput(NS(id, "kHG_ub"), HTML("K<sub>a</sub>(HG) value upper boundary [1/M]"), value = 1e08)
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
        status = "warning", height = 660
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
                width = 12, solidHeader = TRUE, status = "warning"
              ),
              width = 14, title = "Optimization", solidHeader = TRUE,
              collapsible = TRUE, status = "warning"
            )
          )
        ),
        tabPanel(
          "Sensitivity analysis",
          fluidRow(
            box(
              box(
                numericInput(NS(id, "sens_bounds"), "+/- boundary in [%]", value = 15),
                actionButton(NS(id, "Start_Sensi"), "Start Sensitivity analysis"),
                actionButton(NS(id, "cancel_sense"), "Cancel"),
                downloadButton(NS(id, "sensi_download"), "Save result of sensitivity analysis"),
                verbatimTextOutput(NS(id, "output_sense")),
                width = 12
              ),
              box(
                br(),
                DT::DTOutput(NS(id, "sensi_table")),
                width = 10, solidHeader = TRUE, status = "warning"
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
                numericInput(NS(id, "NumRepDataset"),
                  min = 1, max = 5,
                  "How often should each dataset be analysed (using different seeds)",
                  value = 1
                ),
                numericInput(NS(id, "NumCores"),
                  min = 1, max = 20,
                  "How many cores should be used for the batch analysis?",
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
        width = 12
      )
    )
  )
}
