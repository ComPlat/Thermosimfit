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
      "Shiny.addCustomMessageHandler('DBAclearField', function(message) {
              $('#DBA-output').empty();
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldSense', function(message) {
              var result = message.message;
              $('#DBA-output_sense').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAclearFieldSense', function(message) {
              $('#DBA-output_sense').empty();
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateFieldBatch', function(message) {
              var result = message.message;
              $('#DBA-output_Batch').html(result);
            });"
    ),
    tags$script(
      "Shiny.addCustomMessageHandler('DBAclearFieldBatch', function(message) {
              $('#DBA-output_Batch').empty();
            });"
    ),
    fluidRow(
      box(
        textInput(NS(id, "D0"), "Dye conc. [M]", value = 0),
        box(
          title = "Advanced options",
          collapsible = TRUE, collapsed = TRUE,
          box(
            numericInput(NS(id, "npop"), "Number of particles", value = 40),
            numericInput(NS(id, "ngen"), "Number of generations", value = 1000)
          ),
          box(
            selectInput(NS(id, "topology"), "Topology of particle swarm",
              c(
                "star" = "star",
                "random arbitrary neighberhood" = "random"
              ),
              selected = "random",
              selectize = FALSE
            ),
            numericInput(NS(id, "threshold"), "Threshold of the error", value = 0.00001),
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
          textInput(NS(id, "kHD_lb"), HTML("K<sub>a</sub>(HD) value lower boundary [1/M]"), value = 10),
          textInput(NS(id, "kHD_ub"), HTML("K<sub>a</sub>(HD) value upper boundary [1/M]"), value = 1e08)
        ),
        box(
          textInput(NS(id, "I0_lb"), "I(0) value lower boundary", value = 0),
          textInput(NS(id, "I0_ub"), "I(0) value upper boundary", value = 1e08)
        ),
        box(
          textInput(NS(id, "IHD_lb"),
            label = tagList(
              "I(HD) value lower boundary [1/M]",
              actionButton(NS(id, "AdviceUBIHD"), "Help",
                icon = icon("question-circle"),
                style = "background-color:transparent; border:none;"
              )
            ), value = 0
          ),
          textInput(NS(id, "IHD_ub"), "I(HD) value upper boundary [1/M]", value = 1e08)
        ),
        box(
          textInput(NS(id, "ID_lb"), "I(D) value lower boundary [1/M]", value = 0),
          textInput(NS(id, "ID_ub"), "I(D) value upper boundary [1/M]", value = 1e08)
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
                plotlyOutput(NS(id, "plot")),
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
              box(
                id = "DBA-output_Batch",
                plotlyOutput(NS(id, "batch_data_plot"), height = 1200),
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
