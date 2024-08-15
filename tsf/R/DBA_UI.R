dbaUI <- function(id) {
  tabItem(
    tabName = "DBA",
    tags$script(
      "Shiny.addCustomMessageHandler('DBAupdateField', function(message) {
              var result = message.message;
              $('#DBA-DBA_output').html(result);
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
