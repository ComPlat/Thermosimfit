hgUI <- function(id) {
  tabItem(
    tabName = "HG",
    tags$script(
      "Shiny.addCustomMessageHandler('HGupdateField', function(message) {
              var result = message.message;
              $('#HG-HG_output').html(result);
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
          title = "Advanced options",
          collapsible = TRUE, collapsed = TRUE,
          box(
            numericInput(NS(id, "HG_npop"), "Number of particles", value = 40),
            numericInput(NS(id, "HG_ngen"), "Number of generations", value = 1000)
          ),
          box(
            selectInput(NS(id, "HG_topology"), "Topology of particle swarm",
              c(
                "star" = "star",
                "random arbitrary neighberhood" = "random"
              ),
              selected = "random",
              selectize = FALSE
            ),
            numericInput(NS(id, "HG_threshold"), "Threshold of the error", value = 0.00001),
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
          textInput(NS(id, "HG_kHD_lb"), HTML("K<sub>a</sub>(HD) value lower boundary [1/M]"), value = 10),
          textInput(NS(id, "HG_kHD_ub"),
            HTML("K<sub>a</sub>(HD) value upper boundary [1/M]"),
            value = 1e08
          )
        ),
        box(
          textInput(NS(id, "HG_I0_lb"), "I(0) value lower boundary", value = 0),
          textInput(NS(id, "HG_I0_ub"), "I(0) value upper boundary", value = 1e08)
        ),
        box(
          textInput(NS(id, "HG_IHD_lb"),
            label = tagList(
              "I(HD) value lower boundary [1/M]",
              actionButton(NS(id, "AdviceUBIHD"), "Help",
                icon = icon("question-circle"),
                style = "background-color:transparent; border:none;"
              )
            ), value = 0
          ),
          textInput(NS(id, "HG_IHD_ub"), "I(HD) value upper boundary [1/M]", value = 1e08)
        ),
        box(
          textInput(NS(id, "HG_ID_lb"), "I(D) value lower boundary [1/M]", value = 0),
          textInput(NS(id, "HG_ID_ub"), "I(D) value upper boundary [1/M]", value = 1e08)
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
                actionButton(NS(id, "HG_Start_Opti"), "Start Optimization"),
                actionButton(NS(id, "HG_cancel"), "Stop Optimization"),
                actionButton(NS(id, "HG_status"), "Get Status"),
                downloadButton(NS(id, "HG_download"), "Save result of optimization"),
                selectInput(NS(id, "file_type"), "Choose file type:",
                  choices = c("Excel" = "xlsx", "CSV" = "csv")
                ),
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
