gdaUI <- function(id) {
  tabItem(
    tabName = "GDA",
    tags$script(
      "Shiny.addCustomMessageHandler('GDAupdateField', function(message) {
              var result = message.message;
              $('#GDA-GDA_output').html(result);
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
        status = "warning", height = 625
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
        status = "warning", height = 625
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
