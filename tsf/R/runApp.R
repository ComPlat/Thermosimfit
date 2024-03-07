#' Optimize algebraic systems which describe thermodynamic binding systems
#'
#' @export
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
#' @import openxlsx
#' @import DT
runApp <- function() {
  ui <- dashboardPage(
    
    dashboardHeader(title = "Thermosimfit"),
    dashboardSidebar(
      useShinyjs(),
      tags$script(
        "Shiny.addCustomMessageHandler('updateField', function(message) {
          var result = message.message;
          var c = message.arg;
          if(c == 1) {
            $('#HG_output').append(result + '\\n');
          } else if(c == 2) {
            $('#GDA_output').append(result + '\\n');
          } else if(c == 3) {
            $('#IDA_output').append(result + '\\n');
          }
        });"
      ),
      tags$script(
        "Shiny.addCustomMessageHandler('clearField', function(message) {
            var c = message.arg;
            if(c == 1) {
              $('#HG_output').empty();
            } else if(c == 2) {
              $('#GDA_output').empty();
            } else if(c == 3) {
              $('#IDA_output').empty();
          }
        });"
      ),
      sidebarMenu(
        menuItem("Data import", tabName = "data", icon = icon("table")),
        menuItem("HG model", tabName = "HG", icon = icon("table")),
        menuItem("GDA model", tabName = "GDA", icon = icon("table")),
        menuItem("IDA model", tabName = "IDA", icon = icon("table"))
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        # data tab
        # ========================================================================
        tabItem(
          tabName = "data",
          box(
            fileInput("upload", "Upload a file"),
            textInput("op", "Operations", value = "var / 1000"),
            textInput("new_col", "Name of new variable", value = "var"),
            actionButton("mod", "Modify"),
            verbatimTextOutput("mod_error"),
            box(
              DT::DTOutput("df"),  
              width = 10
            ),
            width = 12
          )
        ),
        
        # HG
        # ========================================================================
        tabItem(
           tabName = "HG",
           box(
             textInput("HG_H0", "Host conc.", value = 0),
             numericInput("HG_npop", "Number of particles", value = 40),
             numericInput("HG_ngen", "Number of generations", value = 200),
             selectInput("HG_topology", "Topology of particle swarm",
                         c("star" = "star",
                           "random" = "random"), selectize = FALSE),
             numericInput("HG_threshold", "Threshold of the error", value = 0.00001),
             width = 12
           ),
           box(
             box(
               textInput("HG_kHD_lb", "kHD value lower boundary", value = 0),
               textInput("HG_kHD_ub", "kHD value upper boundary", value = 1e09)
             ),
             box(
               textInput("HG_I0_lb", "I0 value lower boundary", value = 0),
               textInput("HG_I0_ub", "I0 value upper boundary", value = 1)
             ),
             box(
               textInput("HG_IHD_lb", "IHD value lower boundary", value = 0),
               textInput("HG_IHD_ub", "IHD value upper boundary", value = 1e06)
             ),
             box(
               textInput("HG_ID_lb", "ID value lower boundary", value = 0),
               textInput("HG_ID_ub", "ID value upper boundary", value = 1e06)
             ),
             actionButton("HG_Start_Opti","Start Optimization"),
             downloadButton("HG_download","Save result of optimization"),
             verbatimTextOutput("HG_output"),
             DT::DTOutput("HG_params"),
             DT::DTOutput("HG_metrices"),
             box(
               plotOutput("HG_plot"),
               width = 9
             ),
             width = 12
           ),
           box(
             numericInput("HG_sens_bounds", "+/- boundary in [%]", value = 15),
             box(
               actionButton("HG_Start_Sensi","Start Sensitivity analysis"),
               downloadButton("HG_sensi_download", "Save result of sensitivity analysis")
             ),
             div(id = "HG_sens_runs",
                 style = "display: none;
                          font-weight: bold;
                          font-size: 16px;
                          margin-top: 10px;",
                 "Sensitivity analysis runs"),
             plotOutput("HG_sensi"),
             width = 12
           )
        ),
        
        
        tabItem(
          tabName = "GDA",
          box(
            textInput("GDA_H0", "Host conc.", value = 0),
            textInput("GDA_G0", "Guest conc.", value = 0),
            textInput("GDA_kHD", "kHD", value = 0),
            numericInput("GDA_npop", "Number of particles", value = 40),
            numericInput("GDA_ngen", "Number of generations", value = 200),
            selectInput("GDA_topology", "Topology of particle swarm",
                        c("star" = "star",
                          "random" = "random"), selectize = FALSE),
            numericInput("GDA_threshold", "Threshold of the error", value = 0.00001),
            width = 12
          ),
          box(
            box(
              textInput("GDA_kHD_lb", "kHD value lower boundary", value = 0),
              textInput("GDA_kHD_ub", "kHD value upper boundary", value = 1e09)
            ),
            box(
              textInput("GDA_I0_lb", "I0 value lower boundary", value = 0),
              textInput("GDA_I0_ub", "I0 value upper boundary", value = 1)
            ),
            box(
              textInput("GDA_IHD_lb", "IHD value lower boundary", value = 0),
              textInput("GDA_IHD_ub", "IHD value upper boundary", value = 1e06)
            ),
            box(
              textInput("GDA_ID_lb", "ID value lower boundary", value = 0),
              textInput("GDA_ID_ub", "ID value upper boundary", value = 1e06)
            ),
            actionButton("GDA_Start_Opti","Start Optimization"),
            downloadButton("GDA_download","Save result of optimization"),
            verbatimTextOutput("GDA_output"),
            DT::DTOutput("GDA_params"),
            DT::DTOutput("GDA_metrices"),
            box(
              plotOutput("GDA_plot"),
              width = 9
            ),
            width = 12
          ),
          box(
            numericInput("GDA_sens_bounds", "+/- boundary in [%]", value = 15),
            box(
              actionButton("GDA_Start_Sensi","Start Sensitivity analysis"),
              downloadButton("GDA_sensi_download", "Save result of sensitivity analysis")
            ),
            div(id = "GDA_sens_runs",
                style = "display: none;
                          font-weight: bold;
                          font-size: 16px;
                          margin-top: 10px;",
                "Sensitivity analysis runs"),
            plotOutput("GDA_sensi"),
            width = 12
          )
        ),
        
      
        tabItem(
          tabName = "IDA",
          box(
            textInput("IDA_H0", "Host conc.", value = "0"),
            textInput("IDA_D0", "Dye conc.", value = "0"),
            textInput("IDA_kHD", "kHD", value = "0"),
            numericInput("IDA_npop", "Number of particles", value = 40),
            numericInput("IDA_ngen", "Number of generations", value = 200),
            selectInput("IDA_topology", "Topology of particle swarm",
                        c("star" = "star",
                          "random" = "random"), selectize = FALSE),
            numericInput("IDA_threshold", "Threshold of the error", value = 0.00001),
            width = 12
          ),
          box(
            box(
              textInput("IDA_kHD_lb", "kHD value lower boundary", value = 0),
              textInput("IDA_kHD_ub", "kHD value upper boundary", value = 1e09)
            ),
            box(
              textInput("IDA_I0_lb", "I0 value lower boundary", value = 0),
              textInput("IDA_I0_ub", "I0 value upper boundary", value = 1)
            ),
            box(
              textInput("IDA_IHD_lb", "IHD value lower boundary", value = 0),
              textInput("IDA_IHD_ub", "IHD value upper boundary", value = 1e06)
            ),
            box(
              textInput("IDA_ID_lb", "ID value lower boundary", value = 0),
              textInput("IDA_ID_ub", "ID value upper boundary", value = 1e06)
            ),
            actionButton("IDA_Start_Opti","Start Optimization"),
            downloadButton("IDA_download","Save result of optimization"),
            verbatimTextOutput("IDA_output"),
            DT::DTOutput("IDA_params"),
            DT::DTOutput("IDA_metrices"),
            box(
              plotOutput("IDA_plot"),
              width = 9
            ),
            width = 12
          ),
          box(
            numericInput("IDA_sens_bounds", "+/- boundary in [%]", value = 15),
            box(
              actionButton("IDA_Start_Sensi","Start Sensitivity analysis"),
              downloadButton("IDA_sensi_download", "Save result of sensitivity analysis")
            ),
            div(id = "IDA_sens_runs",
                style = "display: none;
                          font-weight: bold;
                          font-size: 16px;
                          margin-top: 10px;",
                "Sensitivity analysis runs"),
            plotOutput("IDA_sensi"),
            width = 12
          )
        )
        
        
        
      )
      
    )
  )
  shinyApp(ui, server)
}