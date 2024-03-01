#' Optimize algebraic systems which describe thermodynamic binding systems
#'
#' @export
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
#' @import DT
runApp <- function() {
  ui <- dashboardPage(
    
    dashboardHeader(title = "Thermosimfit"),
    dashboardSidebar(
      useShinyjs(),
      sidebarMenu(
        menuItem("HG model", tabName = "HG", icon = icon("table")),
        menuItem("GDA model", tabName = "GDA", icon = icon("table")),
        menuItem("IDA model", tabName = "IDA", icon = icon("table"))
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        tabItem(
           tabName = "HG",
           box(
             fileInput("HG_upload", "Upload a file"),
             numericInput("HG_H0", "Host conc.", value = 0),
             numericInput("HG_npop", "Number of particles", value = 40),
             numericInput("HG_ngen", "Number of generations", value = 200),
             textInput("HG_topology", "Topology of particle swarm", value = "star"),
             numericInput("HG_threshold", "Threshold of the error", value = 0.00001),
             DT::DTOutput("HG_dat"),
             width = 12
           ),
           box(
             box(
                numericInput("HG_kHD_lb", "kHD value lower boundary", value = 0),
                numericInput("HG_kHD_ub", "kHD value upper boundary", value = 1e09)
             ),
             box(
                numericInput("HG_I0_lb", "I0 value lower boundary", value = 0),
                numericInput("HG_I0_ub", "I0 value upper boundary", value = 1)
             ),
             box(
                numericInput("HG_IHD_lb", "IHD value lower boundary", value = 0),
                numericInput("HG_IHD_ub", "IHD value upper boundary", value = 1e06)
             ),
             box(
                numericInput("HG_ID_lb", "ID value lower boundary", value = 0),
                numericInput("HG_ID_ub", "ID value upper boundary", value = 1e06)
             ),
             actionButton("HG_Start_Opti","Start Optimization"),
             verbatimTextOutput("consoleOutput"),
             width = 12
           ),
           box(
             actionButton("HG_Start_Sensi","Start Sensitivity analysis"),
             width = 12
           )
        ),
      
        tabItem(
          tabName = "GDA",
          box(
            fileInput("GDA_upload", "Upload a file"),
            numericInput("GDA_H0", "Host conc.", value = 0),
            numericInput("GDA_Dye", "Dye conc.", value = 0),
            numericInput("GDA_kHD", "kHD", value = 0),
            numericInput("GDA_npop", "Number of particles", value = 40),
            numericInput("GDA_ngen", "Number of generations", value = 200),
            textInput("GDA_topology", "Topology of particle swarm", value = "star"),
            numericInput("GDA_threshold", "Thershold of the error", value = 0.00001),
            actionButton("GDA_Start_Opti","Start Optimization"),
            width = 12
          ),
          
          box(
            actionButton("GDA_Start_Sensi","Start Sensitivity analysis"),
            width = 12
          )
        ),
      
        tabItem(
          tabName = "IDA",
          box(
            fileInput("IDA_upload", "Upload a file"),
            numericInput("IDA_H0", "Host conc.", value = 0),
            numericInput("IDA_Guest", "Guest conc.", value = 0),
            numericInput("IDA_kHD", "kHD", value = 0),
            numericInput("IDA_npop", "Number of particles", value = 40),
            numericInput("IDA_ngen", "Number of generations", value = 200),
            textInput("IDA_topology", "Topology of particle swarm", value = "star"),
            numericInput("IDA_threshold", "Thershold of the error", value = 0.00001),
            actionButton("IDA_Start_Opti","Start Optimization"),
            width = 12
          ),
          
          box(
            actionButton("IDA_Start_Sensi","Start Sensitivity analysis"),
            width = 12
          )
        )
        
      )
      
    )
  )
  shinyApp(ui, server)
}