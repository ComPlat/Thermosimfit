#' Optimize algebraic systems which describe thermodynamic binding systems
#'
#' @export
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
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
            h5(strong("Modify a variable")),
            numericInput("HG_H0", "Host conc.", value = 0),
            numericInput("HG_npop", "Number of particles", value = 40),
            numericInput("HG_ngen", "Number of generations", value = 200),
            textInput("HG_topology", "Topology of particle swarm", value = "star"),
            numericInput("HG_thershold", "Thershold of the error", value = 0.00001),
            actionButton("HG_Start","Start Optimization"),
            #verbatimTextOutput("mod_error"),
            fileInput("upload", "Upload a file"),
            width = 12
          )
        ),
      
        tabItem(
          tabName = "GDA",
          box(
            h5(strong("Modify a variable")),
            numericInput("GDA_H0", "Host conc.", value = 0),
            numericInput("GDA_Dye", "Dye conc.", value = 0),
            numericInput("GDA_kHD", "kHD", value = 0),
            numericInput("GDA_npop", "Number of particles", value = 40),
            numericInput("GDA_ngen", "Number of generations", value = 200),
            textInput("GDA_topology", "Topology of particle swarm", value = "star"),
            numericInput("GDA_thershold", "Thershold of the error", value = 0.00001),
            actionButton("GDA_Start","Start Optimization"),
            #verbatimTextOutput("mod_error"),
            fileInput("upload", "Upload a file"),
            width = 12
          )
        ),
      
        tabItem(
          tabName = "IDA",
          box(
            h5(strong("Modify a variable")),
            numericInput("IDA_H0", "Host conc.", value = 0),
            numericInput("IDA_Guest", "Guest conc.", value = 0),
            numericInput("IDA_kHD", "kHD", value = 0),
            numericInput("IDA_npop", "Number of particles", value = 40),
            numericInput("IDA_ngen", "Number of generations", value = 200),
            textInput("IDA_topology", "Topology of particle swarm", value = "star"),
            numericInput("IDA_thershold", "Thershold of the error", value = 0.00001),
            actionButton("IDA_Start","Start Optimization"),
            #verbatimTextOutput("mod_error"),
            fileInput("upload", "Upload a file"),
            width = 12
          )
        )
        
      )
      
    )
  )
  shinyApp(ui, server)
}