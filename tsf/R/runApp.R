#' Optimize algebraic systems which describe thermodynamic binding systems
#'
#' @export
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable, runExample))
#' @rawNamespace import(shinyWidgets, except=c(alert))
#' @import shinydashboard
#' @import shinyjs
#' @import openxlsx
#' @import future
#' @import promises
#' @import DT
runApp <- function() {
  ui <- dashboardPage(

    skin = "blue",
    
    dashboardHeader(title = "Thermosimfit"),
    dashboardSidebar(
      useShinyjs(),

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
        
        hgUI("HG"),
        idaUI("IDA")
        
        
      )
      
    )
  )

  plan(multisession)
  shinyApp(ui, server)
}