#' Offers a GUI for the optimization of
#' algebraic systems describing thermodynamic binding systems
#'
#' @export
#' @param port is a number defining the port to use.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable, runExample))
#' @rawNamespace import(shinyWidgets, except=c(alert))
#' @import shinydashboard
#' @import shinyjs
#' @import openxlsx
#' @import future
#' @import promises
#' @import DT
#' @examples
#' \donttest{
#' tsf::runApp()
#' }
runApp <- function(port) {
  ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Thermosimfit"),
    dashboardSidebar(
      useShinyjs(),
      sidebarMenu(
        menuItem("Data import", tabName = "data", icon = icon("table")),
        menuItem("DBA (const. host) model", tabName = "HG", icon = icon("table")),
        menuItem("DBA (const. dye) model", tabName = "DBA", icon = icon("table")),
        menuItem("GDA model", tabName = "GDA", icon = icon("table")),
        menuItem("IDA model", tabName = "IDA", icon = icon("table")),
        menuItem("Info",
          tabName = "info", icon = icon("info-circle"),
          p(
            "For more info, visit:",
            br(),
            tags$a(href = "https://suprabank.org/glossary", "Suprabank Glossary")
          )
        )
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(HTML("
          body {
            font-size: 14px;
          }
          .sidebar-menu a {
            font-size: 14px;
          }
          .box-title {
            font-size: 14px;
          }
          .content-wrapper, .right-side {
            font-size: 14px;
          }
        "))
      ),
      tabItems(

        # data tab
        tabItem(
          tabName = "data",
          box(
            fileInput("upload", "Upload a file (csv) \n
              which contains two columns: \n
               1. the component which is increased [M] \n
               2. the corresponding signal [unitless]
              "),
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
        dbaUI("DBA"),
        idaUI("IDA"),
        gdaUI("GDA")
      )
    )
  )

  options(shiny.host = "0.0.0.0", shiny.port = 3838)
  plan(multisession)
  shinyApp(ui, server, options = list(port = port))
}
