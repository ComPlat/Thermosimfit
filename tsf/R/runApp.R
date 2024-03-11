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
  plan(multisession)
  shinyApp(uiInterface(), server, options = list(port = port))
}