library(shiny)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(rootSolve)
library(ggplot2)
library(patchwork)
library(R6)
library(sensitivity)
library(openxlsx)
library(future)
library(promises)
library(tsf)

options(shiny.host = "0.0.0.0")
#options(shiny.port = 3838)

#source("/home/ui.R")
#source("/home/server.R")
#shinyApp(ui, server)

tsf::runApp(3838)