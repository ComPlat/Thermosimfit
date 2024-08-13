library(shiny)
library(shinydashboard)



ui <- dashboardPage(
  dashboardHeader(title = "Submenu Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main Tab",
        tabName = "main", icon = icon("dashboard"),
        menuSubItem("Subtab 1", tabName = "subtab1"),
        menuSubItem("Subtab 2", tabName = "subtab2")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "subtab1",
        h2("Content of Subtab 1")
      ),
      tabItem(
        tabName = "subtab2",
        h2("Content of Subtab 2")
      )
    )
  )
)
server <- function(input, output) { }

shinyApp(ui, server)
