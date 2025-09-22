uiInterface <- function() {
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
          box(
              DT::DTOutput("df"),
              width = 10
          ),
          width = 12
          )
        ),
        
        hgUI("HG"),
        idaUI("IDA"),
        gdaUI("GDA")
        
        
      )
      
    )
  )
    return(ui)
}

