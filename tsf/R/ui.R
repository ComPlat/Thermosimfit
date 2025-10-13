uiInterface <- function() {
    ui <- dashboardPage(

    tags$head(tags$style(HTML("
      /* Flex title with right-aligned tools */
      .box .titlebar { display:flex; align-items:center; justify-content:space-between; gap:.75rem; }
      .box .titlebar .tools .btn { padding:2px 10px; }

      /* Pale body fills for whole-box emphasis */
      .box.info-fill   .box-body { background:#eef7fb; }  /* light teal */
      .box.success-fill .box-body { background:#e8f5e9; } /* light green */

      /* Compact plot spacing */
      .box .box-body .shiny-plot-output { margin-bottom: 0.75rem; }
      .muted { color:#6c757d; font-size:.95rem; }
      .crumb { font-weight:600; }
      "))),

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

