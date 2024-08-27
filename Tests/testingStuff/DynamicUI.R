library(shiny)
library(ggplot2)
library(dplyr)
# Load data
data("iris")

# Add row id
iris2 <- iris %>% mutate(ID = 1:n())

# ui
ui <- fluidPage(
  sidebarPanel(
    selectInput(inputId = "sel", label = "Select one or more parameters",
      choices = names(iris2), multiple = TRUE)
  ),
  mainPanel(
    uiOutput("plots")
  )
)

# server
server <- function(input, output, session){

  # Dynamically generate the plots based on the selected parameters
  observe({
    req(input$sel)
    lapply(input$sel, function(par){
      p <- ggplot(iris2, aes_string(x = "ID", y = par)) +
        geom_boxplot(aes(fill = Species, group=Species, color=Species)) +
        ggtitle(paste("Plot: ", par)) 
      output[[paste("plot", par, sep = "_")]] <- renderPlot({
        p
      },
        width = 380,
        height = 350)
    })
  })

  # Create plot tag list
  output$plots <- renderUI({
    req(input$sel)
    plot_output_list <- lapply(input$sel, function(par) {
      plotname <- paste("plot", par, sep = "_")
      plotOutput(plotname, height = '250px', inline=TRUE)
    })

    do.call(tagList, plot_output_list)

  })

}

app <- shinyApp(ui, server)

library(shinytest2)
app <- AppDriver$new(app)
app$get_values() |> str()
