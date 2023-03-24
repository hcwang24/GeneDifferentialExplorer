# Solved package not found message with runing the code below in console.
# library(BiocManager)
# options(repos = BiocManager::repositories())

library(shiny)
library(edgeR)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Gene Differential Expression Explorer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # Sidebar panel for file upload
      fileInput("file1",
        "Upload metadata.csv File (containing list of files with grouping information",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        selected = ","
      ),
      
      actionButton("upload", "Upload"),
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h3("Exploratory Data Analysis"),
      tableOutput("metadata_contents"),
      textOutput("grouping")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Loading metadata table
  metadata_df <- eventReactive(input$upload, {
    req(input$file1)
    read_delim(
      input$file1$datapath,
      delim = input$sep,
      col_names = TRUE,
      show_col_types = FALSE
    )
  })
  
  output$metadata_contents <- renderTable({
    metadata_df()
  })
  
  output$grouping <- renderText({
    print(levels(metadata_df()$group))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
