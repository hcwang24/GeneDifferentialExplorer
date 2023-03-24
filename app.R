# Solved package not found message with runing the code below in console.
# library(BiocManager)
# options(repos = BiocManager::repositories())

library(shiny)
library(edgeR)
library(tidyverse)
library(bslib)
library(shiny)
library(htmltools)
library(plotly)
library(leaflet)

light_theme <- bslib::bs_theme(bootswatch = "journal")

# dark_theme <- bslib::bs_theme(bootswatch = "darkly")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = light_theme,

  # Application title
  titlePanel("Gene Differential Expression Explorer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      card(
        card_header(
          class = "bg-dark",
          "Upload metadata.csv File (containing list of files with grouping information",
        ),
        card_body(
          # Sidebar panel for file upload
          fileInput("file1",
            label = NULL,
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",
              ".txt"
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
          actionButton("upload", "Upload metadata file"),
        ),
      ),
      fluidRow(
        # Sidebar panel for file upload
        fileInput("files2",
          "Upload featurecount files",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".txt"
          )
        ),
        actionButton("upload_fc", "Upload featureCount files"),
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # h3("Exploratory Data Analysis"),
      layout_column_wrap(
        width = 1,
        height = 340,
        fill = TRUE,
        heights_equal = "all",
        
        # Metadata viewer
        card(
          full_screen = TRUE,
          card_header(
            "Metadata File"
          ),
          card_body_fill(
            tableOutput("metadata_contents"),
          )
        ),
        
        # DGEList viewer
        card(
          full_screen = TRUE,
          card_header(
            "Metadata File"
          ),
          card_body_fill(
            tableOutput("dge_contents"),
          )
        ),
        
      )
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

  # Loading featurecounts files
  data <- eventReactive(input$upload_fc, {
    req(input$files2)
    readDGE(
      input$files2$datapath
    )
  })
  output$dge_contents <- renderTable({
    data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
