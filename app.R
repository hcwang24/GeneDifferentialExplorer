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

      # metadata file upload
      card(
        card_header(
          class = "bg-dark",
          "Upload metadata.csv File (containing list of files with grouping information",
        ),
        card_body(
          # Sidebar panel for file upload
          fileInput("metadatafile",
            label = NULL,
            multiple = FALSE,
            accept = c(
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
          actionButton("upload_meta", "Upload metadata file"),
        ),
      ),

      # featureCount files upload
      card(
        card_header(
          class = "bg-dark",
          "Upload featurecount files",
        ),
        card_body(
          fileInput("featurecountfiles",
            label = NULL,
            multiple = TRUE,
            accept = c(
              ".csv",
              ".txt"
            )
          ),
          actionButton("upload_featureCounts", "Upload featureCount files"),
        ),
      ),
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
            # textOutput("metadata_levels")
          )
        ),
      ),
      layout_column_wrap(
        width = 1 / 2,
        height = 340,
        fill = TRUE,
        heights_equal = "all",

        # DGEList viewer
        card(
          full_screen = TRUE,
          card_header(
            "DGE Samples"
          ),
          card_body_fill(
            tableOutput("dge_samples"),
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            "DGE Counts"
          ),
          card_body_fill(
            DT::dataTableOutput("dge_counts"),
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Loading metadata table (data will be stored in metadata_df)
  metadata_df <- eventReactive(input$upload_meta, {
    req(input$metadatafile)
    read_delim(
      input$metadatafile$datapath,
      delim = input$sep,
      col_names = TRUE,
      show_col_types = FALSE
    )
  })
  output$metadata_contents <- renderTable({
    metadata_df()
  })

  # Loading featureCounts files
  files <- reactiveValues()
  data <- eventReactive(input$upload_featureCounts, {
    req(input$featurecountfiles)
    files$names <- input$featurecountfiles$name
    files$path <- input$featurecountfiles$datapath
    d <- readDGE(
      files$path,
      columns = c(1, 3),
      labels = files$names
    )
    d$samples <- d$samples[, -2] # removing the default group from DGE
    d$samples$files <- files$names
    if (!is.null(input$upload_meta)) {
      d$samples <- inner_join(d$samples, metadata_df(), by = join_by(files == files))
    }
    d
  })
  output$dge_samples <- renderTable(
    data()$samples,
    rownames = TRUE,
    hover = TRUE
  )
  output$dge_counts <- DT::renderDataTable(
    data()$counts,
    options = list(scrollX = TRUE)
  )
}

# Run the application
shinyApp(ui = ui, server = server)
