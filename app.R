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
library(RColorBrewer)
library(RUVSeq)

light_theme <- bslib::bs_theme(bootswatch = "journal")

# dark_theme <- bslib::bs_theme(bootswatch = "darkly")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = light_theme,
  titlePanel("Gene Differential Expression Explorer"),
  navbarPage(
    "Navigation",
    tabPanel(
      "1. File uploads",
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
                tableOutput("metadata_contents")
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
    ),
    tabPanel(
      "2. Exploratory data analysis",
      layout_column_wrap(
        width = 1 / 2,
        height = 340,
        fill = TRUE,
        heights_equal = "all",
        card(
          full_screen = TRUE,
          card_header(textOutput("raw_text")),
          card_body_fill(div(span(style = "font-size: 12px;", verbatimTextOutput("raw_summary"))))
        ),
        card(
          full_screen = TRUE,
          card_header(textOutput("filter_text")),
          card_body_fill(div(span(style = "font-size: 12px;", verbatimTextOutput("filtered_summary"))))
        ),
      ),
      layout_column_wrap(
        width = 1 / 3,
        height = 340,
        fill = TRUE,
        heights_equal = "all",
        card(
          full_screen = TRUE,
          card_header("Raw gene expression"),
          card_body_fill(plotOutput("raw_boxplot"))
        ),
        card(
          full_screen = TRUE,
          card_header("Raw relative level of expression"),
          card_body_fill(plotOutput("raw_RLE"))
        ),
        card(
          full_screen = TRUE,
          card_header("Raw PCA"),
          card_body_fill(plotOutput("raw_pca"))
        ),
      ),
    ),
    
    
    tabPanel(
      "3. Quantile normalization",
      "Content"
    ),
    tabPanel(
      "4. TMM Normalization",
      "Content"
    ),
    tabPanel(
      "5. Data visualization",
      "Content"
    ),
    tabPanel(
      "6. EdgeR Pairwise Analysis",
      "Content"
    ),
    tabPanel(
      "7. Final outlook",
      "Content"
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Data and viz used for tab 1
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

  # Data and viz used for tab 2
  raw_dim <- reactive(dim(data())[[1]])
  output$raw_text <- renderText(
    paste("Before filtering, there are", raw_dim(), "genes sequenced.")
  )
  output$raw_summary <- renderPrint(
    summary(data()$counts)
  )

  filtered_data <- reactive({
    keep <- rowSums(cpm(data()) > 5) >= 3
    data()[keep, ]
  })
  remain_dim <- reactive(dim(filtered_data())[[1]])
  removed_dim <- reactive(dim(data())[[1]] - dim(filtered_data())[[1]])
  output$filter_text <- renderText(
    paste(
      "Filtering! Removing",
      removed_dim(),
      "genes and",
      remain_dim(),
      "genes remaining."
    )
  )
  output$filtered_summary <- renderPrint(
    summary(filtered_data()$counts)
  )
  
  g <- reactive(as.factor(filtered_data()$samples$group))
  colors <- brewer.pal(7, "Set2")

  output$raw_boxplot <- renderPlot(
    boxplot(filtered_data()$counts, outline=FALSE, main="Raw Boxplot", col=colors[g()], las=2)
  )

  output$raw_RLE <- renderPlot(
    plotRLE(filtered_data()$counts, outline=FALSE, main="Raw RLE", ylab="RLE", col=colors[g()], las=2)
  )

  output$raw_pca <- renderPlot(
    plotPCA(filtered_data()$counts, main="Raw PCA", col=colors[g()], cex=1.0)
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
