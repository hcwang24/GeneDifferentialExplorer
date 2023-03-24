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
  titlePanel("Gene Differential Expression Explorer"),
  
  navbarPage("Navigation",
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
        fill = TRUE,
        heights_equal = "all",
        card(textOutput("prefilter_text")),
        card(textOutput("filter_text")),
      ),
      layout_column_wrap(
        width = 1 / 2,
        height = 340,
        fill = TRUE,
        heights_equal = "all",
        card(full_screen = TRUE,
          card_header("Prefilter Counts"),
          card_body_fill(verbatimTextOutput("prefilter_summary"))
        ),
        card(full_screen = TRUE,
             card_header("Postfilter Counts"),
             card_body_fill(verbatimTextOutput("filtered_summary"))
        ),
      )
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
  prefilter_dim <- reactive(dim(data())[[1]])
  output$prefilter_text <- renderText(
    paste("Before filtering, there are",prefilter_dim(),"genes sequenced.")
  )
  output$prefilter_summary <- renderPrint(
    summary(data()$counts)
  )
  
  filtered_data <- reactive({
    keep <- rowSums(cpm(data()) > 5) >= 3
    data()[keep, ]
  })
  remain_dim <- reactive(dim(filtered_data())[[1]])
  removed_dim <- reactive(dim(data())[[1]] - dim(filtered_data())[[1]])
  output$filter_text <- renderText(
    paste("Filtering! Removing",
          removed_dim(),
           "genes and",
            remain_dim(),
           "genes remaining.")
  )
  output$filtered_summary <- renderPrint(
    summary(filtered_data()$counts)
  )
  
  
  # library(RColorBrewer)
  # colors <- brewer.pal(7, "Set2")
  # x <- as.factor(d$samples$group)
  # 
  # par(mar=c(12,4,2,2))
  # boxplot(d$counts, outline=FALSE, main="Before Normalization", col=colors[x], las=2)
  # #	Saved as: Before Normalization_Expression.png
  # 
  # plotRLE(d$counts, outline=FALSE, main="Before Normalization", ylab="RLE", col=colors[x], las=2) #Same graph as set2 graph
  # #	Saved as: Before Normalization RLE.png
  # 
  # par(mar=c(5,5,4,3))
  # plotPCA(d$counts, ylim=c(-1, 1), xlim=c(-1, 1), col=colors[x], main="Before Normalization", cex=1.0)
  # #	Saved as: Before Normalization PCA.png
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
