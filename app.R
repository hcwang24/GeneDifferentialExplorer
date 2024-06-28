# Solved package not found message with runing the code below in console.
library(BiocManager)
options(repos = BiocManager::repositories())
getOption("repos")

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
library(shinyWidgets)
library(preprocessCore)
options(shiny.maxRequestSize = 30 * 1024^2)

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
            card_header(class = "bg-dark", "Upload metadata.csv File (containing list of files with grouping information",),
            card_body(
              # Sidebar panel for file upload
              fileInput("metadatafile", label = NULL, multiple = FALSE, accept = c(".csv",".txt")),

              # Input: Select separator ----
              radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
              actionButton("upload_meta", "Upload metadata file"),),
          ),

          # featureCount files upload
          card(
            card_header(class = "bg-dark", "Upload featurecount files",),
            card_body(
              fileInput("featurecountfiles", label = NULL, multiple = TRUE, accept = c(".csv",".txt")),
              actionButton("upload_featureCounts", "Upload featureCount files"),),
          ),

          # Default data upload
          card(
            card_header(class = "bg-dark","Practice with demo data",),
            card_body(actionButton("demo_data", "Load demo data"),),
          ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          layout_column_wrap(
            width = 1,
            height = "50vh",
            fill = TRUE,
            heights_equal = "all",
            
            card(full_screen = TRUE, card_header("Metadata File"), card_body_fill(tableOutput("metadata_contents"))),
            
            layout_column_wrap(
              width = 1 / 2,
              height = "50vh",
              fill = TRUE,
              heights_equal = "all",
              
              card(full_screen = TRUE, card_header("DGE Samples"), card_body_fill(tableOutput("dge_samples"))),
              card(full_screen = TRUE, card_header("DGE Counts"), card_body_fill(DT::dataTableOutput("dge_counts")))
            )
          )
        )
      )
    ),
    
    tabPanel(
      "2. Exploratory data analysis",
      layout_column_wrap(
        width = 1/2,
        height = "50vh",
        fill = TRUE,
        heights_equal = "all",
        card(full_screen = TRUE, card_header(textOutput("raw_text")), card_body_fill(div(span(style = "font-size: 12px;", verbatimTextOutput("raw_summary"))))),
        card(full_screen = TRUE, card_header(textOutput("filter_text")), card_body_fill(div(span(style = "font-size: 12px;", verbatimTextOutput("filtered_summary")))))
      ),
      layout_column_wrap(
        width = 1/3,
        height = "50vh",
        fill = TRUE,
        heights_equal = "all",
        card(full_screen = TRUE, card_header("Raw gene expression"), card_body_fill(plotOutput("raw_boxplot"))),
        card(full_screen = TRUE, card_header("Raw relative level of expression"), card_body_fill(plotOutput("raw_RLE"))),
        card(full_screen = TRUE, card_header("Raw PCA"), card_body_fill(plotOutput("raw_pca")))
      )
    ),
    
    tabPanel(
      "3. Quantile normalization",
      layout_column_wrap(
        width = 1/2, height = "33vh", fill = TRUE, heights_equal = "all",
        card(full_screen = TRUE, card_header("Summary of filtered data in step 2"), card_body_fill(div(span(style = "font-size: 12px;", verbatimTextOutput("filtered_summary2"))))),
        card(full_screen = TRUE, card_header("Summary of quantile normalized data in step 3"), card_body_fill(div(span(style = "font-size: 12px;", verbatimTextOutput("norm_summary")))))
      ),
      layout_column_wrap(
        width = 1/3, height = "33vh", fill = TRUE, heights_equal = "all",
        card(full_screen = TRUE, card_header("Raw gene expression"), card_body_fill(plotOutput("raw_boxplot2"))),
        card(full_screen = TRUE, card_header("Raw relative level of expression"), card_body_fill(plotOutput("raw_RLE2"))),
        card(full_screen = TRUE, card_header("Raw PCA"), card_body_fill(plotOutput("raw_pca2")))
      ),
      layout_column_wrap(
        width = 1/3, height = "33vh", fill = TRUE, heights_equal = "all",
        card(full_screen = TRUE, card_header("Quantile normalized gene expression"), card_body_fill(plotOutput("norm_boxplot"))),
        card(full_screen = TRUE, card_header("Quantile normalized relative level of expression"), card_body_fill(plotOutput("norm_RLE"))),
        card(full_screen = TRUE, card_header("Quantile normalized PCA"), card_body_fill(plotOutput("norm_pca")))
      )
    ),
    
    tabPanel(
      "4. TMM Normalization",
      layout_column_wrap(
        width = 1/2, height = "50vh", fill = TRUE, heights_equal = "all",
        card(full_screen = TRUE, card_header("Effective library size"), card_body_fill(div(span(style = "font-size: 12px;", tableOutput("eff_libsize"))))),
        card(full_screen = TRUE, card_header("data after tagwise disperson (dT)"), card_body_fill(div(span(style = "font-size: 12px;", verbatimTextOutput("dT_output")))))
      ),
      layout_column_wrap(
        width = 1/3, height = "50vh", fill = TRUE, heights_equal = "all",
        card(full_screen = TRUE, card_header("BCV tagwise dispersion"), card_body_fill(plotOutput("BCV_tagwise_dispersion"))),
        card(full_screen = TRUE, card_header("Counts per million (cpm)"), card_body_fill(DT::dataTableOutput("cpm_counts"))),
        card(full_screen = TRUE, card_header("Log2 CPM counts"), card_body_fill(DT::dataTableOutput("log2_cpm_counts")))
      )
    ),
    
    tabPanel(
      "5. Data visualization",
      layout_column_wrap(
        width = 1 / 2,
        height = 340,
        fill = TRUE,
        heights_equal = "all",
        card(
          full_screen = TRUE,
          card_header("% total variance covered by each PC"),
          card_body_fill(plotOutput("percentVar_perPC"))
        ),
        card(
          full_screen = TRUE,
          card_header("Principal Component Analysis"),
          card_body_fill(
            selectInput("pc_number1", "Select the first PC:", choices = NULL),
            selectInput("pc_number2", "Select the second PC:", choices = NULL),
            plotOutput("pca_plot")
          )
        )
      ),
      layout_column_wrap(
        width = 1 / 3,
        height = 340,
        fill = TRUE,
        heights_equal = "all",
        card(
          full_screen = TRUE,
          card_header("BCV tagwise dispersion"),
          card_body_fill(div(span(style = "font-size: 12px;", verbatimTextOutput("check_output"))))
        ),
        card(
          full_screen = TRUE,
          card_header("Counts per million (cpm)"),
          card_body_fill(DT::dataTableOutput("cpm_counts"))
        ),
        card(
          full_screen = TRUE,
          card_header("Log2 CPM counts"),
          card_body_fill(DT::dataTableOutput("log2_cpm_counts"))
        )
      )
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
server <- function(input, output, session) {
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

  metadata_df <- eventReactive(input$demo_data, {
    read_delim(
      "demo/metafile.csv",
      delim = ",",
      col_names = TRUE,
      show_col_types = FALSE
    )
  })

  data <- eventReactive(input$demo_data, {
    files <- c(
      "immB_HET2_featurecounts.txt",
      "immB_HET3_featurecounts.txt",
      "immB_HET4_featurecounts.txt",
      "immB_KO1_featurecounts.txt",
      "immB_KO2_featurecounts.txt",
      "immB_KO3_featurecounts.txt",
      "immB_KO4_featurecounts.txt",
      "immB_WT2_featurecounts.txt",
      "immB_WT3_featurecounts.txt",
      "immB_WT4_featurecounts.txt",
      "preB_HET1new_featurecounts.txt",
      "preB_HET2_featurecounts.txt",
      "preB_HET3_featurecounts.txt",
      "preB_HET4_featurecounts.txt",
      "preB_KO1_featurecounts.txt",
      "preB_KO2_featurecounts.txt",
      "preB_KO3_featurecounts.txt",
      "preB_KO4_featurecounts.txt",
      "preB_WT2_featurecounts.txt",
      "preB_WT3_featurecounts.txt",
      "preB_WT4_featurecounts.txt"
    )
    file_path <- "demo/"
    d <- readDGE(
      files,
      path = file_path,
      columns = c(1, 3),
      labels = files
    )
    d$samples <- d$samples[, -2] # removing the default group from DGE
    d$samples$files <- files
    if (!is.null(input$upload_meta)) {
      d$samples <- inner_join(d$samples, metadata_df(), by = join_by(files == files))
    }
    d
  })

  output$metadata_contents <- renderTable({
    metadata_df()
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

  # Tab 2: Data and viz
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
  output$filtered_summary <- output$filtered_summary2 <- renderPrint(
    summary(filtered_data()$counts)
  )

  g <- reactive(as.factor(filtered_data()$samples$group))
  colors <- brewer.pal(7, "Set2")

  output$raw_boxplot <- output$raw_boxplot2 <- renderPlot(
    boxplot(filtered_data()$counts, outline = FALSE, main = "Raw Boxplot", col = colors[g()], las = 2)
  )

  output$raw_RLE <- output$raw_RLE2 <- renderPlot(
    plotRLE(filtered_data()$counts, outline = FALSE, main = "Raw RLE", ylab = "RLE", col = colors[g()], las = 2)
  )

  output$raw_pca <- output$raw_pca2 <- renderPlot(
    plotPCA(filtered_data()$counts, main = "Raw PCA", col = colors[g()], cex = 1.0)
  )

  # Tab 3: Apply a quantile normalization with preprocessCore
  q <- reactive(as.matrix(filtered_data()$counts))
  norm_data <- reactive({
    normalized <- normalize.quantiles(q(), copy = TRUE, keep.names = TRUE)
  })

  output$norm_summary <- renderPrint(
    summary(norm_data())
  )

  output$norm_boxplot <- renderPlot(
    boxplot(norm_data(), outline = FALSE, main = "Normalized Boxplot", col = colors[g()], las = 2)
  )

  output$norm_RLE <- renderPlot(
    plotRLE(norm_data(), outline = FALSE, main = "Normalized RLE", ylab = "RLE", col = colors[g()], las = 2)
  )

  output$norm_pca <- renderPlot(
    plotPCA(norm_data(), main = "Normalized PCA", col = colors[g()], cex = 1.0)
  )
  # Tab 4: TMM Normalization
  norm_DGE <- reactive({
    norm_DGE <- DGEList(counts = norm_data(), group = g())
    norm_DGE <- calcNormFactors(norm_DGE)
    norm_DGE$samples$eff.lib.size <- norm_DGE$samples$lib.size * norm_DGE$samples$norm.factors # add effective library size
    norm_DGE
  })

  dC <- reactive(estimateCommonDisp(norm_DGE())) # estimates common dispersion
  dT <- reactive(estimateTagwiseDisp(dC())) # estimate Tagwise dispersions

  output$BCV_tagwise_dispersion <- renderPlot(
    plotBCV(dT(), main = "BCV Tagwise Dispersion")
  )

  output$eff_libsize <- renderTable(
    norm_DGE()$samples,
    rownames = TRUE,
    hover = TRUE
  )

  output$dT_output <- renderPrint(
    dT()
  )

  cpm_counts <- reactive(cpm(norm_DGE()))
  log2_cpm_counts <- reactive(log2(cpm(norm_DGE())))

  output$cpm_counts <- DT::renderDataTable(
    cpm_counts() |>
      round(2),
    options = list(scrollX = TRUE)
  )

  output$log2_cpm_counts <- DT::renderDataTable(
    log2_cpm_counts() |>
      round(2),
    options = list(scrollX = TRUE)
  )

  # Tab 5: Data visualization
  # calculate PCA data
  # pca_data <- reactive({
  #   prcomp(t(log2_cpm_counts()), center = TRUE, scale. = TRUE)
  #   })
  # pca_data_x <- reactive({
  #   colnames(pca_data()$x)
  # })
  # 
  # # Update the choices for selectInput based on the number of principal components
  # observe({
  #   updateSelectInput(session, "pc_number1", choices = list(pca_data_x()))
  # })
  # observe({
  #   updateSelectInput(session, "pc_number2", choices = list(pca_data_x()))
  # })
  # 
  # # calculate percent variance covered per PC
  # pca_percent_variance <- reactive({
  #   pca_percent_variance <- data.frame(
  #     variance = pca_data()$sdev^2
  #   ) |>
  #     mutate(
  #       PC = 1:length(variance),
  #       prop_variance = variance / sum(variance)
  #     )
  #   pca_percent_variance
  # })
  # 
  # output$check_output <- renderPrint(
  #   colnames(pca_data()$x)
  # )

  # # Plot for percent variance covered per PC
  # output$percentVar_perPC <- renderPlot(
  #   ggplot(pca_percent_variance(), aes(x = PC, y = prop_variance)) +
  #     geom_line(color = "blue") +
  #     geom_text(
  #       aes(
  #         label = scales::percent(round(prop_variance, 2)),
  #         x = PC, y = prop_variance + 0.01
  #       ),
  #       hjust = 0
  #     ) +
  #     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #     labs(x = "Principal Component", y = "Proportion of variance covered") +
  #     theme_bw()
  # )
  
  # # Plot PCA graph
  # output$pca_plot <- renderPlot({
  #   pca_data()$x |>
  #     as.data.frame() |>
  #     ggplot(aes(x=input$pc_number1, y=input$pc_number2)) +
  #       geom_point(size=3) +
  #       # labs(x = paste0("PC1 (", percent(percentVariance[1]/sum(percentVariance)),")"), y = paste0("PC2 (", percent(percentVariance[2]/sum(percentVariance)),")")) +
  #       # geom_point(size=3, aes(color=factor_genotype, shape=factor_treatment)) +
  #       # geom_text_repel(size=2, aes(label = rownames(pca_data_restricted), color=factor_genotype)) +
  #       theme_bw()
  #       # theme(text=element_text(size=8)) +
  #       # scale_color_brewer(name="Genotype", palette="Set2") +
  #       # scale_shape_discrete(name="Treatment")
  # })
  
  # summary(pca_data)
  # 
  # pca_data_restricted = data.frame(PC1 = pca_data$x[,1], PC2 = pca_data$x[,2])
  # 

  # 
  # colors <- brewer.pal(6, "Set2")
  # colors
  # 
  # pca_graph
  
  # Tab 6: EdgeR Pairwise Analysis
  # Tab 7: Final outlook
}

# Run the application
shinyApp(ui = ui, server = server)
