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
library(ggrepel)
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
      # Sidebar with file upload widgets
      sidebarLayout(
        sidebarPanel(
          # Metadata file upload
          card(
            card_header(class = "bg-dark", "Upload metadata.csv File (containing list of files with grouping information"),
            card_body(
              # Sidebar panel for file upload
              fileInput("metadatafile", label = NULL, multiple = FALSE, accept = c(".csv",".txt")),
              # Input: Select separator
              radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
              actionButton("upload_meta", "Upload metadata file")
            )
          ),
          # FeatureCount files upload
          card(
            card_header(class = "bg-dark", "Upload featurecount files"),
            card_body(
              fileInput("featurecountfiles", label = NULL, multiple = TRUE, accept = c(".csv",".txt")),
              actionButton("upload_featureCounts", "Upload featureCount files")
            )
          ),
          # Default data upload
          card(
            card_header(class = "bg-dark", "Practice with demo data"),
            card_body(
              actionButton("demo_data", "Load demo data")
            )
          )
        ),
        # Main panel with data visualization
        mainPanel(
          layout_column_wrap(
            width = 1,
            height = "50vh",
            fill = TRUE,
            heights_equal = "all",
            # Metadata file contents
            card(full_screen = TRUE, card_header("Metadata File"), div(tableOutput("metadata_contents"))),
            layout_column_wrap(
              width = 1/2,
              height = "50vh",
              fill = TRUE,
              heights_equal = "all",
              # DGE Samples table
              card(full_screen = TRUE, card_header("DGE Samples"), div(tableOutput("dge_samples"))),
              # DGE Counts table
              card(full_screen = TRUE, card_header("DGE Counts"), div(DT::dataTableOutput("dge_counts")))
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
        # Raw text output
        card(full_screen = TRUE, card_header(textOutput("raw_text")), div(span(style = "font-size: 12px;", verbatimTextOutput("raw_summary")))),
        # Filtered text output
        card(full_screen = TRUE, card_header(textOutput("filter_text")), div(span(style = "font-size: 12px;", verbatimTextOutput("filtered_summary"))))
      ),
      layout_column_wrap(
        width = 1/3,
        height = "50vh",
        fill = TRUE,
        heights_equal = "all",
        # Raw gene expression plot
        card(full_screen = TRUE, card_header("Raw gene expression"), div(plotOutput("raw_boxplot"))),
        # Raw relative level of expression plot
        card(full_screen = TRUE, card_header("Raw relative level of expression"), div(plotOutput("raw_RLE"))),
        # Raw PCA plot
        card(full_screen = TRUE, card_header("Raw PCA"), div(plotOutput("raw_pca")))
      )
    ),
    
    tabPanel(
      "3. Quantile normalization",
      layout_column_wrap(
        width = 1/2,
        height = "33vh",
        fill = TRUE,
        heights_equal = "all",
        # Summary of filtered data in step 2
        card(full_screen = TRUE, card_header("Summary of filtered data in step 2"), div(span(style = "font-size: 12px;", verbatimTextOutput("filtered_summary2")))),
        # Summary of quantile normalized data in step 3
        card(full_screen = TRUE, card_header("Summary of quantile normalized data in step 3"), div(span(style = "font-size: 12px;", verbatimTextOutput("norm_summary"))))
      ),
      layout_column_wrap(
        width = 1/3,
        height = "33vh",
        fill = TRUE,
        heights_equal = "all",
        # Raw gene expression plot for step 2
        card(full_screen = TRUE, card_header("Raw gene expression"), div(plotOutput("raw_boxplot2"))),
        # Raw relative level of expression plot for step 2
        card(full_screen = TRUE, card_header("Raw relative level of expression"), div(plotOutput("raw_RLE2"))),
        # Raw PCA plot for step 2
        card(full_screen = TRUE, card_header("Raw PCA"), div(plotOutput("raw_pca2")))
      ),
      layout_column_wrap(
        width = 1/3,
        height = "33vh",
        fill = TRUE,
        heights_equal = "all",
        # Quantile normalized gene expression plot
        card(full_screen = TRUE, card_header("Quantile normalized gene expression"), div(plotOutput("norm_boxplot"))),
        # Quantile normalized relative level of expression plot
        card(full_screen = TRUE, card_header("Quantile normalized relative level of expression"), div(plotOutput("norm_RLE"))),
        # Quantile normalized PCA plot
        card(full_screen = TRUE, card_header("Quantile normalized PCA"), div(plotOutput("norm_pca")))
      )
    ),
    
    tabPanel(
      "4. TMM Normalization",
      layout_column_wrap(
        width = 1/2,
        height = "50vh",
        fill = TRUE,
        heights_equal = "all",
        # Effective library size table
        card(full_screen = TRUE, card_header("Effective library size"), div(span(style = "font-size: 12px;", tableOutput("eff_libsize")))),
        # Data after tagwise disperson (dT) output
        card(full_screen = TRUE, card_header("Data after tagwise disperson (dT)"), div(span(style = "font-size: 12px;", verbatimTextOutput("dT_output"))))
      ),
      layout_column_wrap(
        width = 1/3,
        height = "50vh",
        fill = TRUE,
        heights_equal = "all",
        # BCV tagwise dispersion plot
        card(full_screen = TRUE, card_header("BCV tagwise dispersion"), div(plotOutput("BCV_tagwise_dispersion"))),
        # Counts per million (cpm) table
        card(full_screen = TRUE, card_header("Counts per million (cpm) (This table may take a while to load)"), div(DT::dataTableOutput("cpm_counts_table"))),
        # Log2 CPM counts table
        card(full_screen = TRUE, card_header("Log2Counts per million (log2cpm) (This table may take a while to load)"), div(DT::dataTableOutput("log2cpm_counts_table"))),
      )
    ),
    
    
    tabPanel(
      "5. Data visualization",
      layout_column_wrap(
        fill = TRUE,
        heights_equal = "all",
        card(full_screen = TRUE,
          card_header("% total variance covered by each PC"),
          div(plotOutput("percentVar_perPC"))
        ),
        # PCA graph
        card(
          full_screen = TRUE,
          card_header("Principal Component Analysis"),
          div(
            selectInput("pc_number1", "Select the first PC:", choices = NULL),
            selectInput("pc_number2", "Select the second PC:", choices = NULL),
            plotOutput("pca_plot")
          )
        ),
      )
    ),
    
    tabPanel(
      "6. EdgeR Pairwise Analysis",
      sidebarLayout(
        sidebarPanel(
          # Treatment group selection
          selectInput("treatedfactor", "Select treated factor group (single choice only) ", choices = NULL, multiple = FALSE),
          textInput("treatedfactor_name", "Name for the treated group (for example, KO in KO vs WT group)", value = "Treatment"),
          # Baseline group selection
          selectInput("basefactor", "Select base factor group (single choice only. If needed, update the group names in the input metadata.csv)", choices = NULL, multiple = FALSE),
          textInput("basefactor_name", "Name for the baseline group (for example, WT in KO vs WT group)", value = "Baseline"),
          # Start comparison
          actionButton("compare", "Compare"),
          # Metadata overview aiding users to select the groups
          card(full_screen = TRUE, card_header("Metadata Overview"), div(span(style = "font-size: 12px;", verbatimTextOutput("sample_overview")))),
        ),
        mainPanel(
          # Display the comparison text
          tags$style(HTML("#comparison_text {font-size: 20pt;}")),
          textOutput("comparison_text"),
          # Comparison results table
          card(full_screen = TRUE, card_header("Comparison table"), div(DT::dataTableOutput("results"))),
          plotOutput("volcanoPlot")
        )
      )
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
  norm_DGE_list <- reactive({
    norm_DGE <- DGEList(counts = norm_data(), group = g())
    norm_DGE <- calcNormFactors(norm_DGE)
    norm_DGE$samples$eff.lib.size <- norm_DGE$samples$lib.size * norm_DGE$samples$norm.factors # add effective library size
    # Calculate CPM
    norm_DGE$cpm <- cpm(norm_DGE)
    norm_DGE
  })
  
  dC <- reactive(estimateCommonDisp(norm_DGE_list())) # estimates common dispersion
  dT <- reactive(estimateTagwiseDisp(dC())) # estimate Tagwise dispersions
  
  output$BCV_tagwise_dispersion <- renderPlot(
    plotBCV(dT(), main = "BCV Tagwise Dispersion")
  )
  
  output$eff_libsize <- renderTable(
    norm_DGE_list()$samples,
    rownames = TRUE,
    hover = TRUE
  )
  
  output$dT_output <- renderPrint(
    dT()
  )
  
  # Output CPM table (This table may take a while to load)
  output$cpm_counts_table <- DT::renderDataTable({
    DT::datatable(norm_DGE_list()$cpm |> round(2), options = list(scrollX = TRUE))
  })
  
  # Output log2(CPM) table (This table may take a while to load)
  output$log2cpm_counts_table <- DT::renderDataTable({
    DT::datatable(log2(norm_DGE_list()$cpm) |> round(2), options = list(scrollX = TRUE))
  })

  # Tab 5: Data visualization
  
  # Calculate PCA data
  pca_data <- reactive({
    prcomp(t(log2(norm_DGE_list()$cpm)), center = TRUE, scale. = TRUE)
  })
  
  # Calculate percent variance covered per PC
  pca_percent_variance <- reactive({
    pca_percent_variance <- data.frame(
      variance = pca_data()$sdev^2
    ) |>
      mutate(
        PC = 1:length(variance),
        prop_variance = variance / sum(variance)
      )
    pca_percent_variance
  })

  # Plot for percent variance covered per PC
  output$percentVar_perPC <- renderPlot({
    ggplot(pca_percent_variance(), aes(x = PC, y = prop_variance)) +
      geom_line(color = "blue") +
      geom_text(
        aes(
          label = scales::percent(round(prop_variance, 2)),
          x = PC, y = prop_variance + 0.01
        ),
        hjust = 0
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Principal Component", y = "Percentage of variance covered") +
      theme_bw()
  })

  # Generating a list of the principal components
  principal_components <- reactive({
    tryCatch({ # Adding a trycatch so that when the data is empty, we through a default PC1 and PC2 to bypass error warnings. 
      num_cols <- ncol(pca_data()$x)
      paste0("PC", 1:num_cols) |> as.list()
    }, error = function(e) {
      paste0("PC", 1:2) |> as.list()
    })
  })

  # Update the choices for selectInput based on the number of principal components
  observe({
    updateSelectInput(session, "pc_number1", choices = principal_components(), selected = principal_components()[1])
  })
  observe({
    updateSelectInput(session, "pc_number2", choices = principal_components(), selected = principal_components()[2])
  })
  
  # Plot PCA graph
  output$pca_plot <- renderPlot({
    req(input$pc_number1, input$pc_number2)
    pca_df <- pca_data()$x |> as.data.frame()
    percentVariance <- pca_percent_variance()$prop_variance
    ggplot(pca_df, aes_string(x = input$pc_number1, y = input$pc_number2)) +
      geom_point(size=3, aes(color=g())) +
      geom_text_repel(aes(label = rownames(pca_df), color=g())) +
      labs(
        x = paste0(input$pc_number1, " (", scales::percent(round(percentVariance[as.numeric(gsub("PC", "", input$pc_number1))], 2)), ")"),
        y = paste0(input$pc_number2, " (", scales::percent(round(percentVariance[as.numeric(gsub("PC", "", input$pc_number2))], 2)), ")")
      ) +
      theme_bw() +
      scale_color_brewer(name="Group", palette="Set2")
  })
  
  # Tab 6: EdgeR Pairwise Analysis
  # Display the samples list so users can identify which groups to compare
  output$sample_overview <- renderPrint(
    dT()$samples
  )
  
  # Generating a list of the sample groups
  sample_groups <- reactive({
    # Adding a trycatch so that when the data is empty, we through a default PC1 and PC2 to bypass error warnings. 
    tryCatch({
      sample_groups <- sort(unique(dT()$samples$group))
    }, error = function(e) {
      sample_groups <- c("Check your input file to include factors")
    })
  })
  
  # Update the choices for selectInput based on the sample groups
  observe({
    updateSelectInput(session, "basefactor", choices = sample_groups(), selected = sample_groups()[length(sample_groups())])
    updateSelectInput(session, "treatedfactor", choices = sample_groups(), selected = sample_groups()[1])
  })
  
  observeEvent(input$compare, {
    basefactor <- input$basefactor
    treatedfactor <- input$treatedfactor
    
    basefactor_name <- input$basefactor_name
    treatedfactor_name <- input$treatedfactor_name
    
    # Output the comparison test name
    output$comparison_text <- renderText(
      paste("Comparing", treatedfactor_name, "vs", basefactor_name), 
    )
    
    # Ensure that both groups are selected and have elements
    if (length(basefactor) > 0 && length(treatedfactor) > 0) {
      # Exact test calculation on TagWise dispersion
      et_result <- exactTest(dT(), pair = c(basefactor, treatedfactor))
      
      # Differential expression analysis
      de_result <- decideTestsDGE(et_result, p = 0.05, adjust = "BH")
      de_result_table <- topTags(et_result, n = nrow(dT()$counts), adjust.method = "BH", sort.by = "p.value")$table
      
      # Add significance test columns
      de_result_table$`Significance(FC>=+/-1.5 FDR<=0.01)` <- ifelse(de_result_table$logFC <= -log2(1.5), ifelse(de_result_table$FDR <= 0.01, -1, 0), ifelse(de_result_table$logFC >= log2(1.5), ifelse(de_result_table$FDR <= 0.01, 1, 0), 0))
      de_result_table$`Significance(FC>=+/-2 FDR<=0.05)` <- ifelse(de_result_table$logFC <= -log2(2), ifelse(de_result_table$FDR <= 0.05, -1, 0), ifelse(de_result_table$logFC >= log2(2), ifelse(de_result_table$FDR <= 0.05, 1, 0), 0))
      
      # Insert Fold Change column
      de_result_table$FC <- 2^de_result_table$logFC
      
      # Reorder columns
      de_result_table <- de_result_table[c("FC", "logFC", "PValue", "FDR", "Significance(FC>=+/-1.5 FDR<=0.01)", "Significance(FC>=+/-2 FDR<=0.05)")]
      
      # Save results to file
      # write.table(de_result_table, file = paste0("DGE_", treatedfactor_name, "_vs_", basefactor_name, "_TagDisp.txt"), sep = "\t")
      
      # Output results table
      output$results <- DT::renderDataTable({
        DT::datatable(de_result_table |> mutate(FC = round(FC, 2), logFC = round(logFC, 2), PValue = scales::scientific(PValue, digits = 2), FDR = scales::scientific(FDR, digits = 2)), options = list(scrollX = TRUE))
      })
      
      # Generate Volcano plot
      output$volcanoPlot <- renderPlot({
        plot(de_result_table$logFC, -log10(de_result_table$FDR), xlab = "Log Fold Change", ylab = "-log10FDR", main = paste(treatedfactor_name, "vs", basefactor_name, "Tagwise Dispersion"), pch = 19, cex = 0.2)
      })
    }
  })

  # Tab 6: EdgeR Pairwise Analysis
  
  # output$pca_output <- renderPrint(
  #   rownames(pca_data()$x |> as.data.frame())
  # )
  
  # #	The exact test is based on the qCML methods. Knowing the conditional distribution
  # #	for the sum of counts in a group, we can compute exact p-values by summing over all sums
  # #	of counts that have a probability less than the probability under the null hypothesis of the
  # #	observed sum of counts. The exact test for the negative binomial distribution has strong
  # #	parallels with Fisher's exact test. The exact test is only applicable to experiments
  # #	with a single factor. The testing can be done by using the function exactTest(), and the
  # #	function allows both common dispersion and tagwise dispersion approaches.
  # 
  # # Groups:
  # #		Th0__Bap1KO
  # #		Th0__Bap1WT
  # #		Th17_Bap1KO
  # #		Th17_Bap1WT
  # #		Th2__Bap1KO
  # #		Th2__Bap1WT
  # 
  # # Exact test calculation on TagWise dispersion 
  # et_dt_Th0___KO_vs_WT      <- exactTest(dT,pair=c("Th0__Bap1WT","Th0__Bap1KO"))
  # et_dt_Th17__KO_vs_WT      <- exactTest(dT,pair=c("Th17_Bap1WT","Th17_Bap1KO"))
  # et_dt_Th2___KO_vs_WT      <- exactTest(dT,pair=c("Th2__Bap1WT","Th2__Bap1KO"))
  # et_dt_Th17_vs_Th0_WT      <- exactTest(dT,pair=c("Th0__Bap1WT","Th17_Bap1WT"))
  # et_dt_Th2__vs_Th0_WT      <- exactTest(dT,pair=c("Th0__Bap1WT","Th2__Bap1WT"))
  # 
  # 
  # #The total number of differentially expressed genes at FDR< 0:05 is:
  # de_dt_Th0___KO_vs_WT      <- decideTestsDGE(et_dt_Th0___KO_vs_WT, p=0.05, adjust="BH")
  # de_dt_Th17__KO_vs_WT      <- decideTestsDGE(et_dt_Th17__KO_vs_WT, p=0.05, adjust="BH")
  # de_dt_Th2___KO_vs_WT      <- decideTestsDGE(et_dt_Th2___KO_vs_WT, p=0.05, adjust="BH")
  # de_dt_Th17_vs_Th0_WT      <- decideTestsDGE(et_dt_Th17_vs_Th0_WT, p=0.05, adjust="BH")
  # de_dt_Th2__vs_Th0_WT      <- decideTestsDGE(et_dt_Th2__vs_Th0_WT, p=0.05, adjust="BH")
  # 
  # #Merge to find common DE genes in KO vs WT tests
  # de_dt_Th0___KO_vs_WT_result <- as.matrix(de_dt_Th0___KO_vs_WT[,1])
  # de_dt_Th17__KO_vs_WT_result <- as.matrix(de_dt_Th17__KO_vs_WT[,1])
  # de_dt_Th2___KO_vs_WT_result <- as.matrix(de_dt_Th2___KO_vs_WT[,1])
  # de_dt_Th17_vs_Th0_WT_result <- as.matrix(de_dt_Th17_vs_Th0_WT[,1])
  # de_dt_Th2__vs_Th0_WT_result <- as.matrix(de_dt_Th2__vs_Th0_WT[,1])
  # colnames(de_dt_Th0___KO_vs_WT_result) <- c("Th0_KOvsWT")
  # colnames(de_dt_Th17__KO_vs_WT_result) <- c("Th17_KOvsWT")
  # colnames(de_dt_Th2___KO_vs_WT_result) <- c("Th2_KOvsWT")
  # colnames(de_dt_Th17_vs_Th0_WT_result) <- c("WT_Th17vsTh0")
  # colnames(de_dt_Th2__vs_Th0_WT_result) <- c("WT_Th2vsTh0")
  # 
  # #	de_dt_KOvsWT_summary <- cbind (de_dt_preB_WT_vs_KO_result, de_dt_MPP1_WT_vs_KO_result, de_dt_MPP2_WT_vs_KO_result)
  # #	
  # #	#	   HSC_WT+HSC_KO MPP1_WT+MPP1_KO MPP2_WT+MPP2_KO
  # #	#	-1           518             158              44
  # #	#	0          11016           11941           12019
  # #	#	1            652              87             123
  # #	
  # #	de_common_upregulated <- which(de_dt_KOvsWT_summary[,1]==1 & de_dt_KOvsWT_summary[,2]==1 & de_dt_KOvsWT_summary[,3]==1)
  # #	de_common_downregulated <- which(de_dt_KOvsWT_summary[,1]==-1 & de_dt_KOvsWT_summary[,2]==-1 & de_dt_KOvsWT_summary[,3]==-1)
  # #	length(de_common_upregulated)
  # #	#	[1] 18
  # #	length(de_common_downregulated)
  # #	#	[1] 12
  # #	
  # #	#  DE genes that are in all three KOvsWT comparisons. Top-line is the gene name, followed by gene order in the second line. 
  # #	de_common_upregulated
  # #	#      Phlda3          Ier5        Zfp365         Ccng1          Etv4 9030617O03Rik          Gch1        Cdkn1a         Zmat3 
  # #	#         426           476           906          1432          2020          2571          3328          4679          6830 
  # #	#       Psrc1           Gem         Stap1           Bax         Ccnd1          Mlkl        Rps27l      Sh3bgrl2         Eda2r 
  # #	#        7207          7391          8435          9936         10452         10971         11429         11494         11985
  # #	de_common_downregulated
  # #	#	   Rpl37a    Obsl1    Trib2 Arhgef28    Rps14   Spire1    Eef1g  Rpl22l1    Hspb8     Rps3    Rpl13   Rpl36a 
  # #	#	      208      240     2285     3088     5292     5327     5555     6820     8621    10111    11041    12056
  # #	
  # #	par(mar=c(2,2,2,2))
  # #	vennDiagram(de_dt_KOvsWT_summary, circle.col=c("turquoise", "salmon", "green"))
  # #	title(main="ERT2 RNA-Seq Differentially Expressed Gene Counts")
  # 
  # # To retrieve and visualize the results and p-values, the topTags and plotSmear functions can be used. topTags generates a table that contains the statistical information calculated by the exactTest function. 
  # de_dt_Th0___KO_vs_WT.table <- topTags(et_dt_Th0___KO_vs_WT, n=nrow(dC$counts), adjust.method="BH", sort.by="p.value" )$table
  # de_dt_Th17__KO_vs_WT.table <- topTags(et_dt_Th17__KO_vs_WT, n=nrow(dC$counts), adjust.method="BH", sort.by="p.value" )$table
  # de_dt_Th2___KO_vs_WT.table <- topTags(et_dt_Th2___KO_vs_WT, n=nrow(dC$counts), adjust.method="BH", sort.by="p.value" )$table
  # de_dt_Th17_vs_Th0_WT.table <- topTags(et_dt_Th17_vs_Th0_WT, n=nrow(dC$counts), adjust.method="BH", sort.by="p.value" )$table
  # de_dt_Th2__vs_Th0_WT.table <- topTags(et_dt_Th2__vs_Th0_WT, n=nrow(dC$counts), adjust.method="BH", sort.by="p.value" )$table
  # 
  # # Add significance test to the result FC1.5 p <=0.01
  # de_dt_Th0___KO_vs_WT.table$"Significance(FC>=+/-1.5 FDR<=0.01)" <-ifelse(de_dt_Th0___KO_vs_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th0___KO_vs_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th0___KO_vs_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th0___KO_vs_WT.table$FDR<=0.01,1,0),0))
  # de_dt_Th17__KO_vs_WT.table$"Significance(FC>=+/-1.5 FDR<=0.01)" <-ifelse(de_dt_Th17__KO_vs_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th17__KO_vs_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th17__KO_vs_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th17__KO_vs_WT.table$FDR<=0.01,1,0),0))
  # de_dt_Th2___KO_vs_WT.table$"Significance(FC>=+/-1.5 FDR<=0.01)" <-ifelse(de_dt_Th2___KO_vs_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th2___KO_vs_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th2___KO_vs_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th2___KO_vs_WT.table$FDR<=0.01,1,0),0))
  # de_dt_Th17_vs_Th0_WT.table$"Significance(FC>=+/-1.5 FDR<=0.01)" <-ifelse(de_dt_Th17_vs_Th0_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th17_vs_Th0_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th17_vs_Th0_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th17_vs_Th0_WT.table$FDR<=0.01,1,0),0))
  # de_dt_Th2__vs_Th0_WT.table$"Significance(FC>=+/-1.5 FDR<=0.01)" <-ifelse(de_dt_Th2__vs_Th0_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th2__vs_Th0_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th2__vs_Th0_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th2__vs_Th0_WT.table$FDR<=0.01,1,0),0))
  # 
  # ## Add significance test to the result FC1.5 p <=0.05      
  # #de_dt_Th0___KO_vs_WT.table$"Significance(FC>=+/-1.5 FDR<=0.05)" <-ifelse(de_dt_Th0___KO_vs_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th0___KO_vs_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th0___KO_vs_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th0___KO_vs_WT.table$FDR<=0.05,1,0),0))
  # #de_dt_Th17__KO_vs_WT.table$"Significance(FC>=+/-1.5 FDR<=0.05)" <-ifelse(de_dt_Th17__KO_vs_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th17__KO_vs_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th17__KO_vs_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th17__KO_vs_WT.table$FDR<=0.05,1,0),0))
  # #de_dt_Th2___KO_vs_WT.table$"Significance(FC>=+/-1.5 FDR<=0.05)" <-ifelse(de_dt_Th2___KO_vs_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th2___KO_vs_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th2___KO_vs_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th2___KO_vs_WT.table$FDR<=0.05,1,0),0))
  # #de_dt_Th17_vs_Th0_WT.table$"Significance(FC>=+/-1.5 FDR<=0.05)" <-ifelse(de_dt_Th17_vs_Th0_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th17_vs_Th0_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th17_vs_Th0_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th17_vs_Th0_WT.table$FDR<=0.05,1,0),0))
  # #de_dt_Th2__vs_Th0_WT.table$"Significance(FC>=+/-1.5 FDR<=0.05)" <-ifelse(de_dt_Th2__vs_Th0_WT.table$logFC<=-log2(1.5),ifelse(de_dt_Th2__vs_Th0_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th2__vs_Th0_WT.table$logFC>=log2(1.5),ifelse(de_dt_Th2__vs_Th0_WT.table$FDR<=0.05,1,0),0))
  # 
  # ## Add significance test to the result FC2.0 p <=0.01
  # #de_dt_Th0___KO_vs_WT.table$"Significance(FC>=+/-2 FDR<=0.01)" <-ifelse(de_dt_Th0___KO_vs_WT.table$logFC<=-log2(2),ifelse(de_dt_Th0___KO_vs_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th0___KO_vs_WT.table$logFC>=log2(2),ifelse(de_dt_Th0___KO_vs_WT.table$FDR<=0.01,1,0),0))
  # #de_dt_Th17__KO_vs_WT.table$"Significance(FC>=+/-2 FDR<=0.01)" <-ifelse(de_dt_Th17__KO_vs_WT.table$logFC<=-log2(2),ifelse(de_dt_Th17__KO_vs_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th17__KO_vs_WT.table$logFC>=log2(2),ifelse(de_dt_Th17__KO_vs_WT.table$FDR<=0.01,1,0),0))
  # #de_dt_Th2___KO_vs_WT.table$"Significance(FC>=+/-2 FDR<=0.01)" <-ifelse(de_dt_Th2___KO_vs_WT.table$logFC<=-log2(2),ifelse(de_dt_Th2___KO_vs_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th2___KO_vs_WT.table$logFC>=log2(2),ifelse(de_dt_Th2___KO_vs_WT.table$FDR<=0.01,1,0),0))
  # #de_dt_Th17_vs_Th0_WT.table$"Significance(FC>=+/-2 FDR<=0.01)" <-ifelse(de_dt_Th17_vs_Th0_WT.table$logFC<=-log2(2),ifelse(de_dt_Th17_vs_Th0_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th17_vs_Th0_WT.table$logFC>=log2(2),ifelse(de_dt_Th17_vs_Th0_WT.table$FDR<=0.01,1,0),0))
  # #de_dt_Th2__vs_Th0_WT.table$"Significance(FC>=+/-2 FDR<=0.01)" <-ifelse(de_dt_Th2__vs_Th0_WT.table$logFC<=-log2(2),ifelse(de_dt_Th2__vs_Th0_WT.table$FDR<=0.01,-1,0),ifelse(de_dt_Th2__vs_Th0_WT.table$logFC>=log2(2),ifelse(de_dt_Th2__vs_Th0_WT.table$FDR<=0.01,1,0),0))
  # 
  # # Add significance test to the result FC2.0 p <=0.05      
  # de_dt_Th0___KO_vs_WT.table$"Significance(FC>=+/-2 FDR<=0.05)" <-ifelse(de_dt_Th0___KO_vs_WT.table$logFC<=-log2(2),ifelse(de_dt_Th0___KO_vs_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th0___KO_vs_WT.table$logFC>=log2(2),ifelse(de_dt_Th0___KO_vs_WT.table$FDR<=0.05,1,0),0))
  # de_dt_Th17__KO_vs_WT.table$"Significance(FC>=+/-2 FDR<=0.05)" <-ifelse(de_dt_Th17__KO_vs_WT.table$logFC<=-log2(2),ifelse(de_dt_Th17__KO_vs_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th17__KO_vs_WT.table$logFC>=log2(2),ifelse(de_dt_Th17__KO_vs_WT.table$FDR<=0.05,1,0),0))
  # de_dt_Th2___KO_vs_WT.table$"Significance(FC>=+/-2 FDR<=0.05)" <-ifelse(de_dt_Th2___KO_vs_WT.table$logFC<=-log2(2),ifelse(de_dt_Th2___KO_vs_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th2___KO_vs_WT.table$logFC>=log2(2),ifelse(de_dt_Th2___KO_vs_WT.table$FDR<=0.05,1,0),0))
  # de_dt_Th17_vs_Th0_WT.table$"Significance(FC>=+/-2 FDR<=0.05)" <-ifelse(de_dt_Th17_vs_Th0_WT.table$logFC<=-log2(2),ifelse(de_dt_Th17_vs_Th0_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th17_vs_Th0_WT.table$logFC>=log2(2),ifelse(de_dt_Th17_vs_Th0_WT.table$FDR<=0.05,1,0),0))
  # de_dt_Th2__vs_Th0_WT.table$"Significance(FC>=+/-2 FDR<=0.05)" <-ifelse(de_dt_Th2__vs_Th0_WT.table$logFC<=-log2(2),ifelse(de_dt_Th2__vs_Th0_WT.table$FDR<=0.05,-1,0),ifelse(de_dt_Th2__vs_Th0_WT.table$logFC>=log2(2),ifelse(de_dt_Th2__vs_Th0_WT.table$FDR<=0.05,1,0),0))
  # 
  # # Insert Fold Change column
  # de_dt_Th0___KO_vs_WT.table$FC <- 2^de_dt_Th0___KO_vs_WT.table$logFC
  # de_dt_Th17__KO_vs_WT.table$FC <- 2^de_dt_Th17__KO_vs_WT.table$logFC
  # de_dt_Th2___KO_vs_WT.table$FC <- 2^de_dt_Th2___KO_vs_WT.table$logFC
  # de_dt_Th17_vs_Th0_WT.table$FC <- 2^de_dt_Th17_vs_Th0_WT.table$logFC
  # de_dt_Th2__vs_Th0_WT.table$FC <- 2^de_dt_Th2__vs_Th0_WT.table$logFC
  # 
  # # To keep the analysis results in tab-delimited format
  # write.table(de_dt_Th0___KO_vs_WT.table, file="DGE_Th0___KO_vs_WT_TagDisp.txt", sep="\t")
  # write.table(de_dt_Th17__KO_vs_WT.table, file="DGE_Th17__KO_vs_WT_TagDisp.txt", sep="\t")
  # write.table(de_dt_Th2___KO_vs_WT.table, file="DGE_Th2___KO_vs_WT_TagDisp.txt", sep="\t")
  # write.table(de_dt_Th17_vs_Th0_WT.table, file="DGE_Th17_vs_Th0_WT_TagDisp.txt", sep="\t")
  # write.table(de_dt_Th2__vs_Th0_WT.table, file="DGE_Th2__vs_Th0_WT_TagDisp.txt", sep="\t")
  # 
  # # Reorder columns
  # de_dt_Th0___KO_vs_WT.table <- de_dt_Th0___KO_vs_WT.table[c("FC","logFC","PValue","FDR","Significance(FC>=+/-1.5 FDR<=0.01)","Significance(FC>=+/-2 FDR<=0.05)")]
  # de_dt_Th17__KO_vs_WT.table <- de_dt_Th17__KO_vs_WT.table[c("FC","logFC","PValue","FDR","Significance(FC>=+/-1.5 FDR<=0.01)","Significance(FC>=+/-2 FDR<=0.05)")]
  # de_dt_Th2___KO_vs_WT.table <- de_dt_Th2___KO_vs_WT.table[c("FC","logFC","PValue","FDR","Significance(FC>=+/-1.5 FDR<=0.01)","Significance(FC>=+/-2 FDR<=0.05)")]
  # de_dt_Th17_vs_Th0_WT.table <- de_dt_Th17_vs_Th0_WT.table[c("FC","logFC","PValue","FDR","Significance(FC>=+/-1.5 FDR<=0.01)","Significance(FC>=+/-2 FDR<=0.05)")]
  # de_dt_Th2__vs_Th0_WT.table <- de_dt_Th2__vs_Th0_WT.table[c("FC","logFC","PValue","FDR","Significance(FC>=+/-1.5 FDR<=0.01)","Significance(FC>=+/-2 FDR<=0.05)")]
  # 
  # # Make Volcano plots
  # jpeg("Th0___KO_vs_WT_TagDisp_Volcano_CPM5.jpg");plot(de_dt_Th0___KO_vs_WT.table$logFC, -log10(de_dt_Th0___KO_vs_WT.table$FDR), xlab="Log Fold Change", ylab="-log10FDR", main="Th0 KO vs WT Tagwise Dispersion", pch=19, cex=0.2);dev.off()
  # jpeg("Th17__KO_vs_WT_TagDisp_Volcano_CPM5.jpg");plot(de_dt_Th17__KO_vs_WT.table$logFC, -log10(de_dt_Th17__KO_vs_WT.table$FDR), xlab="Log Fold Change", ylab="-log10FDR", main="Th17 KO vs WT Tagwise Dispersion", pch=19, cex=0.2);dev.off()
  # jpeg("Th2___KO_vs_WT_TagDisp_Volcano_CPM5.jpg");plot(de_dt_Th2___KO_vs_WT.table$logFC, -log10(de_dt_Th2___KO_vs_WT.table$FDR), xlab="Log Fold Change", ylab="-log10FDR", main="TH2 KO vs WT Tagwise Dispersion", pch=19, cex=0.2);dev.off()
  # jpeg("Th17_vs_Th0_WT_TagDisp_Volcano_CPM5.jpg");plot(de_dt_Th17_vs_Th0_WT.table$logFC, -log10(de_dt_Th17_vs_Th0_WT.table$FDR), xlab="Log Fold Change", ylab="-log10FDR", main="WT Th17 vs Th0 Tagwise Dispersion", pch=19, cex=0.2);dev.off()
  # jpeg("Th2__vs_Th0_WT_TagDisp_Volcano_CPM5.jpg");plot(de_dt_Th2__vs_Th0_WT.table$logFC, -log10(de_dt_Th2__vs_Th0_WT.table$FDR), xlab="Log Fold Change", ylab="-log10FDR", main="WT Th2 vs Th0 Tagwise Dispersion", pch=19, cex=0.2);dev.off()
  # 
  # # Transform the cpm count matrix into a data frame
  # cpm_counts <- cpm(d)
  # cpm_counts <- data.frame(cpm_counts)
  # cpm_counts$genes <- rownames(cpm_counts)
  # 
  # # Add suffix to each column name of the tables
  # colnames(de_dt_Th0___KO_vs_WT.table)<-lapply(colnames(de_dt_Th0___KO_vs_WT.table), paste0, " (Th0_KO_vs_WT) ")
  # colnames(de_dt_Th17__KO_vs_WT.table)<-lapply(colnames(de_dt_Th17__KO_vs_WT.table), paste0, " (Th17_KO_vs_WT) ")
  # colnames(de_dt_Th2___KO_vs_WT.table)<-lapply(colnames(de_dt_Th2___KO_vs_WT.table), paste0, " (TH2_KO_vs_WT) ")
  # colnames(de_dt_Th17_vs_Th0_WT.table)<-lapply(colnames(de_dt_Th17_vs_Th0_WT.table), paste0, " (WT_Th17_vs_Th0) ")
  # colnames(de_dt_Th2__vs_Th0_WT.table)<-lapply(colnames(de_dt_Th2__vs_Th0_WT.table), paste0, " (WT_Th2_vs_Th0) ")
  # 
  # # Add in a column with the gene names
  # de_dt_Th0___KO_vs_WT.table$genes <- rownames(de_dt_Th0___KO_vs_WT.table)
  # de_dt_Th17__KO_vs_WT.table$genes <- rownames(de_dt_Th17__KO_vs_WT.table)
  # de_dt_Th2___KO_vs_WT.table$genes <- rownames(de_dt_Th2___KO_vs_WT.table)
  # de_dt_Th17_vs_Th0_WT.table$genes <- rownames(de_dt_Th17_vs_Th0_WT.table)
  # de_dt_Th2__vs_Th0_WT.table$genes <- rownames(de_dt_Th2__vs_Th0_WT.table)
  # 
  # # Merge tables together into one final report by order of WTIR/WTUT, KOUT/WTUT, KOIR/WTUT, KOIR/WTIR, KOUT/WTIR, KOIR/KOUT
  # output <- Reduce(function(x, y) merge(x, y, by="genes"), list(de_dt_Th0___KO_vs_WT.table, de_dt_Th17__KO_vs_WT.table, de_dt_Th2___KO_vs_WT.table, de_dt_Th17_vs_Th0_WT.table, de_dt_Th2__vs_Th0_WT.table))
  # 
  # colnames(output)
  # # [1] "genes"                                                "FC (Th0_KO_vs_WT) "                                  
  # # [3] "logFC (Th0_KO_vs_WT) "                                "PValue (Th0_KO_vs_WT) "                              
  # # [5] "FDR (Th0_KO_vs_WT) "                                  "Significance(FC>=+/-1.5 FDR<=0.01) (Th0_KO_vs_WT) "  
  # # [7] "Significance(FC>=+/-2 FDR<=0.05) (Th0_KO_vs_WT) "     "FC (Th17_KO_vs_WT) "                                 
  # # [9] "logFC (Th17_KO_vs_WT) "                               "PValue (Th17_KO_vs_WT) "                             
  # #[11] "FDR (Th17_KO_vs_WT) "                                 "Significance(FC>=+/-1.5 FDR<=0.01) (Th17_KO_vs_WT) " 
  # #[13] "Significance(FC>=+/-2 FDR<=0.05) (Th17_KO_vs_WT) "    "FC (TH2_KO_vs_WT) "                                  
  # #[15] "logFC (TH2_KO_vs_WT) "                                "PValue (TH2_KO_vs_WT) "                              
  # #[17] "FDR (TH2_KO_vs_WT) "                                  "Significance(FC>=+/-1.5 FDR<=0.01) (TH2_KO_vs_WT) "  
  # #[19] "Significance(FC>=+/-2 FDR<=0.05) (TH2_KO_vs_WT) "     "FC (WT_Th17_vs_Th0) "                                
  # #[21] "logFC (WT_Th17_vs_Th0) "                              "PValue (WT_Th17_vs_Th0) "                            
  # #[23] "FDR (WT_Th17_vs_Th0) "                                "Significance(FC>=+/-1.5 FDR<=0.01) (WT_Th17_vs_Th0) "
  # #[25] "Significance(FC>=+/-2 FDR<=0.05) (WT_Th17_vs_Th0) "   "FC (WT_Th2_vs_Th0) "                                 
  # #[27] "logFC (WT_Th2_vs_Th0) "                               "PValue (WT_Th2_vs_Th0) "                             
  # #[29] "FDR (WT_Th2_vs_Th0) "                                 "Significance(FC>=+/-1.5 FDR<=0.01) (WT_Th2_vs_Th0) " 
  # #[31] "Significance(FC>=+/-2 FDR<=0.05) (WT_Th2_vs_Th0) "   
  # 
  # #	output_summary <- output[,c(1,7,8,13,14,19,20)]
  # #	rownames(output_summary) <- output_summary[,1]
  # #	output_summary <- output_summary[,-1]
  # #	
  # #	de_common_upregulated_FDR0.01_FC1.5 <- rownames(output_summary[which(output_summary[,1]==1 & output_summary[,3]==1 & output_summary[,5]==1),])
  # #	de_common_downregulated_FDR0.01_FC1.5 <- rownames(output_summary[which(output_summary[,1]==-1 & output_summary[,3]==-1 & output_summary[,5]==-1),])
  # #	length(de_common_upregulated_FDR0.01_FC1.5)
  # #	#	[1] 12
  # #	length(de_common_downregulated_FDR0.01_FC1.5)
  # #	#	[1] 8
  # #	
  # #	#  DE genes that are in all three KOvsWT comparisons. Top-line is the gene name, followed by gene order in the second line. 
  # #	de_common_upregulated_FDR0.01_FC1.5
  # #	#	 [1] "9030617O03Rik" "Bax"           "Ccnd1"         "Ccng1"         "Cdkn1a"        "Eda2r"        
  # #	#	 [7] "Etv4"          "Gem"           "Phlda3"        "Psrc1"         "Rps27l"        "Zmat3"  
  # #	de_common_downregulated_FDR0.01_FC1.5
  # #	#	[1] "Arhgef28" "Eef1g"    "Rpl22l1"  "Rpl36a"   "Rpl37a"   "Rps3"     "Spire1"   "Trib2"
  # #	
  # #	de_common_upregulated_FDR0.05_FC1.5 <- rownames(output_summary[which(output_summary[,2]==1 & output_summary[,4]==1 & output_summary[,6]==1),])
  # #	de_common_downregulated_FDR0.05_FC1.5 <- rownames(output_summary[which(output_summary[,2]==-1 & output_summary[,4]==-1 & output_summary[,6]==-1),])
  # #	length(de_common_upregulated_FDR0.05_FC1.5)
  # #	#	[1] 17
  # #	length(de_common_downregulated_FDR0.05_FC1.5)
  # #	#	[1] 11
  # #	
  # #	#  DE genes that are in all three KOvsWT comparisons. Top-line is the gene name, followed by gene order in the second line. 
  # #	de_common_upregulated_FDR0.05_FC1.5
  # #	#	 [1] "9030617O03Rik" "Bax"           "Ccnd1"         "Ccng1"         "Cdkn1a"        "Eda2r"        
  # #	#	 [7] "Etv4"          "Gch1"          "Gem"           "Ier5"          "Mlkl"          "Phlda3"       
  # #	#	[13] "Psrc1"         "Rps27l"        "Sh3bgrl2"      "Zfp365"        "Zmat3"       
  # #	de_common_downregulated_FDR0.05_FC1.5
  # #	#	 [1] "Arhgef28" "Eef1g"    "Hspb8"    "Obsl1"    "Rpl22l1"  "Rpl36a"   "Rpl37a"   "Rps14"    "Rps3"    
  # #	#	[10] "Spire1"   "Trib2" 
  # #	
  # #	vennDiagram(output_summary[,c(1,3,5)], circle.col=c("turquoise", "salmon", "green"), cex=0.8)
  # #	title(main="ERT2 RNA-Seq Differentially Expressed Gene Counts FC>=+/-1.5 FDR<=0.01")
  # #	
  # #	vennDiagram(output_summary[,c(2,4,6)], circle.col=c("turquoise", "salmon", "green"), cex=0.8)
  # #	title(main="ERT2 RNA-Seq Differentially Expressed Gene Counts FC>=+/-1.5 FDR<=0.05")
  # 
  # # Add a column that count if the gene is significant (if any significance test has resulted in a non-zero value)
  # output$"Is the Gene with FDR <=0.01 and FC >= +/- 1.5 in any of the significance tests to the left?" <- apply(abs(output[c(6,12,18)]), 1, max)
  # output$"Is the Gene with FDR <=0.05 and FC >= +/- 2.0 in any of the significance tests to the left?" <- apply(abs(output[c(7,13,19)]), 1, max)
  # 
  # # Re-order cpm_counts alphabetically
  # colnames(cpm_counts)
  # # [1] "Th0_Bap1KO_Rep1"  "Th0_Bap1KO_Rep3"  "Th0_Bap1KO_Rep2"  "Th17_Bap1KO_Rep1" "Th17_Bap1KO_Rep3" "Th17_Bap1KO_Rep2" "Th2_Bap1KO_Rep3" 
  # # [8] "Th2_Bap1KO_Rep2"  "Th2_Bap1KO_Rep1"  "Th0_WT_Rep2"      "Th0_WT_Rep1"      "Th0_WT_Rep3"      "Th17_WT_Rep2"     "Th17_WT_Rep1"    
  # #[15] "Th17_WT_Rep3"     "Th2_WT_Rep2"      "Th2_WT_Rep1"      "Th2_WT_Rep3"      "genes"          
  # 
  # output <- merge(output, cpm_counts, by="genes")
  # 
  # write.table(output, file="Th_cells_BAP1KO_ALL_GENES_Output.txt", row.names = FALSE, sep="\t")
  # 
  # 
  
  # Tab 7: Final outlook
}

# Run the application
shinyApp(ui = ui, server = server)
