# Solved package not found message with runing the code below in console.
library(BiocManager)
# options(repos = BiocManager::repositories())
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
          # Input: Specify how many comparisons to make
          numericInput("num_comparisons", "Number of comparisons", value = 1, min = 1),
          
          # Dynamic UI for the comparison inputs
          uiOutput("comparison_inputs"),
          
          # Start comparison
          actionButton("compare", "Compare"),
          
          # Metadata overview aiding users to select the groups
          card(full_screen = TRUE, card_header("Metadata Overview"), div(span(style = "font-size: 12px;", verbatimTextOutput("sample_overview"))))
        ),
        mainPanel(
          # Display the comparison text
          tags$style(HTML("#comparison_text {font-size: 20pt;}")),
          textOutput("comparison_text"),
          
          # Display results table for the selected comparison
          card(full_screen = TRUE, card_header("Comparison table"), div(DT::dataTableOutput("results"))),
          
          # Volcano plot
          card(
            full_screen = TRUE,
            card_header("Volcano plot (default significance FC>=+/-1.5 FDR<=0.01)"),
            div(
              numericInput("fc_cutoff", "Fold Change Cutoff", value = 1.5),
              numericInput("fdr_cutoff", "FDR Cutoff", value = 0.01),
              plotlyOutput("stackedVolcanoPlots")
            )
          )
        )
      )
    ),
    
    tabPanel(
      "7. Download & Preview Report",
      sidebarLayout(
        sidebarPanel(
          downloadButton("download_report", "Download Report (Markdown)"),
          downloadButton("download_csv", "Download CSV (Analysis Data)")
        ),
        mainPanel(
          card(full_screen = TRUE, card_header("Report Preview"),
               uiOutput("report_preview")) # Display the report content
        )
      )
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
    tryCatch({
      sort(unique(dT()$samples$group))
    }, error = function(e) {
      c("Check your input file to include factors")
    })
  })
  
  # Dynamic UI for multiple comparisons
  output$comparison_inputs <- renderUI({
    num_comparisons <- input$num_comparisons
    
    # Create input fields for each comparison dynamically
    lapply(1:num_comparisons, function(i) {
      fluidRow(
        column(12, tags$h4(tags$strong(paste0("Comparison ", i)))),
        
        column(6, selectInput(paste0("treatedfactor_", i), 
                              paste0("Treatment group for comparison ", i), 
                              choices = sample_groups(), selected = sample_groups()[1])),
        column(6, textInput(paste0("treatedfactor_name_", i), 
                            paste0("Name for Treatment ", i, "(no spaces)"), 
                            value = paste0("Treatment_", i))),
        column(6, selectInput(paste0("basefactor_", i), 
                              paste0("Baseline group for comparison ", i), 
                              choices = sample_groups(), selected = sample_groups()[length(sample_groups())])),
        column(6, textInput(paste0("basefactor_name_", i), 
                            paste0("Name for Baseline ", i, "(no spaces)"), 
                            value = paste0("Baseline_", i)))
      )
    })
  })
  
  # Observing when the user clicks the 'compare' button
  observeEvent(input$compare, {
    num_comparisons <- input$num_comparisons
    
    results_list <- lapply(1:num_comparisons, function(i) {
      basefactor <- input[[paste0("basefactor_", i)]]
      treatedfactor <- input[[paste0("treatedfactor_", i)]]
      
      basefactor_name <- input[[paste0("basefactor_name_", i)]]
      treatedfactor_name <- input[[paste0("treatedfactor_name_", i)]]
      
      # Output the comparison test name
      comparison_text <- paste("Comparing", treatedfactor_name, "vs", basefactor_name)
      
      # Ensure that both groups are selected and have elements
      if (length(basefactor) > 0 && length(treatedfactor) > 0) {
        # Perform the exact test calculation on TagWise dispersion
        et_result <- exactTest(dT(), pair = c(basefactor, treatedfactor))
        
        # Differential expression analysis
        de_result_table <- topTags(et_result, n = nrow(dT()$counts), adjust.method = "BH", sort.by = "p.value")$table
        
        # Add significance test columns
        de_result_table <- de_result_table %>%
          mutate(
            `Significance(FC>=+/-1.5 FDR<=0.01)` = case_when(
              logFC <= -log2(1.5) & FDR <= 0.01 ~ -1,
              logFC >= log2(1.5) & FDR <= 0.01 ~ 1,
              TRUE ~ 0
            ),
            `Significance(FC>=+/-2 FDR<=0.05)` = case_when(
              logFC <= -log2(2) & FDR <= 0.05 ~ -1,
              logFC >= log2(2) & FDR <= 0.05 ~ 1,
              TRUE ~ 0
            )
          )
        
        # Insert Fold Change column
        de_result_table$FC <- 2^de_result_table$logFC
        
        # Rename columns to include comparison prefix
        colnames(de_result_table) <- paste0(treatedfactor_name, "_vs_", basefactor_name, "_", colnames(de_result_table))
        
        # Return both the results table and the volcano plot
        return(list(
          comparison_text = comparison_text,
          results_table = de_result_table
          # volcano_plot = volcano_plot
        ))
      }
    })
    
    # Combine the results of all comparisons into a single table
    combined_results <- do.call(cbind, lapply(results_list, function(result) result$results_table))
    
    # Output the combined comparison text
    output$comparison_text <- renderText({
      paste(lapply(results_list, function(result) result$comparison_text), collapse = " | ")
    })
    
    # Output the combined results table
    output$results <- DT::renderDataTable({
      DT::datatable(combined_results %>%
                      mutate(across(ends_with("FC"), round, 2),
                             across(ends_with("logFC"), round, 2),
                             across(ends_with("logCPM"), round, 2),
                             across(ends_with("PValue"), scales::scientific, digits = 2),
                             across(ends_with("FDR"), scales::scientific, digits = 2)),
                    options = list(scrollX = TRUE))
    })
    
    # Observe changes in FDR cutoff, FC cutoff, or when the compare button is clicked
    observeEvent({input$fdr_cutoff; input$fc_cutoff}, {

      num_comparisons <- input$num_comparisons

      # Recreate volcano plots based on updated fdr_cutoff and fc_cutoff
      volcano_plots <- lapply(1:num_comparisons, function(i) {
        basefactor_name <- input[[paste0("basefactor_name_", i)]]
        treatedfactor_name <- input[[paste0("treatedfactor_name_", i)]]

        de_result_table <- results_list[[i]]$results_table # Assuming results_list is already defined
        # Dynamically construct column names
        logFC_col <- paste0(treatedfactor_name, "_vs_", basefactor_name, "_logFC")
        FDR_col <- paste0(treatedfactor_name, "_vs_", basefactor_name, "_FDR")
        
        # Update volcano plot using new fdr_cutoff and fc_cutoff
        volcano_plot <- ggplot(de_result_table, aes_string(x = logFC_col, y = paste0("-log10(", FDR_col, ")"),
                                                           color = "factor(case_when(
                                                         get(logFC_col) <= -log2(input$fc_cutoff) & get(FDR_col) <= input$fdr_cutoff ~ '-1',
                                                         get(logFC_col) >= log2(input$fc_cutoff) & get(FDR_col) <= input$fdr_cutoff ~ '1',
                                                         TRUE ~ '0'))")) +
          geom_point(size = 1.5) +
          scale_color_manual(values = c("0" = "grey", "1" = "red", "-1" = "blue"),
                             guide = guide_legend(title = "Significance Levels",
                                                  override.aes = list(shape = NA))) +
          labs(x = "Log Fold Change (logFC)", y = "-log10(FDR)") +
          theme_minimal()

        return(ggplotly(volcano_plot))  # Return the ggplotly object for interactivity
      })
      
      
      # Render the vertically stacked volcano plots
      output$stackedVolcanoPlots <- renderPlotly({
        subplot(volcano_plots, nrows = num_comparisons%/%2 + 1, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
      })

    })

  })
  
  # Tab 7. Final report outlook
  
  # CSV Download Handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("analysis_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(de_result_table, file, row.names = FALSE)
    }
  )
  
  # Markdown Report Download Handler
  output$download_report <- downloadHandler(
    filename = function() {
      paste("analysis_report_", Sys.Date(), ".md", sep = "")
    },
    content = function(file) {
      # Call a helper function to create markdown content
      writeLines(generate_report_md(), con = file)
    }
  )
  
  # Generate the markdown content
  generate_report_md <- function() {
    report_md <- c(
      "# Analysis Report",
      "",
      "## 1. Summary",
      "This report summarizes the gene differential expression analysis conducted on the provided dataset.",
      "",
      "## 2. PCA Plot",
      "![](pca_plot.png)",
      "",
      "## 3. Volcano Plot",
      "![](volcano_plot.png)",
      "",
      "## 4. Differential Expression Results",
      "",
      "The table below shows the key results of the analysis:",
      "",
      "```{r}",
      "knitr::kable(de_result_table, caption = 'Differential Expression Results')",
      "```",
      "",
      "## 5. Session Info",
      "```{r}",
      "sessionInfo()",
      "```"
    )
    return(report_md)
  }
  
  # Render the report as HTML in the app
  output$report_preview <- renderUI({
    # Generate the markdown content
    report_md <- generate_report_md()
    
    # Convert markdown to HTML
    report_html <- markdown::markdownToHTML(text = paste(report_md, collapse = "\n"), fragment.only = TRUE)
    
    # Render the HTML in the UI
    HTML(report_html)
  })
  
  # Tab 7: Final outlook
}

# Run the application
shinyApp(ui = ui, server = server)
