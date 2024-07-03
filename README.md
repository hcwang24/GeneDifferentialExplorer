# Gene Differential Expression Explorer

This R Shiny application is designed for exploring gene differential expression in RNASeq data using various statistical techniques powered by EdgeR package and visualizations.

## Author

-   **HanChen Wang**

## Features

-   **File Uploads and Data Handling:**
    -   Upload metadata files (`.csv`, `.txt`) containing grouping information.
    -   Upload featureCount files for differential expression analysis.
    -   Practice with demo data option available.
-   **Exploratory Data Analysis:**
    -   Visualize raw gene expression through boxplots, relative level of expression (RLE), and PCA plots.
    -   Filter and summarize gene counts based on user-defined criteria.
-   **Normalization Techniques:**
    -   Apply Quantile normalization using preprocessCore.
    -   Perform TMM normalization with statistical summaries and visualizations.
-   **Data Visualization:**
    -   Visualize percentage of total variance covered by each Principal Component (PC).
    -   Plot interactive PCA graphs based on user-selected components.
-   **EdgeR Pairwise Analysis:**
    -   Conduct pairwise differential expression analysis using EdgeR.
    -   Generate comparison tables and volcano plots with customizable significance thresholds.

## Usage

1.  **Setup and Installation:**
    -   Ensure R and required libraries (`shiny`, `edgeR`, `tidyverse`, etc.) are installed.
    -   Clone this repository to your local machine.
2.  **Run the App:**
    -   Open R or RStudio.
    -   Set the working directory to the app directory.
    -   Run `shiny::runApp()` in the R console.
3.  **Usage Instructions:**
    -   Navigate through different tabs for specific functionalities.
    -   Upload files and explore data using the sidebar and main panels.
    -   Follow on-screen instructions for each analysis step.

## Additional Notes

-   Adjust themes and visual styles using `bslib` for customized UI experience.
-   Modify analysis parameters and thresholds directly in the app interface.
-   For detailed technical documentation, refer to the code comments and the respective R packages' documentation.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
