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
                    "Upload Metadata .csv File (containing file names and grouping information",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          
          # Input: Select separator ----
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("metadata_contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$metadata_contents <- renderTable({
      req(input$file1)
      
      df <- read_csv(
        input$file1$datapath,
        col_names = TRUE,
      )
      head(df)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
