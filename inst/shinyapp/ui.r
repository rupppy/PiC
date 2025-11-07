library(shiny)
library(shinyjs)
library(shinyFeedback)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)

# Define the UI layout
app_ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  tags$head(
    tags$style(HTML("
      .title-container {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 10px;
        background-color: #f8f9fa;
        border-bottom: 2px solid #dee2e6;
      }
      .logo {
        max-height: 60px;
        margin: 0 20px;
      }
      .title-content {
        flex-grow: 1;
        text-align: center;
      }
      .info-box {
        background-color: #e7f3ff;
        border-left: 4px solid #2196F3;
        padding: 10px;
        margin: 10px 0;
        border-radius: 4px;
      }
      .output-path-display {
        background-color: #f5f5f5;
        padding: 8px;
        border-radius: 4px;
        margin: 10px 0;
        font-family: monospace;
        font-size: 0.9em;
        word-break: break-all;
      }
      .btn-refresh {
        margin-left: 5px;
      }
    "))
  ),
  
  # Header with logos and title
  div(class = "title-container",
      tags$img(src = "logo_ibe.jpg", class = "logo", alt = "IBE Logo"),
      div(class = "title-content",
          titlePanel("PiC - Pointcloud Interactive Computation"),
          tags$p("Forest Structure Analysis from Terrestrial LiDAR Data", 
                 style = "color: #666; margin: 0;")
      ),
      tags$img(src = "logo_pic.jpg", class = "logo", alt = "PiC Logo")
  ),
  
  # Main layout with sidebar and main panel
  sidebarLayout(
    # Sidebar Panel
    sidebarPanel(
      width = 3,
      
      # Function selection
      h4("Analysis Configuration"),
      selectInput(
        "function_select",
        "Select analysis function:",
        choices = names(function_list)
      ),
      
      # Dynamic parameter inputs
      uiOutput("dynamic_inputs"),
      
      hr(),
      
      # Output directory configuration
      h4("Output Configuration"),
      div(class = "info-box",
          tags$small("Select where to save results. Leave empty to use temporary folder.")
      ),
      
      fluidRow(
        column(10, shinyDirButton(
          "dir_choose",
          "Choose folder",
          "Select output directory",
          icon = icon("folder-open")
        )),
        column(2, actionButton(
          "create_dir",
          "",
          icon = icon("folder-plus"),
          title = "Create new folder"
        ))
      ),
      
      textInput(
        "output_path",
        "Output path:",
        placeholder = "Or enter path manually"
      ),
      
      div(class = "output-path-display",
          textOutput("current_output_dir")
      ),
      
      textOutput("filename_preview"),
      
      hr(),
      
      # Run button
      actionButton(
        "run_function",
        "Run Analysis",
        class = "btn-primary btn-lg btn-block",
        icon = icon("play")
      )
    ),
    
    # Main Panel
    mainPanel(
      width = 9,
      
      # Function description
      div(class = "info-box",
          h4("Function Description"),
          textOutput("function_description")
      ),
      
      # Results section
      h4("Analysis Results"),
      verbatimTextOutput("results"),
      
      hr(),
      
      # Visualization section
      h4("3D Point Cloud Visualization"),
      
      fluidRow(
        column(10,
               selectInput(
                 "output_file",
                 "Select file to display:",
                 choices = NULL,
                 width = "100%"
               )
        ),
        column(2,
               actionButton(
                 "refresh_files",
                 "Refresh",
                 icon = icon("sync"),
                 class = "btn-refresh",
                 title = "Refresh file list"
               )
        )
      ),
      
      div(class = "info-box",
          tags$small(
            "Note: Visualization is limited to 100,000 points for performance. ",
            "Large files will be randomly subsampled."
          )
      ),
      
      withSpinner(
        plotlyOutput("point_cloud", height = "600px"),
        type = 8,
        color = "#2196F3"
      ),
      
      br(),
      
      # Footer information
      hr(),
      div(
        style = "text-align: center; color: #666; padding: 20px;",
        tags$p("PiC - Pointcloud Interactive Computation"),
        tags$p(
          tags$small(
            "For forest structure analysis from terrestrial laser scanning data | ",
            "Version 1.2.5"
          )
        )
      )
    )
  )
)