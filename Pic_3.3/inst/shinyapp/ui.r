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
      .warning-box {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
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
      .advanced-params-box {
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 15px;
        margin-top: 10px;
      }
      .advanced-header {
        font-weight: bold;
        color: #2196F3;
        font-size: 1.1em;
        margin-bottom: 5px;
      }
      .conditional-section {
        background-color: #f0f8ff;
        border: 1px solid #b3d9ff;
        border-radius: 4px;
        padding: 12px;
        margin-top: 8px;
        margin-left: 15px;
      }
      .exit-button-container {
        margin: 20px 0;
        padding: 15px;
        border-top: 2px solid #dee2e6;
      }
      .btn-exit {
        min-width: 180px;
        font-weight: 500;
      }
    "))
  ),
  
  # Header with logos and title
  div(class = "title-container",
      tags$img(src = "logo_ibe.png", class = "logo", alt = "IBE Logo"),
      div(class = "title-content",
          titlePanel("PiC - Pointcloud Interactive Computation"),
          tags$p("Forest Structure Analysis from Ground-Based LiDAR Data", 
                 style = "color: #666; margin: 0;")
      ),
      tags$img(src = "logo_pic.png", class = "logo", alt = "PiC Logo")
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
      
      # ===== CONDITIONAL PARAMETER FILE LOADER (Forest_seg only) =====
      conditionalPanel(
        condition = "input.function_select == 'Forest_seg'",
        
        # Parameter file loader
        div(class = "info-box",
            style = "background-color: #e8f5e9; border-left: 4px solid #4caf50;",
            tags$small(
              tags$strong("💡 Quick Setup:"),
              " Load parameters from a previous analysis log or template file"
            )
        ),
        
        div(
          style = "width: 100%;",
          shinyFilesButton(
            "param_file",
            label = "Load Parameters...",
            title = "Select a parameter file",
            multiple = FALSE,
            icon = icon("file-alt")
          ),
          textOutput("param_file_name_display", inline = TRUE)
        ),
        
        # Visual feedback when params loaded
        uiOutput("param_load_status")
      ),
      # ===== END CONDITIONAL PARAMETER LOADER =====
      
      hr(),
      
      # ===== SHARED FILE INPUT - Always Visible =====
      h4("Point Cloud File"),
      div(class = "info-box",
          tags$small(
            "Select your point cloud file (.xyz, .txt, .csv, .las, .laz). ",
            "Only the file path is stored - no upload needed."
          )
      ),
      
      div(
        style = "width: 100%;",
        shinyFilesButton(
          "shared_file_input",
          label = "Browse...",
          title = "Select a point cloud file",
          multiple = FALSE,
          icon = icon("file")
        ),
        textOutput("shared_file_name_display", inline = TRUE)
      ),
      
      # Visual feedback when file loaded
      uiOutput("file_status"),
      
      hr(),
      # ===== END SHARED FILE INPUT =====
      
      # Dynamic parameter inputs (base parameters only)
      uiOutput("dynamic_inputs"),
      
      # ===== WORKFLOW OPTIONS - Forest_seg only =====
      conditionalPanel(
        condition = "input.function_select == 'Forest_seg'",
        
        hr(),
      
      # WORKFLOW OPTIONS - Hierarchical structure
      h4("Workflow Options"),
      
      div(class = "info-box",
          tags$small(
            tags$strong("Control what the analysis generates:"),
            tags$br(),
            "• Segmentation files are always created",
            tags$br(),
            "• Enable Forestry Reports for detailed tree and plot metrics",
            tags$br(),
            "• CBH and Canopy require Forestry Reports"
          )
      ),
      
      # Level 1: Generate Forestry Reports (master switch)
      checkboxInput(
        "generate_reports",
        label = tags$span(
          tags$strong("Generate Forestry Reports"),
          tags$br(),
          tags$small("Creates tree_report.csv (individual trees) and plot_report.csv (summary statistics)")
        ),
        value = TRUE
      ),
      
      # Level 2: Conditional options (shown only if generate_reports = TRUE)
      conditionalPanel(
        condition = "input.generate_reports == true",
        div(
          class = "conditional-section",
          
          # CBH Option
          checkboxInput(
            "calculate_cbh",
            label = tags$span(
              tags$strong("Calculate Crown Base Height (CBH)"),
              tags$br(),
              tags$small("GAB Voronoi method: hexagonal tessellation + BFS connectivity")
            ),
            value = TRUE
          ),
          
          # Canopy Analysis Option
          checkboxInput(
            "analyze_canopy",
            label = tags$span(
              tags$strong("Analyze Canopy Structure"),
              tags$br(),
              tags$small("Adds canopy metrics to plot_report (volume, coverage, vertical profile)")
            ),
            value = TRUE
          ),
          
        )
      ),
      
      # Warning when reports are disabled
      conditionalPanel(
        condition = "input.generate_reports == false",
        div(
          class = "warning-box",
          tags$small(
            tags$strong("⚠️ Forestry Reports disabled:"),
            tags$br(),
            "Only segmentation files will be saved (forest floor, wood, foliage).",
            tags$br(),
            "No tree reports, DBH, heights, CBH or canopy analysis will be calculated."
          )
        )
      )
      ), # End conditionalPanel for Workflow Options
      # ===== END WORKFLOW OPTIONS =====
      
      hr(),
      
      # Advanced parameters section (for segmentation parameters)
      div(id = "advanced_section",
          actionButton(
            "toggle_advanced",
            "Show Advanced Parameters",
            icon = icon("chevron-down"),
            class = "btn-info btn-block",
            style = "margin-bottom: 10px;"
          ),
          uiOutput("advanced_inputs")
      ),
      
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
      ),
      
      br(),
      
      # Exit info box
      div(
        style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 8px; margin-top: 10px; border-radius: 4px;",
        tags$small(
          tags$strong("🛑 To stop the server: "),
          "Click 'Exit Application' button at page bottom, or press Ctrl+C / Stop button in RStudio."
        )
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
      
      # ====================================================================
      # DIAGNOSTIC PLOTS SECTION (for Analyze_cloud)
      # ====================================================================
      
      conditionalPanel(
        condition = "input.function_select == 'Analyze_cloud'",
        
        hr(),
        
        uiOutput("analyze_plots_ui")
      ),
      
      # ====================================================================
      # END DIAGNOSTIC PLOTS SECTION
      # ====================================================================
      # Footer with Exit button
      hr(),
      div(
        class = "exit-button-container",
        style = "text-align: center;",
        
        # Exit button - STOPS THE SERVER
        actionButton(
          "exit_app",
          "Exit Application",
          icon = icon("sign-out-alt"),
          class = "btn-danger btn-exit"
        ),
        
        tags$p(
          style = "margin-top: 10px; color: #999; font-size: 0.85em;",
          tags$small(
            tags$strong("This stops the server."),
            " Closing browser only disconnects the interface (server keeps running)."
          )
        )
      ),
      
      # Version info
      div(
        style = "text-align: center; color: #666; padding: 20px;",
        tags$p("PiC - Pointcloud Interactive Computation"),
        tags$p(
          tags$small(
            "For forest structure analysis from ground-based laser scanning data | ",
            "Version 3.3"
          )
        ),
        tags$p(
          tags$small("by Roberto Ferrara - CNR - IBE - Sassari")
        )
      )
    )   
  ) 
)
