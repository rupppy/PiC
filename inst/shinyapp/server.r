library(shiny)
library(shinyjs)
library(shinyFeedback)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)

# Define the server logic
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^3)
  
  # Cross-platform volume detection
  volumes <- if (.Platform$OS.type == "windows") {
    c(
      Home = fs::path_home(),
      Documents = file.path(fs::path_home(), "Documents"),
      Desktop = file.path(fs::path_home(), "Desktop"),
      getVolumes()()
    )
  } else {
    c(
      Home = fs::path_home(),
      Documents = file.path(fs::path_home(), "Documents"),
      Desktop = file.path(fs::path_home(), "Desktop"),
      Root = "/",
      Temp = tempdir()
    )
  }
  
  # Reactive value to store output files
  output_files <- reactiveVal(NULL)
  current_output_path <- reactiveVal(tempdir())
  
  # Handle directory selection
  observeEvent(input$dir_choose, {
    shinyDirChoose(input, "dir_choose", roots = volumes, session = session)
    if (!is.integer(input$dir_choose)) {
      path <- parseDirPath(volumes, input$dir_choose)
      if (length(path) > 0) {
        updateTextInput(session, "output_path", value = path)
        current_output_path(path)
        # Update file list when directory changes
        update_file_list(path)
      }
    }
  })
  
  # Handle new directory creation
  observeEvent(input$create_dir, {
    showModal(
      modalDialog(
        title = "Create New Folder",
        textInput("new_dir", "New folder name:", placeholder = "E.g., analysis_2025"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_create", "Create", class = "btn-primary")
        )
      )
    )
  })
  
  # Create new directory when confirmed
  observeEvent(input$confirm_create, {
    req(input$new_dir)
    
    # Validate folder name
    if (!grepl("^[a-zA-Z0-9_-]+$", input$new_dir)) {
      showNotification(
        "Folder name can only contain letters, numbers, hyphens and underscores",
        type = "error"
      )
      return()
    }
    
    new_path <- file.path(
      if (nzchar(input$output_path)) input$output_path else fs::path_home(),
      input$new_dir
    )
    
    tryCatch({
      dir.create(new_path, recursive = TRUE)
      updateTextInput(session, "output_path", value = new_path)
      current_output_path(new_path)
      removeModal()
      showNotification(paste("Folder created:", new_path), type = "message")
      # Update file list for new directory
      update_file_list(new_path)
    }, error = function(e) {
      showNotification(paste("Error creating folder:", e$message), type = "error")
    })
  })
  
  # Function to update file list
  update_file_list <- function(path) {
    if (dir.exists(path)) {
      files <- list.files(
        path,
        pattern = "\\.(txt|csv)$",
        full.names = FALSE
      )
      
      if (length(files) > 0) {
        # Sort by modification time (most recent first)
        file_info <- file.info(file.path(path, files))
        files <- files[order(file_info$mtime, decreasing = TRUE)]
        
        output_files(files)
        updateSelectInput(session, "output_file", choices = files, selected = files[1])
      } else {
        output_files(NULL)
        updateSelectInput(session, "output_file", choices = NULL)
      }
    }
  }
  
  # Initialize file list with tempdir
  observe({
    update_file_list(current_output_path())
  })
  
  # Handle function execution
  observeEvent(input$run_function, {
    # Identify selected function
    func_name <- input$function_select
    if (is.null(func_name)) return()
    
    # Collect all parameters in a list
    params <- function_list[[func_name]]$params
    param_values <- list()
    
    # Load file input first
    file_loaded <- FALSE
    
    for (param_name in names(params)) {
      input_name <- paste0("param_", param_name)
      
      if (params[[param_name]]$type == "file") {
        if (!is.null(input[[input_name]])) {
          # Read the file data
          file_data <- tryCatch({
            data.table::fread(input[[input_name]]$datapath, header = FALSE)
          }, error = function(e) {
            showNotification(paste("Error reading file:", e$message), type = "error")
            return(NULL)
          })
          
          if (is.null(file_data)) return()
          
          # Ensure proper column naming
          if (ncol(file_data) >= 3) {
            colnames(file_data)[1:3] <- c("x", "y", "z")
          }
          
          param_values[[param_name]] <- file_data
          file_loaded <- TRUE
        } else {
          showNotification(
            paste("Please upload an input file for", params[[param_name]]$label),
            type = "error"
          )
          return()
        }
      } else if (params[[param_name]]$type == "numeric") {
        param_values[[param_name]] <- as.numeric(input[[input_name]])
        if (is.na(param_values[[param_name]]) || 
            (!is.null(params[[param_name]]$min) && 
             param_values[[param_name]] < params[[param_name]]$min)) {
          showNotification(
            paste(params[[param_name]]$label, "must be at least", params[[param_name]]$min),
            type = "error"
          )
          return()
        }
      } else if (params[[param_name]]$type == "text") {
        param_values[[param_name]] <- input[[input_name]]
        if (params[[param_name]]$required && 
            (is.null(param_values[[param_name]]) || param_values[[param_name]] == "")) {
          showNotification(
            paste(params[[param_name]]$label, "is required"),
            type = "error"
          )
          return()
        }
      } else if (params[[param_name]]$type == "checkbox") {
        param_values[[param_name]] <- input[[input_name]]
      } else if (params[[param_name]]$type == "select") {
        param_values[[param_name]] <- input[[input_name]]
      }
    }
    
    if (!file_loaded) {
      showNotification("No input file loaded", type = "error")
      return()
    }
    
    # CRITICAL: Inject output_path from UI
    output_path <- if (nzchar(input$output_path)) {
      # Normalize path for cross-platform compatibility
      normalizePath(input$output_path, winslash = "/", mustWork = FALSE)
    } else {
      tempdir()
    }
    
    # Verify directory exists and is writable
    if (!dir.exists(output_path)) {
      tryCatch({
        dir.create(output_path, recursive = TRUE)
      }, error = function(e) {
        showNotification(
          paste("Cannot create output directory:", e$message),
          type = "error"
        )
        return()
      })
    }
    
    # Test write permissions
    test_file <- file.path(output_path, ".write_test")
    can_write <- tryCatch({
      writeLines("test", test_file)
      file.remove(test_file)
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    if (!can_write) {
      showNotification(
        paste("Cannot write to directory:", output_path),
        type = "error"
      )
      return()
    }
    
    param_values[["output_path"]] <- output_path
    current_output_path(output_path)
    
    # Execute function with collected parameters
    tryCatch({
      withProgress(message = 'Running analysis...', value = 0, {
        incProgress(0.1, detail = "Initializing...")
        
        # Execute selected function with parameters
        result <- do.call(func_name, param_values)
        
        incProgress(0.8, detail = "Finalizing...")
        
        # Update file list after execution
        Sys.sleep(0.5)  # Brief delay to ensure files are written
        update_file_list(output_path)
        
        incProgress(1.0, detail = "Complete!")
        
        # Show success notification with output location
        showNotification(
          paste0("Analysis completed successfully!\nResults saved to: ", output_path),
          type = "message",
          duration = 5
        )
        
        # Display results
        output$results <- renderPrint({
          if (!is.null(result)) {
            if (is.data.frame(result)) {
              cat("Analysis Results:\n")
              print(result)
            } else if (is.list(result)) {
              cat("Analysis completed. Generated files:\n")
              if ("tree_report" %in% names(result)) {
                cat("\n- Individual tree report:", basename(result$tree_report))
              }
              if ("plot_report" %in% names(result)) {
                cat("\n- Study area summary:", basename(result$plot_report))
              }
              if ("forest_floor" %in% names(result)) {
                cat("\n- Forest floor:", basename(result$forest_floor))
              }
              if ("wood" %in% names(result)) {
                cat("\n- Wood points:", basename(result$wood))
              }
              if ("foliage" %in% names(result)) {
                cat("\n- Foliage points:", basename(result$foliage))
              }
              cat("\n\nAll files saved to:", output_path)
            } else {
              print(result)
            }
          }
        })
      })
    }, error = function(e) {
      showNotification(
        paste("Analysis error:", e$message),
        type = "error",
        duration = 10
      )
      print(e)
    })
  })
  
  # Display function description
  output$function_description <- renderText({
    function_list[[input$function_select]]$description
  })
  
  # Generate dynamic input fields based on selected function
  output$dynamic_inputs <- renderUI({
    req(input$function_select)
    params <- function_list[[input$function_select]]$params
    
    input_list <- lapply(names(params), function(param_name) {
      param <- params[[param_name]]
      if (param$type == "file") {
        fileInput(
          inputId = paste0("param_", param_name),
          label = param$label,
          accept = c(".txt", ".xyz", ".csv", ".asc"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        )
      } else if (param$type == "numeric") {
        numericInput(
          inputId = paste0("param_", param_name),
          label = param$label,
          value = param$default,
          min = if (!is.null(param$min)) param$min else NA,
          max = if (!is.null(param$max)) param$max else NA,
          step = if (!is.null(param$step)) param$step else 1
        )
      } else if (param$type == "text") {
        textInput(
          inputId = paste0("param_", param_name),
          label = param$label,
          value = param$default,
          placeholder = if (!is.null(param$placeholder)) param$placeholder else ""
        )
      } else if (param$type == "checkbox") {
        checkboxInput(
          inputId = paste0("param_", param_name),
          label = param$label,
          value = param$default
        )
      } else if (param$type == "select") {
        selectInput(
          inputId = paste0("param_", param_name),
          label = param$label,
          choices = param$choices,
          selected = param$default
        )
      }
    })
    
    do.call(tagList, input_list)
  })
  
  # Validate filename input
  observe({
    if (!is.null(input$param_filename) && nzchar(input$param_filename)) {
      valid <- grepl("^[a-zA-Z0-9_-]+$", input$param_filename)
      shinyFeedback::feedbackWarning(
        "param_filename",
        !valid,
        "Use only letters, numbers, hyphens and underscores"
      )
    }
  })
  
  # Preview filename pattern
  output$filename_preview <- renderText({
    params <- function_list[[input$function_select]]$params
    if (!"filename" %in% names(params)) return(NULL)
    
    req(input$param_filename)
    
    if (nzchar(input$param_filename)) {
      paste("Files will be saved as:", paste0(input$param_filename, "_*.txt/csv"))
    } else {
      "Files will be saved with default prefix"
    }
  })
  
  # Display current output directory
  output$current_output_dir <- renderText({
    path <- if (nzchar(input$output_path)) {
      input$output_path
    } else {
      tempdir()
    }
    paste("Current output directory:", path)
  })
  
  # Render 3D point cloud visualization
  output$point_cloud <- renderPlotly({
    req(input$output_file, output_files())
    
    file_path <- file.path(current_output_path(), input$output_file)
    
    validate(
      need(file.exists(file_path), "File not found"),
      need(file.size(file_path) > 0, "File is empty")
    )
    
    # Load and process data
    data <- tryCatch({
      df <- data.table::fread(file_path, header = FALSE)
      
      if (ncol(df) >= 3) {
        colnames(df)[1:3] <- c("x", "y", "z")
        df <- df[, 1:3]
      } else {
        stop("File must contain at least 3 columns (x, y, z)")
      }
      
      # Subsample for performance if needed
      if (nrow(df) > 1e5) {
        set.seed(123)
        df <- df[sample(.N, 1e5)]
        showNotification(
          "Displaying 100,000 randomly sampled points for performance",
          type = "warning",
          duration = 3
        )
      }
      
      df
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
    
    validate(need(!is.null(data), "Unable to load data"))
    
    # Create 3D scatter plot
    plot_ly(
      data,
      x = ~x,
      y = ~y,
      z = ~z,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 2,
        color = ~z,
        colorscale = "Viridis",
        colorbar = list(title = "Height (m)"),
        opacity = 0.8,
        line = list(color = "rgba(0,0,0,0.3)", width = 0.5)
      )
    ) %>%
      layout(
        title = list(
          text = paste("Point Cloud Visualization:", input$output_file),
          font = list(size = 14)
        ),
        autosize = TRUE,
        scene = list(
          aspectmode = "data",
          camera = list(eye = list(x = 1.5, y = 1.5, z = 0.5)),
          xaxis = list(title = "X (m)", gridcolor = "lightgray"),
          yaxis = list(title = "Y (m)", gridcolor = "lightgray"),
          zaxis = list(title = "Z (m)", gridcolor = "lightgray")
        ),
        margin = list(l = 0, r = 0, b = 0, t = 40)
      )
  })
  
  # Manual refresh button for file list
  observeEvent(input$refresh_files, {
    update_file_list(current_output_path())
    showNotification("File list refreshed", type = "message", duration = 2)
  })
}