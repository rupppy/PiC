library(shiny)
library(shinyjs)
library(shinyFeedback)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)


app_server <- function(input, output, session) {
  
  # Load pic_analyze_cloud function
  pic_analyze_cloud_paths <- c(
    "pic_analyze_cloud.R",
    "R/pic_analyze_cloud.R",
    file.path(dirname(getwd()), "pic_analyze_cloud.R")
  )
  
  for (path in pic_analyze_cloud_paths) {
    if (file.exists(path)) {
      message("Loading pic_analyze_cloud from: ", path)
      tryCatch({
        source(path, local = FALSE)
        message("✓ pic_analyze_cloud loaded successfully")
        break
      }, error = function(e) {
        warning("Failed to load ", path, ": ", e$message)
      })
    }
  }
  
  if (!exists("pic_analyze_cloud", mode = "function")) {
    message("⚠ pic_analyze_cloud function not found!")
    message("  Place pic_analyze_cloud.R in the same directory as server.R")
  }
  
  # ===== SESSION HANDLERS =====
  session$onSessionEnded(function() {
    message("PiC app session ended cleanly")
  })
  
  # ===== EXIT BUTTON =====
  observeEvent(input$exit_app, {
    showModal(
      modalDialog(
        title = "Exit PiC Application",
        div(
          tags$p("This will stop the Shiny server and close the application."),
          tags$p(
            style = "color: #856404; background-color: #fff3cd; padding: 8px; border-radius: 4px; margin-top: 10px;",
            tags$strong("Note: "), 
            "Simply closing the browser tab will disconnect the interface ",
            "but the server will keep running. Use this button to stop it completely."
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_exit", "Stop Server & Exit", class = "btn-danger")
        )
      )
    )
  })
  
  observeEvent(input$confirm_exit, {
    message("User confirmed app exit - stopping server cleanly")
    removeModal()
    # Use isolate to avoid reactive context errors and schedule stopApp
    isolate({
      session$onFlushed(function() {
        stopApp()
      }, once = TRUE)
    })
  })
  
  # ===== CROSS-PLATFORM VOLUMES =====

  volumes <- get_system_volumes()

  # ===== FILE CHOOSER REGISTRATION (shinyFiles) =====
  shinyFileChoose(input, "shared_file_input", roots = volumes, session = session,
                  filetypes = c("txt", "xyz", "csv", "asc", "las", "laz"))

  shinyFileChoose(input, "param_file", roots = volumes, session = session,
                  filetypes = c("txt"))

  # ===== REACTIVE VALUES =====
  output_files <- reactiveVal(NULL)
  current_output_path <- reactiveVal(tempdir())
  
  # Point cloud memory management
  loaded_cloud <- reactiveVal(NULL)           # Stores loaded point cloud data
  loaded_cloud_info <- reactiveVal(NULL)      # Stores metadata (filename, n_points, etc.)
  analysis_results <- reactiveVal(NULL)       # Stores pic_analyze_cloud results
  
  # Cache for visualization to avoid re-reading same file
  viz_cache <- reactiveValues(
    file_path = NULL,
    data = NULL
  )

  # ===== PARSED FILE PATHS (shinyFiles) =====
  # Reactive wrappers that return list(datapath, name) or NULL
  selected_file <- reactive({
    req(input$shared_file_input)
    if (is.integer(input$shared_file_input)) return(NULL)
    parsed <- parseFilePaths(volumes, input$shared_file_input)
    if (nrow(parsed) == 0) return(NULL)
    list(datapath = as.character(parsed$datapath), name = as.character(parsed$name))
  })

  selected_param_file <- reactive({
    req(input$param_file)
    if (is.integer(input$param_file)) return(NULL)
    parsed <- parseFilePaths(volumes, input$param_file)
    if (nrow(parsed) == 0) return(NULL)
    list(datapath = as.character(parsed$datapath), name = as.character(parsed$name))
  })

  # Display selected filenames
  output$shared_file_name_display <- renderText({
    sf <- selected_file()
    if (is.null(sf)) return("No file selected")
    sf$name
  })

  output$param_file_name_display <- renderText({
    spf <- selected_param_file()
    if (is.null(spf)) return("No parameter file selected")
    spf$name
  })

  # ===== ADVANCED PARAMETERS TOGGLE =====
  observeEvent(input$toggle_advanced, {
    shinyjs::toggle("advanced_params_container", anim = TRUE, animType = "slide")
    
    current_visible <- input$toggle_advanced %% 2 == 1
    if (current_visible) {
      shinyjs::html("toggle_advanced", 
                    HTML('<i class="fa fa-chevron-up"></i> Hide Advanced Parameters'))
    } else {
      shinyjs::html("toggle_advanced", 
                    HTML('<i class="fa fa-chevron-down"></i> Show Advanced Parameters'))
    }
  })
  
  # ===== DIRECTORY SELECTION =====
  observeEvent(input$dir_choose, {
    shinyDirChoose(input, "dir_choose", roots = volumes, session = session)
    if (!is.integer(input$dir_choose)) {
      path <- parseDirPath(volumes, input$dir_choose)
      if (length(path) > 0) {
        updateTextInput(session, "output_path", value = path)
        current_output_path(path)
        update_file_list(path)
      }
    }
  })
  
  # ===== DIRECTORY CREATION =====
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
  
  observeEvent(input$confirm_create, {
    req(input$new_dir)
    
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
      update_file_list(new_path)
    }, error = function(e) {
      showNotification(paste("Error creating folder:", e$message), type = "error")
    })
  })
  
  # ===== FILE LIST MANAGEMENT =====
  update_file_list <- function(path) {
    if (dir.exists(path)) {
      files <- list.files(path, pattern = "\\.(txt|csv)$", full.names = FALSE)
      
      # Exclude parameter log files from visualization (they contain comments, not point clouds)
      files <- files[!grepl("_parameters_", files, fixed = TRUE)]
      
      if (length(files) > 0) {
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
  
  observe({
    update_file_list(current_output_path())
  })
  
  observeEvent(input$refresh_files, {
    update_file_list(current_output_path())
    showNotification("File list refreshed", type = "message", duration = 2)
  })
  

  # ============================================================================
  # PARAMETER FILE LOADER - 
  # ============================================================================
  
  # Reactive value to store parameters from file
  params_to_apply <- reactiveVal(NULL)
  
  # Step 1: Load and parse file
  observeEvent(selected_param_file(), {
    spf <- selected_param_file()
    req(spf)

    params <- parse_parameter_file(spf$datapath)

    if (is.null(params) || length(params) == 0) {
      showNotification(
        "Could not parse parameter file",
        type = "error",
        duration = 5
      )
      return()
    }

    # Store for application
    params_to_apply(list(
      params = params,
      filename = basename(spf$name),
      timestamp = Sys.time(),
      attempts = 0
    ))

    showNotification(
      paste("\U0001F4C1 Loaded", length(params), "parameters from", basename(spf$name)),
      type = "message",
      duration = 3
    )
  })
  
  # Step 2: Apply parameters when inputs are ready (WITH RETRY)
  observeEvent(params_to_apply(), {
    req(params_to_apply())
    
    params_data <- params_to_apply()
    params <- params_data$params
    attempts <- params_data$attempts
    
    # Safety: max 10 attempts (2 seconds total)
    if (attempts >= 10) {
      showNotification(
        "❌ Could not apply parameters - UI inputs not ready after 2 seconds",
        type = "error",
        duration = 5
      )
      params_to_apply(NULL)
      return()
    }
    
    # =========================================================================
    # FIX: Check for dynamic input existence
    # =========================================================================
    
    # Workflow parameters (defined directly in ui.R, no "param_" prefix)
    workflow_params <- c("generate_reports", "calculate_cbh", "analyze_canopy")
    
    # Special parameters (handled separately in UI, not in dynamic inputs)
    special_params <- c("output_path", "a")  # ← ADDED
    
    # Get function definition to check parameter types
    func_def <- function_list[[input$function_select]]
    
    if (is.null(func_def)) {
      params_to_apply(NULL)
      return()
    }
    
    # Find first parameter that should be a dynamic input (not file, not workflow, not special)
    test_param <- NULL
    for (pname in names(params)) {
      # Skip workflow params (they're always in UI)
      if (pname %in% workflow_params) next
      
      # Skip special params (handled separately)
      if (pname %in% special_params) next  # ← ADDED
      
      # Get parameter definition from function_list
      param_def <- func_def$params[[pname]]
      
      # Check if it's a dynamic parameter (not file type)
      if (!is.null(param_def) && param_def$type != "file") {
        test_param <- pname
        break
      }
    }
    
    # If we have dynamic params, check if UI is ready
    if (!is.null(test_param)) {
      test_input_id <- paste0("param_", test_param)
      
      # Check if test input exists in session
      if (!test_input_id %in% names(input)) {
        # Input not ready yet - RETRY in 200ms
        params_data$attempts <- attempts + 1
        params_to_apply(params_data)
        invalidateLater(200, session)
        return()
      }
    }
    
    # ✓ OK, inputs exist (or no dynamic inputs needed) - apply parameters
    
    # =========================================================================
    # FIX: Apply parameters with special handling
    # =========================================================================
    
    loaded_count <- 0
    skipped_params <- character()
    failed_params <- character()
    
    for (param_name in names(params)) {
      param_value <- params[[param_name]]
      
      # =====================================================================
      # SPECIAL HANDLING FOR output_path
      # =====================================================================
      if (param_name == "output_path") {
        tryCatch({
          updateTextInput(session, "output_path", value = param_value)
          current_output_path(param_value)  # Update reactive value
          loaded_count <- loaded_count + 1
        }, error = function(e) {
          failed_params <- c(failed_params, param_name)
        })
        next  # Skip to next parameter
      }
      
      # =====================================================================
      # SKIP file input 'a' (cannot be set programmatically)
      # =====================================================================
      if (param_name == "a") {
        skipped_params <- c(skipped_params, param_name)
        next
      }
      
      # =====================================================================
      # REGULAR PARAMETERS
      # =====================================================================
      
      # Determine input ID based on parameter category
      if (param_name %in% workflow_params) {
        # Workflow checkboxes: no prefix
        input_id <- param_name
      } else {
        # Dynamic parameters: "param_" prefix
        input_id <- paste0("param_", param_name)
      }
      
      # Check if input exists
      if (!input_id %in% names(input)) {
        skipped_params <- c(skipped_params, param_name)
        next
      }
      
      # Apply update based on value type
      tryCatch({
        
        if (is.logical(param_value)) {
          # Boolean → Checkbox
          updateCheckboxInput(session, input_id, value = param_value)
          loaded_count <- loaded_count + 1
          
        } else if (is.numeric(param_value)) {
          # Number → Numeric input
          updateNumericInput(session, input_id, value = param_value)
          loaded_count <- loaded_count + 1
          
        } else if (is.character(param_value)) {
          # String → Text or Select input
          # Try as select first, fallback to text
          tryCatch({
            updateSelectInput(session, input_id, selected = param_value)
            loaded_count <- loaded_count + 1
          }, error = function(e_select) {
            # Not a select, try as text input
            tryCatch({
              updateTextInput(session, input_id, value = param_value)
              loaded_count <- loaded_count + 1
            }, error = function(e_text) {
              # Neither select nor text worked
              failed_params <<- c(failed_params, param_name)
            })
          })
        }
        
      }, error = function(e) {
        failed_params <- c(failed_params, param_name)
        warning(paste("Failed to apply", param_name, ":", e$message))
      })
    }
    
    # =========================================================================
    # Build result message with detailed feedback
    # =========================================================================
    
    msg_parts <- c(
      paste("✓ Applied", loaded_count, "of", length(params), "parameters")
    )
    
    # Show skipped parameters (not found in UI)
    if (length(skipped_params) > 0) {
      # Filter out expected skips
      unexpected_skips <- setdiff(skipped_params, c("a"))  # 'a' is expected
      
      if (length(unexpected_skips) > 0) {
        if (length(unexpected_skips) <= 5) {
          msg_parts <- c(msg_parts, 
                         paste("⚠ Not found in UI:", paste(unexpected_skips, collapse = ", ")))
        } else {
          msg_parts <- c(msg_parts, 
                         paste("⚠", length(unexpected_skips), "parameters not found in UI"))
        }
      }
    }
    
    # Show failed parameters (found but update failed)
    if (length(failed_params) > 0) {
      msg_parts <- c(msg_parts,
                     paste("Failed to update:", paste(failed_params, collapse = ", ")))
    }
    
    # Show notification with appropriate type
    notification_type <- if (loaded_count > 0 && length(failed_params) == 0) {
      "message"
    } else if (loaded_count > 0) {
      "warning"
    } else {
      "error"
    }
    
    showNotification(
      paste(msg_parts, collapse = "\n"),
      type = notification_type,
      duration = 8
    )
    
    # Clear to avoid reapplying
    params_to_apply(NULL)
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Visual feedback for loaded parameters
  output$param_load_status <- renderUI({
    spf <- selected_param_file()
    req(spf)

    div(
      style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;
               padding: 8px; margin: 10px 0; font-size: 0.9em;",
      icon("check-circle", style = "color: #28a745;"),
      strong(" Parameters file loaded: "),
      basename(spf$name)
    )
  })
  
  # Visual feedback for selected file
  output$file_status <- renderUI({
    sf <- selected_file()
    req(sf)

    file_size_mb <- file.size(sf$datapath) / (1024^2)

    div(
      style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;
               padding: 8px; margin: 10px 0; font-size: 0.9em;",
      icon("check-circle", style = "color: #28a745;"),
      strong(" File selected: "),
      sf$name,
      tags$br(),
      tags$small(sprintf("Size: %.1f MB | Path: %s", file_size_mb, sf$datapath))
    )
  })
  

  # ============================================================================
  # MAIN FUNCTION EXECUTION - MEMORY OPTIMIZED
  # ============================================================================
  # Key optimization: Pass file PATH to functions, not data
  # File is read ONLY ONCE inside .coerce_to_xyz() in Forest_seg()
  # ============================================================================
  
  observeEvent(input$run_function, {
    
    # ========================================================================
    # STEP 1: COLLECT PARAMETERS
    # ========================================================================
    
    func_name <- input$function_select
    func_def <- function_list[[func_name]]
    
    if (is.null(func_def)) {
      showNotification("Function not found", type = "error")
      return()
    }
    
    # Get output path
    output_path <- if (nzchar(input$output_path)) {
      input$output_path
    } else {
      tempdir()
    }
    
    # Create output directory if needed
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Collect parameter values
    param_values <- list()
    
    for (param_name in names(func_def$params)) {
      param_def <- func_def$params[[param_name]]
      
      # Workflow parameters (no "param_" prefix)
      if (param_name %in% c("generate_reports", "calculate_cbh", "analyze_canopy")) {
        if (!is.null(input[[param_name]])) {
          param_values[[param_name]] <- input[[param_name]]
        } else {
          param_values[[param_name]] <- param_def$default
        }
      }
      # File input (use shared file input if available)
      else if (param_def$type == "file") {
        sf <- selected_file()
        if (!is.null(sf)) {
          param_values[[param_name]] <- sf$datapath
        } else {
          # Fallback to function-specific input (shinyFiles)
          input_id <- paste0("param_", param_name)
          val <- input[[input_id]]
          if (!is.null(val) && !is.integer(val)) {
            parsed <- parseFilePaths(volumes, val)
            if (nrow(parsed) > 0) param_values[[param_name]] <- as.character(parsed$datapath)
          }
        }
      }
      # Regular parameters (with "param_" prefix)
      else {
        input_id <- paste0("param_", param_name)
        if (!is.null(input[[input_id]])) {
          param_values[[param_name]] <- input[[input_id]]
        } else {
          param_values[[param_name]] <- param_def$default
        }
      }
    }
    
    # Add output_path
    param_values[["output_path"]] <- output_path
    
    # ========================================================================
    # STEP 2: VALIDATE REQUIRED PARAMETERS
    # ========================================================================
    
    # Check if file input is required and provided
    for (param_name in names(func_def$params)) {
      param_def <- func_def$params[[param_name]]
      if (param_def$type == "file" && isTRUE(param_def$required)) {
        if (is.null(selected_file())) {
          showNotification(
            "Please select a point cloud file",
            type = "error",
            duration = 5
          )
          return()
        }
      }
    }
    
    # ========================================================================
    # STEP 3: SPECIAL HANDLING FOR ANALYZE_CLOUD
    # ========================================================================
    
    if (func_name == "Analyze_cloud") {

      sf <- selected_file()
      req(sf)  # Require file selection

      # Parse voxel sizes
      voxel_sizes_input <- gsub("\\s", "", param_values[["voxel_sizes"]])
      voxel_sizes <- as.numeric(strsplit(voxel_sizes_input, ",")[[1]])

      if (length(voxel_sizes) == 0 || any(is.na(voxel_sizes))) {
        showNotification(
          "Invalid voxel sizes. Use comma-separated values (e.g., 0.02, 0.05, 0.1)",
          type = "error"
        )
        return()
      }

      tryCatch({
        withProgress(message = 'Analyzing point cloud...', value = 0, {

          incProgress(0.1, detail = "Loading point cloud...")

          # Load cloud data (keep only first 3 columns for x, y, z)
          file_ext <- tolower(tools::file_ext(sf$name))
          if (file_ext %in% c("las", "laz")) {
            las <- lidR::readLAS(sf$datapath, select = "xyz")
            cloud <- data.table::as.data.table(las@data[, .(X, Y, Z)])
            data.table::setnames(cloud, c("X", "Y", "Z"), c("x", "y", "z"))
          } else {
            cloud <- data.table::fread(sf$datapath)
            cloud <- cloud[, 1:3]  # Keep only first 3 columns
            colnames(cloud) <- c("x", "y", "z")
          }

          # Store in memory for subsequent Forest_seg use
          loaded_cloud(cloud)
          loaded_cloud_info(list(
            filename = sf$name,
            n_points = nrow(cloud),
            loaded_at = Sys.time()
          ))
          
          incProgress(0.3, detail = "Running diagnostic analysis...")
          
          # Run pic_analyze_cloud
          result <- pic_analyze_cloud(
            points = cloud,
            voxel_sizes = voxel_sizes,
            generate_pdf = param_values[["generate_pdf"]],
            pdf_output = if (param_values[["generate_pdf"]]) {
              file.path(output_path, param_values[["pdf_filename"]])
            } else NULL,
            output_path = output_path
          )
          
          # Store results for visualization
          analysis_results(result)
          
          incProgress(0.9, detail = "Finalizing...")
          
          # Update file list if PDF was generated
          if (!is.null(result$pdf_file)) {
            Sys.sleep(0.5)
            update_file_list(output_path)
          }
          
          incProgress(1.0, detail = "Complete!")
          
          showNotification(
            HTML(paste0(
              "<strong>✓ Point cloud analyzed and loaded in memory</strong><br>",
              "• Points: ", format(result$summary$n_points, big.mark = ","), "<br>",
              "• Ready for Forest_seg analysis<br>",
              if (!is.null(result$pdf_file)) paste0("• PDF saved: ", basename(result$pdf_file))
            )),
            type = "message", 
            duration = 8
          )
          
          # Display text results
          output$results <- renderPrint({
            cat(paste(result$text_output, collapse = ""))
          })
          
        })
        
      }, error = function(e) {
        showNotification(
          paste("Error in analysis:", e$message),
          type = "error",
          duration = 10
        )
        print(e)
      })
      
      return()  # Exit handler for Analyze_cloud
    }
    
    # ========================================================================
    # STEP 4: SPECIAL HANDLING FOR FOREST_SEG WITH LOADED CLOUD
    # ========================================================================
    
    if (func_name == "Forest_seg") {
      
      # Check if we have a loaded cloud in memory
      if (!is.null(loaded_cloud())) {
        
        message("Using previously loaded cloud from Analyze_cloud")
        message("  Points: ", nrow(loaded_cloud()))
        message("  Loaded from: ", loaded_cloud_info()$filename)
        
        # Replace file input parameter with in-memory data
        param_values[["a"]] <- loaded_cloud()
        
        showNotification(
          HTML(paste0(
            "<strong>Using cloud from memory</strong><br>",
            "Previously loaded: ", loaded_cloud_info()$filename, "<br>",
            "Points: ", format(loaded_cloud_info()$n_points, big.mark = ",")
          )),
          type = "message",
          duration = 5
        )
      }
    }
    
    # ========================================================================
    # STEP 5: EXECUTE FUNCTION
    # ========================================================================
    
    tryCatch({
      
      withProgress(message = paste('Running', func_name, '...'), value = 0, {
        
        incProgress(0.1, detail = "Preparing...")
        
        # Get the function
        func <- get(func_name)
        
        # Execute with collected parameters
        incProgress(0.3, detail = "Processing...")
        result <- do.call(func, param_values)
        
        incProgress(0.9, detail = "Finalizing...")
        
        # Update file list
        Sys.sleep(0.5)
        update_file_list(output_path)
        
        incProgress(1.0, detail = "Complete!")
        
        # Show success notification
        showNotification(
          paste(func_name, "completed successfully!"),
          type = "message",
          duration = 5
        )
        
        # Display results
        output$results <- renderPrint({
          print(result)
        })
        
      })
      
    }, error = function(e) {
      showNotification(
        paste("Error executing", func_name, ":", e$message),
        type = "error",
        duration = 10
      )
      print(e)
      traceback()
    })
    
  })
  
  # ===== FUNCTION DESCRIPTION =====
  output$function_description <- renderText({
    function_list[[input$function_select]]$description
  })
  
  # ===== DYNAMIC INPUTS (BASE PARAMETERS) =====
  output$dynamic_inputs <- renderUI({
    req(input$function_select)
    params <- function_list[[input$function_select]]$params
    
    workflow_params <- c("generate_reports", "calculate_cbh", "analyze_canopy")
    
    base_params <- params[sapply(params, function(p) {
      param_name <- names(params)[which(sapply(params, identical, p))]
      is_base <- (is.null(p$advanced) || !p$advanced)
      is_not_workflow <- !(param_name %in% workflow_params)
      return(is_base && is_not_workflow)
    })]
    
    input_list <- lapply(names(base_params), function(param_name) {
      param <- base_params[[param_name]]
      
      if (param$type == "file") {
        input_id <- paste0("param_", param_name)
        shinyFileChoose(input, input_id, roots = volumes, session = session,
                        filetypes = c("txt", "xyz", "csv", "asc", "las", "laz"))
        div(
          shinyFilesButton(input_id, label = "Browse...", title = param$label,
                           multiple = FALSE, icon = icon("file")),
          textOutput(paste0(input_id, "_display"), inline = TRUE)
        )
      } else if (param$type == "numeric") {
        numericInput(
          paste0("param_", param_name),
          param$label,
          value = param$default,
          min = if (!is.null(param$min)) param$min else NA,
          max = if (!is.null(param$max)) param$max else NA,
          step = if (!is.null(param$step)) param$step else 1
        )
      } else if (param$type == "text") {
        textInput(
          paste0("param_", param_name),
          param$label,
          value = param$default,
          placeholder = if (!is.null(param$placeholder)) param$placeholder else ""
        )
      } else if (param$type == "checkbox") {
        checkboxInput(
          paste0("param_", param_name),
          param$label,
          value = param$default
        )
      } else if (param$type == "select") {
        selectInput(
          paste0("param_", param_name),
          param$label,
          choices = param$choices,
          selected = param$default
        )
      }
    })
    
    do.call(tagList, input_list)
  })
  
  
  
  
  # ===== ADVANCED INPUTS =====
  output$advanced_inputs <- renderUI({
    req(input$function_select)
    params <- function_list[[input$function_select]]$params
    
    workflow_params <- c("generate_reports", "calculate_cbh", "analyze_canopy")
    
    adv_params <- params[sapply(params, function(p) {
      param_name <- names(params)[which(sapply(params, identical, p))]
      is_advanced <- (!is.null(p$advanced) && p$advanced)
      is_not_workflow <- !(param_name %in% workflow_params)
      return(is_advanced && is_not_workflow)
    })]
    
    if (length(adv_params) == 0) return(NULL)
    
    input_list <- lapply(names(adv_params), function(param_name) {
      param <- adv_params[[param_name]]
      
      if (param$type == "numeric") {
        numericInput(
          paste0("param_", param_name),
          param$label,
          value = param$default,
          min = if (!is.null(param$min)) param$min else NA,
          max = if (!is.null(param$max)) param$max else NA,
          step = if (!is.null(param$step)) param$step else 1
        )
      } else if (param$type == "checkbox") {
        checkboxInput(
          paste0("param_", param_name),
          param$label,
          value = param$default
        )
      } else if (param$type == "select") {
        selectInput(
          paste0("param_", param_name),
          param$label,
          choices = param$choices,
          selected = param$default
        )
      }
    })
    
    div(
      id = "advanced_params_container",
      style = "display: none;",
      class = "advanced-params-box",
      div(class = "advanced-header", 
          icon("cog"), " Advanced Segmentation Parameters"),
      tags$p(
        style = "font-size: 0.9em; color: #666; margin-top: 5px;",
        "Fine-tune voxelization and clustering"
      ),
      hr(style = "margin: 5px 0 10px 0;"),
      do.call(tagList, input_list)
    )
  })
  
  # ===== FILENAME VALIDATION =====
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
  
  output$filename_preview <- renderText({
    params <- function_list[[input$function_select]]$params
    if (!"filename" %in% names(params)) return(NULL)
    
    req(input$param_filename)
    
    if (nzchar(input$param_filename)) {
      paste("Files will be saved as:", paste0(input$param_filename, "_*.xyz/csv"))
    } else {
      "Files will be saved with default prefix"
    }
  })
  
  output$current_output_dir <- renderText({
    path <- if (nzchar(input$output_path)) {
      input$output_path
    } else {
      tempdir()
    }
    paste("Current output directory:", path)
  })
  
  # ============================================================================
  # 3D VISUALIZATION - WITH CACHING
  # ============================================================================
  # This reads OUTPUT files (different from input), so reading is unavoidable
  # But we cache to avoid re-reading the same file multiple times
  # ============================================================================
  # Grid visualization for Analyze_cloud plots
  output$plot_density_raster <- renderPlotly({
    req(analysis_results())
    req("density_raster" %in% names(analysis_results()$plots))
    
    ggplotly(analysis_results()$plots[["density_raster"]])
  })
  
  output$plot_density_curve <- renderPlotly({
    req(analysis_results())
    req("density_curve" %in% names(analysis_results()$plots))
    
    ggplotly(analysis_results()$plots[["density_curve"]])
  })
  
  # Conditional UI for showing plots when Analyze_cloud is selected
  output$analyze_plots_ui <- renderUI({
    req(input$function_select == "Analyze_cloud")
    req(!is.null(analysis_results()))
    
    tagList(
      h4("Diagnostic Plots"),
      div(class = "info-box",
          tags$small("Interactive plots showing point cloud quality metrics. ",
                     "Zoom, pan, and hover for details.")),

      fluidRow(
        column(6,
               h5("Density Map (points/m²)"),
               withSpinner(plotlyOutput("plot_density_raster", height = "400px"),
                           type = 8, color = "#2196F3")),
        column(6,
               h5("Density Distribution (0.1m voxels)"),
               withSpinner(plotlyOutput("plot_density_curve", height = "400px"),
                           type = 8, color = "#2196F3"))
      )
    )
  })
  
  
  output$point_cloud <- renderPlotly({
    req(input$output_file, output_files())
    
    file_path <- file.path(current_output_path(), input$output_file)
    
    validate(
      need(file.exists(file_path), "File not found"),
      need(file.size(file_path) > 0, "File is empty")
    )
    
    # Check cache
    if (!is.null(viz_cache$file_path) && 
        viz_cache$file_path == file_path && 
        !is.null(viz_cache$data)) {
      
      # Use cached data
      data <- viz_cache$data
      
    } else {
      # Load new file
      data <- tryCatch({
        # Additional safety check: skip parameter files
        if (grepl("_parameters_", basename(file_path), fixed = TRUE)) {
          stop("Cannot visualize parameter log files")
        }
        
        df <- data.table::fread(file_path, header = FALSE, nThread = 2, 
                                fill = TRUE, blank.lines.skip = TRUE)
        
        # Validate that we have numeric columns
        if (ncol(df) < 3) {
          stop("File must contain at least 3 columns (x, y, z)")
        }
        
        # Check if first row is numeric (not text/comments)
        if (!is.numeric(df[[1]][1]) || !is.numeric(df[[2]][1]) || !is.numeric(df[[3]][1])) {
          stop("File does not appear to be a valid point cloud")
        }
        
        colnames(df)[1:3] <- c("x", "y", "z")
        df <- df[, 1:3]
        
        # Subsample if needed
        if (nrow(df) > 1e5) {
          set.seed(123)
          df <- df[sample(.N, 1e5)]
          showNotification(
            "Displaying 100,000 sampled points for performance",
            type = "warning", duration = 3
          )
        }
        
        # Cache the result
        viz_cache$file_path <- file_path
        viz_cache$data <- df
        
        df
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        NULL
      })
    }
    
    validate(need(!is.null(data), "Unable to load data"))
    
    # Create plot
    plot_ly(
      data,
      x = ~x, y = ~y, z = ~z,
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
          text = paste("Point Cloud:", input$output_file),
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
}