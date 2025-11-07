#' Launch PiC Shiny App
#'
#' @description Launch the Shiny app for interactive 3D point cloud processing.
#' @details This function launches an interactive web application for analyzing
#'   forest point cloud data. The app requires additional packages that are not
#'   installed by default. If these packages are missing, you will be prompted
#'   to install them.
#' @return No return value, called for side effects (launches Shiny app)
#' @export
#' @examples
#' \dontrun{
#' # Launch the interactive app
#' run_PiC()
#' }
run_PiC <- function() {
  
  # List of required Shiny packages
  shiny_packages <- c(
    "shiny", "shinyjs", "shinyFeedback", "shinydashboardPlus",
    "shinycssloaders", "shinythemes", "shinyWidgets", "shinydashboard",
    "shinyFiles", "DT", "plotly", "fs"
  )
  
  # Check which packages are missing
  missing_packages <- shiny_packages[!sapply(shiny_packages, requireNamespace, quietly = TRUE)]
  
  # If packages are missing, provide helpful message
  if (length(missing_packages) > 0) {
    message("The Shiny app requires additional packages that are not currently installed.")
    message("\nMissing packages: ", paste(missing_packages, collapse = ", "))
    message("\nTo install them, run:")
    message('  install.packages(c("', paste(missing_packages, collapse = '", "'), '"))')
    
    # Ask user if they want to install now (only in interactive sessions)
    if (interactive()) {
      response <- readline(prompt = "\nDo you want to install these packages now? (yes/no): ")
      if (tolower(trimws(response)) %in% c("yes", "y", "si", "s")) {
        message("\nInstalling packages...")
        tryCatch({
          utils::install.packages(missing_packages, quiet = FALSE)
          message("\nPackages installed successfully!")
        }, error = function(e) {
          stop("Failed to install packages: ", e$message, call. = FALSE)
        })
      } else {
        stop("Cannot launch app without required packages. Please install them and try again.", 
             call. = FALSE)
      }
    } else {
      stop("Cannot launch app without required packages. Please install them first.", 
           call. = FALSE)
    }
  }
  
  # Find the app directory
  app_dir <- system.file("shinyapp", package = "PiC")
  
  if (app_dir == "") {
    stop("App directory not found. Please reinstall the package.", call. = FALSE)
  }
  
  # Launch the app
  message("Launching PiC Shiny app...")
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
}
