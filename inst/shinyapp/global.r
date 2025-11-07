# PiC Shiny Application - Global Configuration
# Multi-platform compatibility for Windows, macOS, and Linux

# Load required packages
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(fs)
library(data.table)
library(plotly)

# Cross-platform volume detection
# This ensures compatibility across Windows, macOS, and Linux
get_system_volumes <- function() {
  if (.Platform$OS.type == "windows") {
    # Windows: Include standard user folders and available drives
    volumes <- c(
      Home = fs::path_home(),
      Desktop = file.path(fs::path_home(), "Desktop"),
      Documents = file.path(fs::path_home(), "Documents"),
      Temp = tempdir()
    )
    
    # Add available drives (C:, D:, etc.)
    tryCatch({
      drives <- getVolumes()()
      volumes <- c(volumes, drives)
    }, error = function(e) {
      message("Could not detect drives: ", e$message)
    })
    
  } else {
    # Unix-like systems (macOS, Linux)
    volumes <- c(
      Home = fs::path_home(),
      Desktop = file.path(fs::path_home(), "Desktop"),
      Documents = file.path(fs::path_home(), "Documents"),
      Root = "/",
      Temp = tempdir()
    )
    
    # macOS specific: Add /Volumes if it exists
    if (Sys.info()["sysname"] == "Darwin" && dir.exists("/Volumes")) {
      volumes <- c(volumes, Volumes = "/Volumes")
    }
  }
  
  return(volumes)
}

# Define available analysis functions with their parameters
# This structure drives the dynamic UI generation
assign("function_list", list(
  
  ############################################################################
  # FOREST_SEG - Complete forest segmentation and metrics
  ############################################################################
  "Forest_seg" = list(
    description = paste(
      "Complete forest analysis pipeline: segments 3D point clouds into forest floor,",
      "wood, and foliage components, then computes individual tree metrics (DBH, height,",
      "crown base) and canopy-level statistics (coverage, volume, height distribution)."
    ),
    params = list(
      "a" = list(
        type = "file",
        label = "Input point cloud file (.txt, .xyz, .csv)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv")
      ),
      "filename" = list(
        type = "text",
        label = "Output file prefix",
        default = "forest_analysis",
        required = FALSE,
        placeholder = "e.g., plot_A1"
      ),
      "dimVox" = list(
        type = "numeric",
        label = "Voxel size for wood segmentation (cm)",
        default = 2,
        min = 1,
        max = 20,
        step = 1
      ),
      "th" = list(
        type = "numeric",
        label = "Minimum points per voxel",
        default = 2,
        min = 1,
        max = 50,
        step = 1
      ),
      "eps" = list(
        type = "numeric",
        label = "DBSCAN epsilon radius (voxel units)",
        default = 2,
        min = 0.1,
        max = 10,
        step = 0.1
      ),
      "mpts" = list(
        type = "numeric",
        label = "DBSCAN minimum points for clustering",
        default = 9,
        min = 1,
        max = 50,
        step = 1
      ),
      "h_tree" = list(
        type = "numeric",
        label = "Minimum trunk length (m)",
        default = 1,
        min = 0.1,
        max = 5,
        step = 0.1
      ),
      "soil_dim" = list(
        type = "numeric",
        label = "Voxel size for floor segmentation (m)",
        default = 0.1,
        min = 0.01,
        max = 0.5,
        step = 0.01
      ),
      "N" = list(
        type = "numeric",
        label = "Minimum voxels in wood cluster",
        default = 500,
        min = 10,
        max = 5000,
        step = 10
      ),
      "R" = list(
        type = "numeric",
        label = "Cluster shape parameter (PCA threshold)",
        default = 30,
        min = 1,
        max = 100,
        step = 1
      ),
      "Vox_print" = list(
        type = "checkbox",
        label = "Save voxelized point cloud",
        default = FALSE
      ),
      "WoodVox_print" = list(
        type = "checkbox",
        label = "Save wood voxelization",
        default = FALSE
      ),
      "analyze_canopy" = list(
        type = "checkbox",
        label = "Perform canopy analysis",
        default = TRUE
      ),
      "canopy_voxel_size" = list(
        type = "numeric",
        label = "Canopy voxel size (m)",
        default = 0.1,
        min = 0.01,
        max = 0.5,
        step = 0.01
      ),
      "min_canopy_height" = list(
        type = "numeric",
        label = "Minimum canopy height threshold (m)",
        default = 1.5,
        min = 0.1,
        max = 5,
        step = 0.1
      ),
      "coverage_method" = list(
        type = "select",
        label = "Coverage degree calculation method",
        choices = c(
          "Linear (column max)" = "linear",
          "Mean normalized" = "mean_normalized",
          "Exponential" = "exponential",
          "Threshold-based" = "threshold",
          "Mediterranean adapted" = "mediterranean"
        ),
        default = "mean_normalized"
      )
    )
  ),
  
  ############################################################################
  # SEGONE - Single tree segmentation
  ############################################################################
  "SegOne" = list(
    description = paste(
      "Single tree segmentation: separates wood from foliage in an individual tree",
      "point cloud using voxelization, DBSCAN clustering, and PCA-based wood detection.",
      "Computes tree height, DBH, and crown base height."
    ),
    params = list(
      "a" = list(
        type = "file",
        label = "Input point cloud file (.txt, .xyz, .csv)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv")
      ),
      "filename" = list(
        type = "text",
        label = "Output file prefix",
        default = "single_tree",
        required = FALSE,
        placeholder = "e.g., tree_001"
      ),
      "dimVox" = list(
        type = "numeric",
        label = "Voxel size (cm)",
        default = 2,
        min = 1,
        max = 20,
        step = 1
      ),
      "th" = list(
        type = "numeric",
        label = "Minimum points per voxel",
        default = 2,
        min = 1,
        max = 50,
        step = 1
      ),
      "eps" = list(
        type = "numeric",
        label = "DBSCAN epsilon radius",
        default = 1,
        min = 0.1,
        max = 10,
        step = 0.1
      ),
      "mpts" = list(
        type = "numeric",
        label = "DBSCAN minimum points",
        default = 4,
        min = 1,
        max = 20,
        step = 1
      ),
      "N" = list(
        type = "numeric",
        label = "Minimum voxels in wood cluster",
        default = 1000,
        min = 10,
        max = 10000,
        step = 10
      ),
      "R" = list(
        type = "numeric",
        label = "Cluster shape parameter",
        default = 30,
        min = 1,
        max = 100,
        step = 1
      )
    )
  ),
  
  ############################################################################
  # FLOSEG - Forest floor segmentation
  ############################################################################
  "Floseg" = list(
    description = paste(
      "Forest floor segmentation: separates ground points from above-ground biomass",
      "using voxelization and DBSCAN clustering. Useful for terrain normalization",
      "and understory analysis."
    ),
    params = list(
      "a" = list(
        type = "file",
        label = "Input point cloud file (.txt, .xyz, .csv)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv")
      ),
      "filename" = list(
        type = "text",
        label = "Output file prefix",
        default = "forest_floor",
        required = FALSE,
        placeholder = "e.g., plot_floor"
      ),
      "soil_dim" = list(
        type = "numeric",
        label = "Voxel size for floor detection (m)",
        default = 0.3,
        min = 0.01,
        max = 1.0,
        step = 0.01
      ),
      "th" = list(
        type = "numeric",
        label = "Minimum points per voxel",
        default = 20,
        min = 1,
        max = 100,
        step = 1
      ),
      "N" = list(
        type = "numeric",
        label = "Minimum voxels in floor cluster",
        default = 500,
        min = 10,
        max = 5000,
        step = 10
      )
    )
  ),
  
  ############################################################################
  # VOXELS - Simple voxelization
  ############################################################################
  "Voxels" = list(
    description = paste(
      "Basic point cloud voxelization: converts point cloud to voxel representation",
      "with configurable resolution and minimum point threshold. Useful for data",
      "reduction and preprocessing."
    ),
    params = list(
      "a" = list(
        type = "file",
        label = "Input point cloud file (.txt, .xyz, .csv)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv")
      ),
      "filename" = list(
        type = "text",
        label = "Output file prefix",
        default = "voxelized",
        required = FALSE,
        placeholder = "e.g., data_vox"
      ),
      "dimVox" = list(
        type = "numeric",
        label = "Voxel size (cm)",
        default = 2,
        min = 1,
        max = 1000,
        step = 1
      ),
      "th" = list(
        type = "numeric",
        label = "Minimum points per voxel",
        default = 2,
        min = 1,
        max = 1000,
        step = 1
      )
    )
  )
), envir = .GlobalEnv)

# Helper function to validate file paths across platforms
validate_path <- function(path) {
  if (is.null(path) || path == "") return(FALSE)
  
  # Normalize path separators
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  
  # Check if path is valid
  tryCatch({
    dir.exists(path) || file.exists(path)
  }, error = function(e) {
    FALSE
  })
}

# Helper function to format file sizes
format_file_size <- function(bytes) {
  if (is.na(bytes) || bytes < 1024) {
    return(paste(bytes, "B"))
  } else if (bytes < 1024^2) {
    return(paste(round(bytes / 1024, 1), "KB"))
  } else if (bytes < 1024^3) {
    return(paste(round(bytes / 1024^2, 1), "MB"))
  } else {
    return(paste(round(bytes / 1024^3, 1), "GB"))
  }
}