# PiC Shiny Application - Global Configuration
# Multi-platform compatibility for Windows, macOS, and Linux
# Updated for Forest_seg 3.3

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
#


assign("function_list", list(
  
  ############################################################################
  # FOREST_SEG 3.3 - Complete forest segmentation
  ############################################################################
  "Forest_seg" = list(
    description = paste(
      "Complete forest analysis pipeline: segments 3D point clouds into forest floor,",
      "wood, understory and foliage components, then computes individual tree metrics (DBH, height,",
      "crown base) and canopy-level statistics (coverage, volume, height distribution).",
      "\n\n**3.3:** Optimized DTM extraction with voxel-based approach.",
      "GAB CBH uses voxel coordinates (u,v,w); tree center = wood voxel with min(w) per cluster;",
      "Voronoi for crown_voxels based on (u,v). CBH BFS uses direct vertical adjacency (no gap jumps),",
      "targeting the lowest foliated branch.",
      "DBH via circle-fitting at multiple heights (1.3 m primary, 1.8 / 2.3 m fallback).",
      "\n\nBASIC interface: Essential parameters for quick analysis.",
      "ADVANCED mode: Fine-tune DTM, clustering, and validation thresholds."
    ),
    params = list(
      
      # ========================================================================
      # SECTION 1: INPUT/OUTPUT & COORDINATE SYSTEM
      # ========================================================================
      
      "a" = list(
        type = "file",
        label = "Input point cloud file (.txt, .xyz, .csv, .las, .laz)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv", ".las", ".laz"),
        advanced = TRUE  # Hidden - use shared_file_input instead
      ),
      
      "filename" = list(
        type = "text",
        label = "Output file prefix",
        default = "XXX",
        required = FALSE,
        placeholder = "e.g., plot_001"
      ),
      
      "integer_precision" = list(
        type = "select",
        label = "Coordinate precision - Memory vs accuracy tradeoff",
        choices = c(
          "Millimeter (1mm precision, higher memory)" = "mm",
          "Centimeter (1cm precision, lower memory)" = "cm"
        ),
        default = "mm",
        advanced = FALSE,
        help_text = "mm: Best for high-resolution TLS data. cm: Efficient computation"
      ),
      
      "output_path" = list(
        type = "text",
        label = "Output directory path",
        default = "tempdir()",
        required = FALSE,
        placeholder = "Leave empty for temp directory",
        advanced = TRUE
      ),
      
      # ========================================================================
      # SECTION 2: TERRAIN - Forest floor segmentation (DTM-based)
      # ========================================================================
      # Purpose: Extract ground surface for height normalization
      # Spatial scale: Decimeter to meter resolution
      # All parameters ADVANCED - DTM uses robust defaults
      
      "dtm_coarse_res" = list(
        type = "numeric",
        label = "DTM coarse resolution (m) - Initial grid size",
        default = 0.5,
        min = 0.1,
        max = 5,
        step = 0.1,
        advanced = TRUE,
        help_text = "Larger values = faster processing, suitable for flat terrain"
      ),
      
      "tolerance" = list(
        type = "numeric",
        label = "Floor extraction tolerance (m) - Vertical threshold above DTM",
        default = 0.4,
        min = 0.1,
        max = 1.0,
        step = 0.1,
        advanced = TRUE,
        help_text = "Points within this height above DTM are classified as floor"
      ),
      
      # ========================================================================
      # SECTION 3: WOOD - Trunk and branch detection
      # ========================================================================
      # Purpose: Identify woody structures using density + geometry
      # Spatial scale: Centimeter voxels, decimeter clustering
      
      # --- Voxelization stage ---
      "dimVox" = list(
        type = "numeric",
        label = "Wood voxel size (cm) - 3D grid resolution for density analysis",
        default = 2,
        min = 1,
        max = 20,
        step = 1,
        advanced = FALSE,
        help_text = "Smaller = higher detail but slower. 2cm typical for TLS, 5-10cm for ALS"
      ),
      
      "th" = list(
        type = "numeric",
        label = "Min points per voxel (n) - Occupancy threshold for wood",
        default = 2,
        min = 1,
        max = 50,
        step = 1,
        advanced = FALSE,
        help_text = "Voxels with fewer points are discarded as noise"
      ),
      
      # --- Clustering stage (DBSCAN) ---
      "eps" = list(
        type = "numeric",
        label = "DBSCAN epsilon (voxel units) - Neighborhood search distance",
        default = 2,
        min = 0.1,
        max = 10,
        step = 0.1,
        advanced = FALSE,
        help_text = "Distance to connect voxels into clusters. Larger = merge more structures"
      ),
      
      "mpts" = list(
        type = "numeric",
        label = "DBSCAN min neighbors (n) - Core point threshold",
        default = 9,
        min = 1,
        max = 50,
        step = 1,
        advanced = FALSE,
        help_text = "Minimum voxels in neighborhood to form cluster core"
      ),
      
      # --- Filtering stage ---
      "h_trunk" = list(
        type = "numeric",
        label = "Min trunk length (m) - Vertical extent filter",
        default = 3,
        min = 1,
        max = 20,
        step = 0.1,
        advanced = TRUE,
        help_text = "Clusters shorter than this are discarded (removes shrubs, branches)"
      ),
      
      "N" = list(
        type = "numeric",
        label = "Min voxels per cluster (n) - Size filter for wood detection",
        default = 3000,
        min = 100,
        max = 1e6,
        step = 100,
        advanced = TRUE,
        help_text = "Minimum cluster size to qualify as tree. Lower for small trees"
      ),
      
      "w_linear" = list(
        type = "numeric",
        label = "PCA linearity threshold (0-1) - Geometric descriptor for wood shape",
        default = 0.50,
        min = 0.1,
        max = 1,
        step = 0.01,
        advanced = TRUE,
        help_text = "Higher values = stricter cylindrical shape requirement. 0.5 = balanced"
      ),

      "h_rescue_min" = list(
        type = "numeric",
        label = "Wood rescue min height (m) - Lower bound for isolated wood rescue",
        default = 1,
        min = 0,
        max = 5,
        step = 0.5,
        advanced = TRUE,
        help_text = "Minimum height for rescuing isolated wood points not assigned to any cluster"
      ),

      "h_rescue_max" = list(
        type = "numeric",
        label = "Wood rescue max height (m) - Upper bound for isolated wood rescue",
        default = 5,
        min = 1,
        max = 30,
        step = 0.5,
        advanced = TRUE,
        help_text = "Maximum height for rescuing isolated wood points not assigned to any cluster"
      ),

      # ========================================================================
      # SECTION 4: CROWN-UNDERSTORY SEPARATION (DAV Classification)
      # ========================================================================
      # Purpose: Vertical stratification using density-based approach
      # Spatial scale: Decimeter voxels, meter-scale clustering
      
      "canopy_vox_dim" = list(
        type = "numeric",
        label = "DAV voxel size (m) - Resolution for wood/foliage separation and crown detection",
        default = 0.15,
        min = 0.05,
        max = 0.5,
        step = 0.05,
        advanced = TRUE,
        help_text = "Grid resolution for vertical analysis. Smaller = more detail (0.10m default)"
      ),
      
      

      "dav_understory_max_start" = list(
        type = "numeric",
        label = "DAV min understory base (m)",
        default = 1.3,
        min = 0.5,
        max = 10.0,
        step = 0.1,
        advanced = TRUE,
        help_text = "Understory threshold: vegetation rooted below this may be classified as understory"
      ),
      
      "dav_eps" = list(
        type = "numeric",
        label = "DAV DBSCAN radius (voxel units) - Horizontal clustering distance",
        default = 2,
        min = 0.5,
        max = 10.0,
        step = 0.1,
        advanced = TRUE,
        help_text = "Distance to connect crown voxels. Larger = merge adjacent crowns"
      ),
      
      "dav_minPts" = list(
        type = "numeric",
        label = "DAV DBSCAN min neighbors (n) - Core point threshold",
        default = 4,
        min = 2,
        max = 50,
        step = 1,
        advanced = TRUE,
        help_text = "Minimum voxels to form cluster core. Lower = detect smaller crowns"
      ),

      # ========================================================================
      # SECTION 5: CROWN BASE HEIGHT (CBH) - GAB Voronoi
      # ========================================================================
      # Purpose: Identify lowest live crown attachment point
      # Method: Hexagonal tessellation + Voronoi + BFS connectivity
      # Spatial scale: Decimeter layers, configurable hex cells

      "calculate_cbh" = list(
        type = "checkbox",
        label = "Calculate crown base height (CBH) using GAB Voronoi method",
        default = TRUE,
        advanced = FALSE
      ),

      "cbh_hex_side" = list(
        type = "numeric",
        label = "CBH hexagon edge (m) - Spatial resolution for crown tessellation",
        default = 0.15,
        min = 0.05,
        max = 0.50,
        step = 0.01,
        advanced = TRUE,
        help_text = "Edge length of hexagonal cells for crown analysis. Smaller = finer detail but slower"
      ),

      "cbh_min_branch_length" = list(
        type = "numeric",
        label = "CBH min branch length (m) - Minimum foliage extent in XY plane",
        default = 2.0,
        min = 1,
        max = 10.0,
        step = 0.5,
        advanced = TRUE,
        help_text = "Minimum horizontal extent of connected foliage to qualify as valid branch (converted to hex cells internally)"
      ),

      # ========================================================================
      # SECTION 6: CANOPY DENSITY THRESHOLD (shared by DAV and CBH)
      # ========================================================================
      # Purpose: minimum point density for a voxel to count as valid canopy
      # Note: crown/understory coverage and volumes are computed automatically
      #       (no separate canopy module; CPI removed in 3.3)

      "canopy_min_density" = list(
        type = "numeric",
        label = "Min canopy density (pts/m³) - Density threshold for valid canopy",
        default = 500,
        min = 10,
        max = 5000,
        step = 10,
        advanced = TRUE,
        help_text = paste(
          "Minimum point density to classify voxel as canopy.",
          "• Dense forests (temperate): 200-500 pts/m³",
          "• Sparse forests (Mediterranean): 50-200 pts/m³",
          "• Very sparse (degraded): 10-100 pts/m³",
          "Lower values include more sparse crown material but may add noise"
        )
      ),
      # ========================================================================
      # SECTION 7: DBH - Diameter Measurement
      # ========================================================================
      # Purpose: DBH with cylindrical pre-filter on [1.2m, 1.8m] + circle fitting
      # Fixed internally: heights = c(1.3, 1.8, 2.3), min_points = 8

      "dbh_tolerance" = list(
        type = "numeric",
        label = "DBH vertical tolerance (m) - Slice thickness around target height",
        default = 0.05,
        min = 0.01,
        max = 0.2,
        step = 0.01,
        advanced = TRUE,
        help_text = "Points within ±tolerance of target height are used (±5cm default)"
      ),
      
      "dbh_max_rmse" = list(
        type = "numeric",
        label = "Max DBH RMSE (cm) - Quality threshold for circle fitting",
        default = 5,
        min = 0.5,
        max = 50,
        step = 0.5,
        advanced = TRUE,
        help_text = "Fits with higher RMSE are marked as invalid (poor fit quality)"
      ),
      
      "dbh_min_radius" = list(
        type = "numeric",
        label = "Min trunk radius (m) - Lower bound for valid measurements",
        default = 0.025,
        min = 0.01,
        max = 0.1,
        step = 0.005,
        advanced = TRUE,
        help_text = "Minimum valid radius (2.5cm = 5cm DBH, suitable for saplings)"
      ),
      
      "dbh_max_radius" = list(
        type = "numeric",
        label = "Max trunk radius (m) - Upper bound for valid measurements",
        default = 0.5,
        min = 0.1,
        max = 5.0,
        step = 0.1,
        advanced = TRUE,
        help_text = "Maximum valid radius (0.5m = 1.0m DBH)"
      ),

      # ========================================================================
      # SECTION 8: DIAGNOSTICS & EXPORT OPTIONS
      # ========================================================================
      
      "generate_reports" = list(
        type = "checkbox",
        label = "Generate CSV reports (tree metrics, plot statistics)",
        default = TRUE,
        advanced = TRUE
      ),
      
      "Vox_print" = list(
        type = "checkbox",
        label = "Export voxelized point cloud (wood detection stage)",
        default = FALSE,
        advanced = TRUE
      ),

      "Woodpoints_print" = list(
        type = "checkbox",
        label = "Export wood point cloud (all detected wood points)",
        default = TRUE,
        advanced = TRUE
      ),

      "cbh_save_points" = list(
        type = "checkbox",
        label = "Save CBH foliage layer points to file",
        default = TRUE,
        advanced = TRUE
      ),

      "output_format" = list(
        type = "select",
        label = "Output file format",
        choices = c(
          "LAS/LAZ format (.las)" = "las",
          "Text/XYZ format (.xyz)" = "xyz"
        ),
        default = "las",
        advanced = TRUE,
        help_text = "Format for output point cloud files. LAS is binary and compact; TXT is human-readable"
      )
    )
  ),

  ############################################################################
  # METRICS_FROM_LAS - Recompute metrics from an already-classified LAS
  ############################################################################
  "metrics_from_las" = list(
    description = paste(
      "Recomputes tree- and plot-level metrics from an ALREADY CLASSIFIED point",
      "cloud (LAS with ASPRS classes: ground=2, understory=3, wood=4, crown=5).",
      "Skips segmentation and runs only DBH, height, crown base height (CBH) and",
      "plot statistics (coverage, crown/understory volume).",
      "\n\nUseful to re-run the metrics after manual edits of the classification,",
      "without repeating the full Forest_seg segmentation. Provide a classified",
      ".las/.laz file as input."
    ),
    params = list(

      # ========================================================================
      # INPUT / OUTPUT
      # ========================================================================
      "las_file" = list(
        type = "file",
        label = "Classified LAS/LAZ file (ASPRS classes 2,3,4,5)",
        required = TRUE,
        accept = c(".las", ".laz")
      ),

      "filename" = list(
        type = "text",
        label = "Output file prefix",
        default = "metrics",
        required = FALSE,
        placeholder = "e.g., plot_001"
      ),

      "output_path" = list(
        type = "text",
        label = "Output directory path",
        default = "tempdir()",
        required = FALSE,
        placeholder = "Leave empty for temp directory",
        advanced = TRUE
      ),

      # ========================================================================
      # DBH - Diameter measurement
      # ========================================================================
      "dbh_tolerance" = list(
        type = "numeric",
        label = "DBH vertical tolerance (m) - Slice thickness around target height",
        default = 0.05,
        min = 0.01,
        max = 0.2,
        step = 0.01,
        advanced = TRUE,
        help_text = "Points within \u00b1tolerance of target height are used (\u00b15cm default)"
      ),

      "dbh_max_rmse" = list(
        type = "numeric",
        label = "Max DBH RMSE (cm) - Quality threshold for circle fitting",
        default = 5,
        min = 0.5,
        max = 50,
        step = 0.5,
        advanced = TRUE,
        help_text = "Fits with higher RMSE are marked as invalid (poor fit quality)"
      ),

      "dbh_min_radius" = list(
        type = "numeric",
        label = "Min trunk radius (m) - Lower bound for valid measurements",
        default = 0.025,
        min = 0.01,
        max = 0.1,
        step = 0.005,
        advanced = TRUE,
        help_text = "Minimum valid radius (2.5cm = 5cm DBH, suitable for saplings)"
      ),

      "dbh_max_radius" = list(
        type = "numeric",
        label = "Max trunk radius (m) - Upper bound for valid measurements",
        default = 0.8,
        min = 0.1,
        max = 5.0,
        step = 0.1,
        advanced = TRUE,
        help_text = "Maximum valid radius (0.8m = 1.6m DBH)"
      ),

      # ========================================================================
      # CBH - Crown base height
      # ========================================================================
      "calculate_cbh" = list(
        type = "checkbox",
        label = "Calculate crown base height (CBH) using GAB Voronoi method",
        default = TRUE,
        advanced = FALSE
      ),

      "cbh_hex_side" = list(
        type = "numeric",
        label = "CBH hexagon edge (m) - Spatial resolution for crown tessellation",
        default = 0.15,
        min = 0.05,
        max = 0.50,
        step = 0.01,
        advanced = TRUE,
        help_text = "Edge length of hexagonal cells for crown analysis. Smaller = finer detail but slower"
      ),

      "cbh_min_branch_length" = list(
        type = "numeric",
        label = "CBH min branch length (m) - Minimum foliage extent in XY plane",
        default = 2.0,
        min = 1,
        max = 10.0,
        step = 0.5,
        advanced = TRUE,
        help_text = "Minimum horizontal extent of connected foliage to qualify as valid branch"
      ),

      # ========================================================================
      # CANOPY DENSITY (shared by DAV and CBH)
      # ========================================================================
      "canopy_vox_dim" = list(
        type = "numeric",
        label = "DAV voxel size (m) - Resolution for crown detection and volume",
        default = 0.15,
        min = 0.05,
        max = 0.5,
        step = 0.05,
        advanced = TRUE,
        help_text = "Grid resolution for vertical analysis. Smaller = more detail"
      ),

      "canopy_min_density" = list(
        type = "numeric",
        label = "Min canopy density (pts/m\u00b3) - Density threshold for valid canopy",
        default = 100,
        min = 10,
        max = 5000,
        step = 10,
        advanced = TRUE,
        help_text = paste(
          "Minimum point density to classify voxel as canopy.",
          "\u2022 Dense forests (temperate): 200-500 pts/m\u00b3",
          "\u2022 Sparse forests (Mediterranean): 50-200 pts/m\u00b3",
          "Lower values include more sparse crown material but may add noise"
        )
      ),

      # ========================================================================
      # EXPORT
      # ========================================================================
      "generate_reports" = list(
        type = "checkbox",
        label = "Generate CSV reports (tree metrics, plot statistics)",
        default = TRUE,
        advanced = TRUE
      )
    )
  ),

  ############################################################################
  # SEGONE - Single tree wood segmentation
  ############################################################################
  "SegOne" = list(
    description = paste(
      "Individual tree wood segmentation for TLS scans of isolated trees.",
      "Uses voxel-based density analysis combined with DBSCAN clustering to",
      "separate trunk and branches from foliage. Outputs wood point cloud."
    ),
    params = list(
      "a" = list(
        type = "file",
        label = "Input point cloud file (.txt, .xyz, .csv, .las, .laz)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv", ".las", ".laz"),
        advanced = TRUE  # Hidden - use shared_file_input instead
      ),
      "filename" = list(
        type = "text",
        label = "Output file prefix",
        default = "tree_wood",
        required = FALSE,
        placeholder = "e.g., tree_001"
      ),
      "dimVox" = list(
        type = "numeric",
        label = "Voxel size (cm) - Grid resolution for density analysis",
        default = 2,
        min = 1,
        max = 20,
        step = 1
      ),
      "th" = list(
        type = "numeric",
        label = "Min points per voxel (n) - Occupancy threshold",
        default = 2,
        min = 1,
        max = 50,
        step = 1
      ),
      "eps" = list(
        type = "numeric",
        label = "DBSCAN radius (voxel units) - Clustering distance",
        default = 1,
        min = 1,
        max = 10,
        step = 1
      ),
      "mpts" = list(
        type = "numeric",
        label = "DBSCAN min neighbors (n) - Core point threshold",
        default = 6,
        min = 4,
        max = 20,
        step = 1
      ),
      "N" = list(
        type = "numeric",
        label = "Min voxels per cluster (n) - Size filter",
        default = 1000,
        min = 100,
        max = 50000,
        step = 100,
        advanced = TRUE
      ),
      "R" = list(
        type = "numeric",
        label = "Shape parameter (dimensionless) - Cluster compactness",
        default = 30,
        min = 1,
        max = 100,
        step = 1,
        advanced = TRUE
      )
    )
  ),
  
  ############################################################################
  # ANALYZE_CLOUD - Point cloud diagnostic analysis
  ############################################################################
  "Analyze_cloud" = list(
    description = paste(
      "Diagnostic analysis of point cloud quality and structure before processing.",
      "Generates density maps and voxel density statistics to assess data quality.",
      "\n\n**Features:**",
      "• Density analysis: points per m² with spatial distribution map",
      "• Voxel density analysis at 10 cm resolution with density per m³",
      "• Interactive visualization in Shiny interface",
      "• Optional PDF report export",
      "\n\n**Workflow integration:**",
      "The loaded point cloud remains in memory for direct use in Forest_seg analysis,",
      "avoiding redundant file loading and speeding up the processing pipeline."
    ),
    params = list(
      "a" = list(
        type = "file",
        label = "Input point cloud file (.txt, .xyz, .csv, .las, .laz)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv", ".las", ".laz"),
        advanced = TRUE  # Hidden - use shared_file_input instead
      ),
      
      "generate_pdf" = list(
        type = "checkbox",
        label = "Generate PDF report",
        default = TRUE,
        advanced = FALSE,
        help_text = "Create a multi-page PDF with all plots and statistics"
      ),
      
      "pdf_filename" = list(
        type = "text",
        label = "PDF filename (without extension)",
        default = "cloud_diagnostics",
        required = FALSE,
        advanced = FALSE,
        placeholder = "e.g., plot_001_diagnostics"
      ),
      
      "voxel_sizes" = list(
        type = "text",
        label = "Voxel sizes for analysis (comma-separated, meters)",
        default = "0.1",
        advanced = TRUE,
        help_text = "Voxel size in meters for density analysis"
      )
    )
  ),
  
  
  ############################################################################
  # FLOSEG - Forest floor segmentation
  ############################################################################
  "Floseg" = list(
    description = paste(
      "Forest floor segmentation using 2-stage DTM approach: creates coarse DTM",
      "with outlier removal and gap filling, then interpolates fine DTM for accurate",
      "floor extraction. Robust on sloped and irregular terrain. Useful for terrain",
      "normalization and understory analysis."
    ),
    params = list(
      "a" = list(
        type = "file",
        label = "Input point cloud file (.txt, .xyz, .csv, .las, .laz)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv", ".las", ".laz"),
        advanced = TRUE  # Hidden - use shared_file_input instead
      ),
      "filename" = list(
        type = "text",
        label = "Output file prefix",
        default = "forest_floor",
        required = FALSE,
        placeholder = "e.g., plot_floor"
      ),
      
      # DTM PARAMETERS - Always visible
      "dtm_coarse_res" = list(
        type = "numeric",
        label = "Coarse DTM resolution (m) - Initial grid size",
        default = 0.5,
        min = 0.1,
        max = 2.0,
        step = 0.1,
        advanced = FALSE
      ),
      
      "tolerance" = list(
        type = "numeric",
        label = "Vertical tolerance (m) - Height threshold above DTM",
        default = 0.4,
        min = 0.1,
        max = 1.0,
        step = 0.1,
        advanced = FALSE
      ),
      
      # ADVANCED PARAMETERS
      "dtm_fine_res" = list(
        type = "numeric",
        label = "Fine DTM resolution (m) - Interpolated grid size",
        default = 0.1,
        min = 0.01,
        max = 0.5,
        step = 0.01,
        advanced = TRUE
      ),
      "outlier_k" = list(
        type = "numeric",
        label = "Z-score threshold (σ) - Outlier removal strength",
        default = 1,
        min = 1,
        max = 5,
        step = 1,
        advanced = TRUE
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
        label = "Input point cloud file (.txt, .xyz, .csv, .las, .laz)",
        required = TRUE,
        accept = c(".txt", ".xyz", ".csv", ".las", ".laz"),
        advanced = TRUE  # Hidden - use shared_file_input instead
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
        label = "Voxel size (cm) - 3D grid resolution",
        default = 2,
        min = 1,
        max = 1000,
        step = 1
      ),
      "th" = list(
        type = "numeric",
        label = "Min points per voxel (n) - Occupancy threshold",
        default = 2,
        min = 1,
        max = 1000,
        step = 1
      ),
      "coordinate_precision" = list(
        type = "select",
        label = "Coordinate precision - Memory vs accuracy tradeoff",
        choices = c(
          "Millimeter (1mm precision, higher memory)" = "mm",
          "Centimeter (1cm precision, lower memory)" = "cm"
        ),
        default = "mm",
        advanced = FALSE,
        help_text = "mm: Best for high-resolution data. cm: Efficient for lower resolution"
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

# Parameter file parser for loading parameters from log/template files
parse_parameter_file <- function(file_path) {
  
  tryCatch({
    # Read all lines
    lines <- readLines(file_path, warn = FALSE)
    
    # Remove empty lines
    lines <- lines[nchar(trimws(lines)) > 0]
    
    # Remove pure comment lines (starting with #)
    lines <- lines[!grepl("^\\s*#", lines)]
    
    # Keep only lines with "parameter = value" pattern
    param_lines <- lines[grepl("=", lines)]
    
    if (length(param_lines) == 0) {
      return(NULL)
    }
    
    # Parse each line
    params <- list()
    
    for (line in param_lines) {
      # Remove trailing comma if present
      line <- gsub(",$", "", trimws(line))
      
      # Remove inline comments (anything after # on same line)
      line <- gsub("#.*$", "", line)
      line <- trimws(line)
      
      # Split by first " = " or "="
      parts <- strsplit(line, "\\s*=\\s*", perl = TRUE)[[1]]
      
      if (length(parts) >= 2) {
        param_name <- trimws(parts[1])
        param_value <- trimws(paste(parts[-1], collapse = "="))
        
        # Skip if param_name is empty
        if (nchar(param_name) == 0) next
        
        # Convert value to appropriate type
        parsed_value <- .parse_param_value(param_value)
        
        if (!is.null(parsed_value)) {
          params[[param_name]] <- parsed_value
        }
      }
    }
    
    return(params)
    
  }, error = function(e) {
    warning("Error parsing parameter file: ", e$message)
    return(NULL)
  })
}

# Helper function to parse value from string
.parse_param_value <- function(value_str) {
  
  value_str <- trimws(value_str)
  
  # Logical: TRUE/FALSE (case insensitive)
  if (toupper(value_str) == "TRUE") {
    return(TRUE)
  } else if (toupper(value_str) == "FALSE") {
    return(FALSE)
  }
  
  # String: quoted with " or '
  if (grepl('^["\'].*["\']$', value_str)) {
    return(gsub('^["\']|["\']$', '', value_str))
  }
  
  # Numeric: try conversion
  num_value <- suppressWarnings(as.numeric(value_str))
  if (!is.na(num_value)) {
    return(num_value)
  }
  
  # Default: return as string (unquoted strings)
  return(value_str)
}

# ============================================================================
# AUTO-LOAD FOREST_SEG 3.3
# ============================================================================
# Try to load Forest_seg from common locations
forest_seg_paths <- c(
  "Forest_seg_v3_3.r",
  "R/Forest_seg_v3_3.r",
  "Forest_seg.R",
  "R/Forest_seg.R"
)

for (path in forest_seg_paths) {
  if (file.exists(path)) {
    message("Loading Forest_seg from: ", path)
    tryCatch({
      source(path, local = FALSE)
      message("✓ Forest_seg loaded successfully")
      break
    }, error = function(e) {
      warning("Failed to load ", path, ": ", e$message)
    })
  }
}

# Verify Forest_seg is available
if (!exists("Forest_seg", mode = "function")) {
  message("⚠ Forest_seg function not found!")
  message("  Current directory: ", getwd())
  message("  Place Forest_seg_v3_3.r in the same directory as global.R")
  message("  Or use package version if installed")
}
