# ==============================================================================
# FOREST_SEG v1.7 - Global Variable Declarations
# ==============================================================================
# 
# This file declares global variables used in data.table operations to avoid
# R CMD check NOTEs about "no visible binding for global variable".
# 
# These variables are created by data.table's non-standard evaluation (NSE)
# and are valid within data.table operations even though they appear undefined
# in standard R scope.
# 
# Version 1.7 uses INTEGER coordinates internally (Xi, Yi, Zi) and converts
# to float only for final outputs.
# 
# NOTE: The function uses assign(".tic_start_time", ..., envir = .GlobalEnv)
# for tictoc timing. This triggers a NOTE in R CMD check but is intentional
# for performance timing. Alternative: store in function environment instead.
# ==============================================================================

# Import standard R functions to avoid NOTEs
#' @importFrom stats sd complete.cases weighted.mean median quantile prcomp density setNames
#' @importFrom grDevices chull dev.off hcl.colors pdf
#' @importFrom utils head object.size write.csv install.packages
#' @importFrom data.table := .SD .BY .N .I
#' @importFrom data.table data.table setDT setkey setnames setorder copy fread fwrite
#' @importFrom data.table CJ between fifelse setindex haskey rbindlist as.data.table frollmean
#' @importFrom dplyr anti_join summarise
#' @importFrom dbscan dbscan
#' @importFrom terra rast ext rasterize focal ifel as.data.frame
#' @importFrom collapse fcount na_omit
#' @importFrom conicfit CircleFitByPratt CircleFitByLandau
#' @importFrom magrittr %>%
#' @importFrom tictoc tic toc
#' @importFrom tools file_ext
NULL


utils::globalVariables(c(
  # data.table special symbols
  ".", ".SD", ".BY", ".I", ".N", ":=", "J",
  # ============================================================================
  # COORDINATE VARIABLES
  # ============================================================================
  
  # Original input coordinates (float, only in initial loading and final output)
  "x", "y", "z",
  "X", "Y", "Z",
  
  # Integer coordinates (internal processing - main working variables)
  "Xi", "Yi", "Zi",           # Absolute integer coordinates (shifted to origin)
  "Zni",                      # Normalized height (Zi - z_dtm_i)
  "Zi_original",              # Original Zi before DAV processing
  
  # ============================================================================
  # GRID INDICES (dimensionless integers)
  # ============================================================================
  
  # DTM grid indices (coarse and fine)
  "u_dtm_idx", "v_dtm_idx",   # Coarse DTM grid indices
  "u_fine_idx", "v_fine_idx", # Fine DTM grid indices
  "u_coarse_idx", "v_coarse_idx", # Coarse grid reference for fine grid
  
  # Generic grid indices (used in various contexts)
  "u_idx", "v_idx", "w_idx",  # Voxel grid indices (wood, DAV, canopy)
  "x_grid", "y_grid",          # Generic grid positions
  "grid_x", "grid_y", "grid_z", # Alternative naming for grid positions
  "grid_x_dsm", "grid_y_dsm",  # DSM-specific grid positions
  
  # ============================================================================
  # DTM VARIABLES (integer elevations)
  # ============================================================================
  
  "z_dtm_i",                  # DTM elevation (integer)
  "z_fine_i",                 # Fine DTM interpolated elevation
  "x_fine_i", "y_fine_i",     # Fine grid absolute coordinates
  "z_min_i",                  # Minimum elevation (used in multiple contexts)
  "dsm_z_i",                  # DSM elevation (integer)
  
  # Floor extraction
  "is_floor",                 # Boolean flag for floor classification
  "z_diff",                   # Height difference from DTM
  
  # ============================================================================
  # TREE METRICS
  # ============================================================================
  
  # Basic identifiers
  "cls", "Tree_n", "Tree_n_old",
  
  # Heights and dimensions
  "Height", "Height_tree",
  "base_z_i", "high_z_i",     # Wood cluster base and top (integer)
  "height_dsm_m", "height_wood_m", # Temporary height calculations (meters)
  
  # DBH variables
  "DBH", "DBH_cm", "DBH_valid",
  "DBH_RMSE", "DBH_RMSE_cm",
  "x_m", "y_m",               # Temporary meter coordinates for DBH fitting
  
  # Tree positions
  "Xi_tree", "Yi_tree", "X_tree", "Y_tree",
  
  # CBH
  "CBH", "Crown_Base_m",
  
  # ============================================================================
  # CLUSTER STATISTICS (LOR - Ligneous Object Recognition)
  # ============================================================================
  
  "N_vox",                    # Number of voxels in cluster
  "delta_w",                  # Vertical extent in voxels
  "linearity",                # PCA linearity metric
  
  # ============================================================================
  # DAV (Directional Anisotropy of Vegetation) CLASSIFICATION VARIABLES
  # ============================================================================
  
  "cluster_id",               # DAV cluster ID
  "cluster",                  # DBSCAN cluster assignment
  "Zni_min",                  # Minimum normalized height in voxel
  "min_z_i",                  # Generic minimum z (integer)
  "max_z_i",                  # Generic maximum z (integer) - also used in heights
  
  # ============================================================================
  # GAB (Geometrical Aggregation of Biomass) / CBH CALCULATION VARIABLES
  # ============================================================================
  
  # Cylindrical coordinates
  "layer_rel",                # Relative vertical layer
  "banda",                    # Radial band
  "settore",                  # Angular sector
  "theta",                    # Azimuth angle
  "dx", "dy",                 # Relative position from tree center
  "dist_2d",                  # 2D distance from tree
  
  # Tree assignment
  "tree_id",
  
  # Connectivity analysis
  "layer_min", "layer_max",
  "settore_prev", "settore_next",
  "cell_id", "visited", "component_id",
  "n_cells", "z_min_component", "banda_max",
  
  # ============================================================================
  # AGGREGATION VARIABLES
  # ============================================================================
  
  "N",                        # Count of points/voxels
  "n_points",                 # Count of points in aggregation
  "n_voxels",                 # Count of voxels in aggregation
  
  # ============================================================================
  # DTM OUTLIER REMOVAL
  # ============================================================================
  
  "loc_median", "loc_sd",     # Local statistics for outlier detection
  
  # ============================================================================
  # TEMPORARY JOIN VARIABLES
  # ============================================================================
  
  "idx",                      # Generic index
  "u_neighbor", "v_neighbor", # Neighbor grid positions
  "sx", "sy",                 # Search grid positions
  
  # ============================================================================
  # DATA.TABLE JOIN PREFIX VARIABLES (i.* pattern)
  # ============================================================================
  
  "i.z_dtm_i",
  "i.base_z_i", "i.high_z_i",
  "i.neighbor_z_i", "i.wood_z_i",
  "i.cls_new",
  
  # ============================================================================
  # STATISTICS VARIABLES
  # ============================================================================
  
  "gx", "gy",                 # Grid positions for convex hull
  
  # ============================================================================
  # LEGACY/COMPATIBILITY VARIABLES (v1.5.8 and other functions)
  # ============================================================================
  
  # Old DTM naming (used in some legacy functions like Floseg)
  "u_dtm", "v_dtm", "z_dtm",
  "u_fine", "v_fine", "z_fine",
  "x_fine", "y_fine",
  "u_coarse", "v_coarse",
  
  # Other legacy variables
  "num",                      # Generic counter/number variable

  # ============================================================================
  # DATA.TABLE FUNCTIONS
  # ============================================================================

  "setcolorder",              # Column reordering function
  "first",                    # First element function
  ".N", ".SD", ".I",          # data.table special symbols

  # ============================================================================
  # FOREST_SEG v3.0.6 VARIABLES
  # ============================================================================

  # Float coordinate variables
  "gx_fine", "gy_fine",       # Fine grid float coordinates
  "gx_coarse", "gy_coarse",   # Coarse grid float coordinates
  "i.z_dtm",                  # DTM join variable (float)
  "i.base_z", "i.top_z",      # Tree base/top z (float)
  "i.neighbor_z",             # Neighbor z for interpolation (float)
  "i.z_dsm", "i.z_min",       # DSM and z_min join vars (float)
  "z_dtm_coarse",              # Coarse DTM z for gap-fill (float)
  "base_z", "z_dsm",          # Tree base z, DSM z (float)
  "wood_base_z", "wood_top_z", # Wood cluster base/top (float)
  "height_dsm", "height_wood", # Height calculations (float)
  "z_min", "z_max", "z_median", # Cluster z statistics
  "n_pts",                     # Point count in grid cells
  "z_dtm_smooth",              # Smoothed DTM values
  "z_smooth", "z_residual",    # DTM outlier detection (shared_utils)
  "u", "v", "w", "wmin", "w0", # Voxel coordinates for floor extraction (DBSCAN)
  "i.wmin",                      # Join variable for wmin
  "floor_vox_th", "floor_min_cluster", # Floor extraction parameters
  "quality",                   # Wood cluster quality metric
  "shape_ratio",               # Shape ratio for wood filtering
  "delta_u", "delta_v",        # Voxel extent in u/v
  "height",                    # Generic height variable
  "r",                         # Radius variable
  "density",                   # Density variable
  "classification",            # DAV classification
  "is_grounded",               # DAV grounded flag
  "i.classification",          # DAV classification join variable
  "..tree_cols",               # data.table column selection
  "is_wood",                   # Wood voxel flag
  "valid_tree",                # Valid tree flag
  "u_15", "v_15",              # 15cm voxel coordinates
  "w_raw", "w_norm", "w_orig", # Voxel w coordinates

  # GAB (Geometrical Aggregation of Biomass) / CBH v3.0.4+ variables
  "u_base", "v_base",          # Tree base voxel coords
  "hex_q", "hex_r",            # Hexagonal grid coordinates
  "layer", "z_m",              # Layer index and z in meters
  "vtype",                     # Voxel type (wood/crown)
  "N_total",                   # Total point count
  "cell_type",                 # Cell type classification
  "n_wood", "n_crown",         # Wood/crown voxel counts
  "i.cluster",                 # Cluster join variable
  "z_center",                  # Cluster z center
  "x_center", "y_center",      # Cluster x/y center
  "h_dist",                    # Horizontal distance

  # pic_analyze_cloud variables
  "x_cell", "y_cell",          # Grid cell indices
  "mat_idx",                   # Matrix index for raster filling
  "ix", "iy", "iz",            # Voxel indices
  "summarise",                 # dplyr function used with pipe

  # ============================================================================
  # FOREST_SEG v3.1 VARIABLES
  # ============================================================================

  # LAS classification column (uppercase, as used in lidR LAS objects)
  "Classification",

  # CPI (Crown Packing Index) intermediate variables
  "n_occupied",               # Occupied voxel count per column
  "n_envelope",               # Envelope voxel count per column
  "CPI",                      # Per-column Crown Packing Index

  # ============================================================================
  # metrics_from_las VARIABLES
  # ============================================================================
  "cls_las",                  # Raw LAS Classification code column
  "cls_raw",                  # DBSCAN cluster ID before renumbering
  "z_norm",                   # Height normalised by tree base z
  "i.cls"                     # data.table join prefix for cls
))
