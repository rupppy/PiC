# ==============================================================================
# FOREST_SEG v3.3 - MAIN FUNCTION
# ==============================================================================
#
# CHANGELOG v3.3:
# - CHANGED: RESCUE PASS now (a) also considers pure-noise voxels (cls==0), not
#   only rejected clusters; (b) clamps h_trunk_factor to [0.3, 1.0] so rescue is
#   never stricter than the base pass on height; (c) halves DBSCAN minPts so
#   sparse noise can re-cluster; (d) applies a STRICTER linearity threshold than
#   the base pass (low trunk-band segments must be more linear), to reject bushes.
# - CHANGED: CBH connectivity now uses DIRECT vertical adjacency only (no gap
#   jumps). The BFS keeps the 6 lateral hexagonal neighbours but a single empty
#   layer breaks a branch instead of being bridged. This targets the lowest
#   foliated branch (CBH) rather than bridging into the upper crown.
#   The cbh_max_gap_layers parameter is retained for compatibility but unused.
# - NEW: crown_volume_m3 and understory_volume_m3 computed from the FINAL
#   classified points (re-voxelized at canopy_vox_dim) and written to plot_report.
#   Replaces the previous dav_volumes derived from voxel-level DAV stats.
# - REMOVED: Crown Packing Index (CPI) and the .analyze_canopy_float() function.
#   It was never wired into the pipeline; all CPI metrics are dropped.
# - REMOVED: unused helper .assign_components_bfs_float().
# - FIXED: voxel->point mapping no longer fails when DAV returns early
#   (H_threshold absent from stats); a robust fallback is applied.
# - FIXED: documented parameter defaults now match the function signature.
# - CHANGED: crown_volume_m3 / understory_volume_m3 rounded to whole m3 (no
#   decimals). Report CSVs keep the dot decimal separator for international use.
#
# CHANGELOG v3.1:
# - NEW: Crown Packing Index (CPI) [REMOVED in v3.3].
#
# CHANGELOG v3.0.7:
# - FIXED: Small crown clusters (< 10 voxels) reclassified as understory
#   to remove scattered low-height artifact points from crown.xyz
#
# CHANGELOG v3.0.6:
# - FIXED: GAB CBH branch length filter now measures per-branch XY extent
#   Previous version measured XY extent of entire connected component (trunk+crown),
#   which made the min_branch_length filter ineffective.
#   New approach: from each trunk-crown contact point, BFS only through crown cells
#   to isolate individual branches, then measure XY extent of each branch separately.
#   CBH = lowest z_min of a crown cell in a valid branch (XY extent >= threshold)
#
# CHANGELOG v3.0.4:
# - REFACTORED: Cleaned up code, removed obsolete functions
# - NEW: GAB v4 uses voxel coordinates (u,v,w) instead of original (x,y)
#   Tree center = wood voxel with min(w) for each cls
#   Voronoi for crown_voxels based on (u,v), not (x,y)
# - REMOVED: Old CBH functions (.calculate_cbh_float, .calculate_cbh_gab_v3)
# - REMOVED: Old DAV function (.classify_dav_float)
# - REMOVED: Standalone hex functions (integrated in GAB v4)
# - SIMPLIFIED: Single version of each function, cleaner codebase
#
# CHANGELOG v3.0.3:
# - BFS algorithm for CBH calculation starting from trunk cells
# - Vertical gap tolerance increased to 5 layers
# - Full 3D adjacency (6 lateral + vertical with gap tolerance)
#
# CHANGELOG v3.0.2:
# - CHANGED: H_threshold_understory formula simplified
#   Formula: H_median - (0.70 * H_median) = H_median * 0.30
#   Crown occupies top 70% of median tree height
# - REMOVED: Hardcoded minimum of 5.0m for H_threshold_understory
#   Now uses adaptive calculation based on actual tree heights
#
# CHANGELOG v3.0.1:
# - NEW: Integrated GAB Voronoi + Parallel CBH calculation
# - NEW: Hexagonal tessellation for crown analysis (replaces polar sectors)
# - NEW: Crown + Wood voxel integration for complete spatial continuity
# - NEW: Parallel processing for CBH (multi-core support)
# - NEW: 2D/3D Voronoi crown assignment options
# - IMPROVED: Trunk -> Branches -> Foliage connectivity tracing
# - IMPROVED: More accurate CBH estimation via BFS on hexagonal graph
#
# CHANGELOG v2.8:
# - REFACTORED: Uses shared_utils.R for common operations
# - NEW: non_valid_trees layer for clusters that fail DBH validation
# - SIMPLIFIED: Removed duplicate code, references shared functions
# - CLEANER: Better separation of concerns
#
# CHANGELOG v2.7:
# - COMPLETE FLOAT: Eliminated ALL integer coordinate references
# - OPTIMIZED VOXELIZATION: No mean() calculations, only counts + deterministic centroids
# - PERFORMANCE: 10-100x faster voxelization
# - SIMPLIFIED: Only float coordinates (x,y,z) throughout entire pipeline
# - MAINTAINED: Global shift for numerical precision (stored as attributes)
# - MAINTAINED: Reverse shift applied only to final outputs
#
# Author: Roberto Ferrara, CNR-IBE
# ==============================================================================

#' @title Forest component segmentation, plot and tree metrics
#' @name Forest_seg
#' @description
#' \code{Forest_seg()} is the main end-to-end routine of the package. It takes a
#' raw terrestrial or mobile laser scanning (TLS/MLS) point cloud and runs a
#' complete eight-stage pipeline that reconstructs the forest floor, recognises
#' the woody structures, measures individual trees, separates crown from
#' understory, and writes fully classified point clouds together with per-tree
#' and per-plot reports and a reproducibility log.
#'
#' All processing is performed on native float coordinates. A single global
#' shift is applied at the start for numerical precision and reversed once at
#' the end, so that output coordinates are returned in the original reference
#' system. The wood and crown analysis relies on three dedicated algorithms,
#' \strong{LOR} (Ligneous Object Recognition), \strong{DAV} (Directional
#' Anisotropy of Vegetation) and \strong{GAB} (Geometrical Aggregation of
#' Biomass), described in the Details section.
#' 
#' @param a Input point cloud data frame (.xyz format) or file path
#' @param filename Output file prefix (default = "XXX")
#' @param integer_precision Coordinate decimal precision: "mm" (3 decimals) or "cm" (2 decimals)
#' @param dtm_coarse_res Coarse DTM/DSM resolution in m (default = 0.5).
#'   Fine resolution is automatically set to half. DSM uses same resolution for height calculation.
#' @param tolerance Vertical tolerance for floor extraction in m (default = 0.4)
#' @param dimVox Voxel dimension in cm for LOR wood segmentation (default = 2)
#' @param th Minimum points per voxel (default = 2)
#' @param eps DBSCAN epsilon radius in voxel units (default = 2)
#' @param mpts DBSCAN minimum points (default = 9)
#' @param h_trunk Minimum trunk length in m (default = 3)
#' @param N Minimum voxels in wood cluster (default = 3000)
#' @param w_linear Wood cluster quality filter (default = 0.5)
#' @param Vox_print Save voxelization? (default = FALSE)
#' @param Woodpoints_print Save wood points? (default = TRUE)
#' @param h_rescue_min Minimum normalized height (m above DTM) for RESCUE PASS wood recovery (default = 1)
#' @param h_rescue_max Maximum normalized height (m above DTM) for RESCUE PASS wood recovery (default = 5)
#' @param output_path Output directory (default = tempdir())
#' @param generate_reports Generate CSV reports? (default = TRUE)
#' @param canopy_vox_dim Voxel size for DAV (Directional Anisotropy of Vegetation) analysis in m (default = 0.15)
#' @param canopy_min_density Minimum canopy density in pts/m^3 (default = 500)
#' @param dav_understory_max_start Maximum start height for understory in m (default = 1.3)
#' @param dav_height_factor Internal DAV parameter. Upper fraction of median tree height for understory ceiling (default = 0.8, stable value).
#' @param dav_median_height_factor Internal DAV parameter. Fraction of median tree height for mass distribution threshold (default = 0.50, stable value).
#' @param dav_eps DBSCAN epsilon for DAV (default = 2)
#' @param dav_minPts DBSCAN minPts for DAV (default = 4)
#' @param calculate_cbh Calculate crown base height? (default = TRUE)
#' @param cbh_hex_side Hexagon edge length in m (default = 0.15)
#' @param cbh_min_branch_length Minimum horizontal extent of foliage in m (default = 2.0)
#' @param cbh_save_points Save CBH points? (default = TRUE)
#' @param ... Currently unused; reserved for future extensions
#' @param dbh_tolerance Vertical tolerance for DBH slice in m (default = 0.05)
#' @param dbh_max_rmse Maximum RMSE for DBH fit in cm (default = 5)
#' @param dbh_min_radius Minimum valid DBH radius in m (default = 0.025, i.e. 5 cm DBH)
#' @param dbh_max_radius Maximum valid DBH radius in m (default = 0.5, i.e. 100 cm DBH)
#' @param output_format Output format: "las" (single classified LAS file, default) or "xyz" (separate files per class). Any value other than "xyz" falls back to "las".
#'
#' @return A named list with the in-memory \code{tree_metrics} table, the paths
#'   of the files written (\code{tree_report_file}, \code{plot_report_file},
#'   \code{las_file} or, in XYZ mode, the per-class \code{crown_file},
#'   \code{understory_file}, \code{wood_file}, \code{noiseL_file},
#'   \code{noiseH_file}, \code{non_valid_trees_file}), the \code{parameters_log}
#'   path, and \code{shift_info} (the applied coordinate shift).
#'
#' @details
#' \strong{Processing pipeline (8 stages)}
#' \enumerate{
#'   \item \emph{Load and shift} - the input (data frame or \code{.xyz} /
#'     \code{.txt} / \code{.las} / \code{.laz} file) is coerced to an
#'     \code{x, y, z} table and a global coordinate shift is applied.
#'   \item \emph{Forest floor extraction} - a coarse/fine ground model (DTM)
#'     separates the forest floor from low vegetation and above-ground biomass
#'     (AGB).
#'   \item \emph{LOR (Ligneous Object Recognition)} - woody points (trunks and
#'     branches) are identified.
#'   \item \emph{Tree detection and metrics} - tree positions and heights are
#'     derived from the woody clusters.
#'   \item \emph{DBH} - diameter at breast height is fitted at multiple heights.
#'   \item \emph{DAV (Directional Anisotropy of Vegetation)} - foliage is split
#'     into tree crown and understory.
#'   \item \emph{GAB (Geometrical Aggregation of Biomass)} - crown base height
#'     (CBH) is estimated.
#'   \item \emph{Output} - the classified point cloud, the reports and the log
#'     are written.
#' }
#'
#' \strong{Point classification scheme}
#'
#' Every point is assigned an integer class, consistent with the codes stored in
#' the classified LAS file:
#' \itemize{
#'   \item \code{2} Forest floor (ground / vegetal soil)
#'   \item \code{3} Understory (low vegetation and shrubs below the crown)
#'   \item \code{4} Wood (trunks and branches)
#'   \item \code{5} Crown / foliage
#'   \item \code{6} Non-valid trees (clusters that failed DBH validation)
#'   \item \code{7} Noise (isolated or non-classifiable points)
#' }
#'
#' \strong{LOR - Ligneous Object Recognition (stage 3)}
#'
#' AGB points are voxelised (\code{dimVox}, \code{th}) and grouped with a
#' density-based clustering (DBSCAN, parameters \code{eps} / \code{mpts}).
#' Clusters are kept as woody structures when they exceed a minimum size
#' (\code{N}), satisfy a linearity/quality filter (\code{w_linear}) and reach a
#' minimum trunk length (\code{h_trunk}). A dedicated rescue pass
#' (\code{h_rescue_min}-\code{h_rescue_max}) recovers woody points in the height
#' band where trunks and low branches are most often missed.
#'
#' \strong{DAV - Directional Anisotropy of Vegetation (stage 6)}
#'
#' The remaining foliage is voxelised at \code{canopy_vox_dim} and filtered by a
#' minimum density (\code{canopy_min_density}). Voxels are clustered
#' (\code{dav_eps} / \code{dav_minPts}) and split into crown and understory using
#' an adaptive height threshold derived from the median tree height
#' (\code{dav_height_factor}, \code{dav_median_height_factor}, capped by
#' \code{dav_understory_max_start}). Low vegetation from the forest floor stage
#' is assigned directly to the understory. The voxel classification is then
#' mapped back to the original points, and crown and understory volumes are
#' accumulated for the plot report.
#'
#' \strong{GAB - Geometrical Aggregation of Biomass / CBH (stage 7)}
#'
#' When \code{calculate_cbh = TRUE}, crown and wood voxels are merged for spatial
#' continuity and projected onto a hexagonal tessellation (\code{cbh_hex_side}).
#' Each foliage cell is assigned to the nearest tree by Voronoi partitioning on
#' voxel coordinates, and a breadth-first connectivity trace (trunk to branches
#' to foliage) identifies the lowest continuously foliated branch, whose height
#' is reported as the crown base height. The step runs in parallel across the
#' available cores. A minimum foliage extent (\code{cbh_min_branch_length})
#' guards against spurious detections.
#'
#' \strong{DBH measurement (stage 5)}
#'
#' Diameter is estimated by circle fitting (Pratt/Landau) on horizontal slices
#' at 1.3 m, with fallbacks at 1.8 m and 2.3 m. A fit is accepted only within the
#' radius range (\code{dbh_min_radius}-\code{dbh_max_radius}) and below the
#' maximum RMSE (\code{dbh_max_rmse}); trees failing all heights are flagged as
#' non-valid (class 6).
#'
#' \strong{Outputs}
#' \itemize{
#'   \item \emph{Classified point cloud} - by default a single classified LAS
#'     file (\code{output_format = "las"}) carrying the class codes above; with
#'     \code{output_format = "xyz"} one plain-text file per class is written
#'     instead.
#'   \item \emph{Tree report} (\code{<filename>_tree_report.csv}) - one row per
#'     tree: identifier, class, position (X, Y, Z), height, DBH and its RMSE,
#'     validity flag, and CBH when computed.
#'   \item \emph{Plot report} (\code{<filename>_plot_report.csv}) - stand-level
#'     statistics including tree density, basal area, crown and understory
#'     volume, and canopy coverage.
#'   \item \emph{CBH diagnostics} - per-tree CBH details, written when
#'     \code{calculate_cbh = TRUE}.
#'   \item \emph{Parameters log} - a complete record of every parameter (exposed
#'     and internal), the input summary, the coordinate shift, the
#'     software/system versions and the timing, for full reproducibility.
#' }
#' Report and log writing is controlled by \code{generate_reports}.
#'
#' \strong{Fixed internal parameters (not exposed in the interface)}
#' \itemize{
#'   \item \code{dtm_fine_res}: automatically set to \code{dtm_coarse_res / 2}
#'   \item \code{dbh_heights}: fixed at \code{c(1.3, 1.8, 2.3)} metres
#'   \item \code{dbh_min_points}: fixed at 8 (works well for both Pratt and
#'     Landau fitting)
#' }
#'
#' @export
Forest_seg <- function(a, 
                       filename = "XXX",
                       integer_precision = "mm",
                       
                       # =====================================================
                       # DTM - Terrain extraction
                       # =====================================================
                       dtm_coarse_res = 0.5,
                       #dtm_fine_res = 0.1,
                       tolerance = 0.4,
                       # =====================================================
                       # WOOD - Trunk and branch detection
                       # =====================================================
                       dimVox = 2, 
                       th = 2,
                       eps = 2, 
                       mpts = 9, 
                       h_trunk = 3,
                       N = 3000, 
                       w_linear = 0.5,
                       Vox_print = FALSE,
                       Woodpoints_print = TRUE,
                       h_rescue_min = 1,
                       h_rescue_max = 5,
                       
                       # =====================================================
                       # DBHx - Multi-height diameter measurement
                       # =====================================================
                       #dbh_heights = c(1.3, 1.8, 2.3),
                       dbh_tolerance = 0.05,
                       dbh_max_rmse = 5,
                       dbh_min_radius = 0.025,
                       dbh_max_radius = 0.5,
                       #dbh_min_points = 8,
                       
                       # =====================================================
                       # DAV - Crown-understory separation
                       # =====================================================
                       canopy_vox_dim = 0.15,
                       canopy_min_density = 500,  # <- SHARED with CBH
                       dav_understory_max_start = 1.3,
                       dav_height_factor = 0.8,
                       dav_median_height_factor = 0.50,
                       dav_eps = 2,
                       dav_minPts = 4,

                       # =====================================================
                       # CBH - Crown base height (GAB - Forest_seg 3.3)
                       # =====================================================
                       # ONLY 2 USER PARAMETERS (v3.0.4: removed cbh_layer_height)
                       calculate_cbh = TRUE,
                       cbh_hex_side = 0.15,           # Spatial resolution
                       cbh_min_branch_length = 2.0,   # Min foliage extent in XY (m)
                       cbh_save_points = TRUE,

                       # CBH AUTO-CALCULATED (not exposed):
                       # - cbh_min_cluster_cells: ceiling(cbh_min_branch_length / cbh_hex_side)
                       # - cbh_min_points: f(hex_side, canopy_vox_dim, canopy_min_density)
                       #
                       # CBH v3.0.4: Uses voxel coordinates (u,v,w)
                       # - Tree center = wood voxel with min(w) for each cls
                       # - Voronoi assignment on (u,v) coordinates
                       # - layer = w (voxel z-coordinate, no separate layer_height)
                       #
                       # CBH FIXED (not exposed):
                       # - max_gap_layers = 2
                       # - n_cores = detectCores() - 1
                       
                       # =====================================================
                       # OUTPUT - File generation
                       # =====================================================
                       output_format = "las",
                       output_path = tempdir(),
                       generate_reports = TRUE,
                       ...) {

  # ===========================================================================
  # FIXED INTERNAL PARAMETERS (not exposed in Shiny interface)
  # ===========================================================================
  dtm_fine_res <- dtm_coarse_res / 2    # Fine DTM = half of coarse
  floor_vox_th <- 20L                   # Min points per voxel to be valid floor candidate
  floor_min_cluster <- 100L             # Min voxels per DBSCAN cluster to keep as floor
  dbh_heights <- c(1.3, 1.8, 2.3)        # DBH heights: primary 1.3 m, fallbacks 1.8 / 2.3 m
  dbh_min_points <- 8                   # Minimum points for circle fitting (Pratt/Landau)

  old_opts <- options(digits = 22, scipen = 999)
  on.exit(options(old_opts), add = TRUE)
  
  tic_total <- Sys.time()
  
  # ===========================================================================
  # SETUP
  # ===========================================================================
  
  plot_id <- filename
  
  # Decimal precision setup (for coordinate rounding)
  COORD_DECIMALS <- switch(integer_precision,
                           "mm" = 3L,  # millimeter precision
                           "cm" = 2L,  # centimeter precision
                           stop("integer_precision must be 'mm' or 'cm'"))

  # Output format: LAS is the default. Only an explicit "xyz" produces separate
  # per-class files; any other value (NULL, typo, legacy "txt", ...) falls back to LAS.
  output_format <- if (length(output_format) == 1L &&
                       identical(tolower(as.character(output_format)), "xyz")) {
    "xyz"
  } else {
    "las"
  }
  
  message("\n=============================================================================")
  message("  Forest_seg 3.3")
  message("=============================================================================")
  message("Plot ID: ", plot_id)
  message("Coordinate precision: ", integer_precision, " (", COORD_DECIMALS, " decimals)")
  message("Unified density: ", canopy_min_density, " pts/m\u00b3")
  
  # ===========================================================================
  # STAGE 1: LOAD DATA AND APPLY GLOBAL SHIFT
  # ===========================================================================
  
  message("\nStage 1/8: Loading point cloud...")
  # Uses shared function from shared_utils.R
  point_cloud <- .coerce_to_xyz_dt(a)
  
  n_points <- nrow(point_cloud)
  message("  Points loaded: ", format(n_points, big.mark = ","))
  
  # Apply global shift for numerical stability
  point_cloud <- .apply_global_shift(point_cloud, COORD_DECIMALS)
  shift_values <- .get_shift_values(point_cloud)
  
  message("  Global shift: x0=", shift_values$x_shift, 
          ", y0=", shift_values$y_shift, 
          ", z0=", shift_values$z_shift)
  
  # ===========================================================================
  # STAGE 2: DTM EXTRACTION
  # ===========================================================================
  
  message("\nStage 2/8: DTM extraction...")
  dtm_result <- .extract_dtm_float_optimized(
    point_cloud, dtm_coarse_res, dtm_fine_res,
    tolerance, floor_vox_th, floor_min_cluster, COORD_DECIMALS
  )
  
  Forest_floor <- dtm_result$floor_points
  low_vegetation <- dtm_result$low_vegetation
  AGB <- dtm_result$AGB
  dtm_grid <- dtm_result$dtm_grid
  dsm_grid <- dtm_result$dsm_grid

  message("  Floor points: ", format(nrow(Forest_floor), big.mark = ","))
  message("  Low vegetation: ", format(nrow(low_vegetation), big.mark = ","))
  message("  AGB points: ", format(nrow(AGB), big.mark = ","))

  # ===========================================================================
  # STAGE 3: LOR (Ligneous Object Recognition)
  # ===========================================================================
  
  message("\nStage 3/8: LOR (Ligneous Object Recognition)...")
  wood_result <- .lor_segment(
    AGB, dimVox, th, eps, mpts,
    h_trunk, N, w_linear, Vox_print, output_path, plot_id,
    dtm_grid, dtm_fine_res,        # <- RESCUE PASS PARAMETERS
    h_rescue_min = h_rescue_min, h_rescue_max = h_rescue_max,
    decimals = COORD_DECIMALS
  )
  
  Tronchi <- wood_result$tree_bases
  Woodpoints <- wood_result$wood_points_valid
  AGB_with_cls <- wood_result$AGB_with_cls

  # Add z_norm to Woodpoints (needed for CBH calculation)
  # Note: dtm_grid uses dtm_fine_res for gx_fine/gy_fine
  Woodpoints[, ':='(
    gx_fine = as.integer(floor(x / dtm_fine_res)),
    gy_fine = as.integer(floor(y / dtm_fine_res))
  )]
  Woodpoints[dtm_grid, z_dtm := i.z_dtm, on = .(gx_fine, gy_fine)]
  Woodpoints[, z_norm := z - z_dtm]
  Woodpoints[, c("gx_fine", "gy_fine", "z_dtm") := NULL]  # Clean up temp columns

  # ===========================================================================
  # VOXEL-BASED WOOD/FOLIAGE SEPARATION (canopy_vox_dim resolution)
  # ===========================================================================
  # Voxelize both AGB and Wood at 15cm resolution, then subtract

  message("  Voxelizing for wood/foliage separation (", canopy_vox_dim * 100, " cm)...")

  # Remove old voxel coords from LOR (2cm) and recalculate at 15cm
  # AGB_with_cls has u,v,w from LOR - remove them
  if ("u" %in% names(AGB_with_cls)) AGB_with_cls[, u := NULL]
  if ("v" %in% names(AGB_with_cls)) AGB_with_cls[, v := NULL]
  if ("w" %in% names(AGB_with_cls)) AGB_with_cls[, w := NULL]

  # Voxelize ALL AGB points at canopy_vox_dim resolution
  AGB_with_cls[, ':='(
    u_15 = as.integer(floor(x / canopy_vox_dim)) + 1L,
    v_15 = as.integer(floor(y / canopy_vox_dim)) + 1L,
    w_raw = as.integer(floor(z / canopy_vox_dim)) + 1L
  )]

  AGB_voxels <- AGB_with_cls[, .(
    N = .N,
    x = mean(x),
    y = mean(y),
    z = mean(z)
  ), by = .(u = u_15, v = v_15, w_raw)]

  message("    AGB voxels: ", format(nrow(AGB_voxels), big.mark = ","))

  # Voxelize Woodpoints at same resolution
  Woodpoints[, ':='(
    u = as.integer(floor(x / canopy_vox_dim)) + 1L,
    v = as.integer(floor(y / canopy_vox_dim)) + 1L,
    w_raw = as.integer(floor(z / canopy_vox_dim)) + 1L
  )]

  wood_voxel_keys <- unique(Woodpoints[, .(u, v, w_raw)])
  message("    Wood voxels: ", format(nrow(wood_voxel_keys), big.mark = ","))

  # Subtract: AGB_def = AGB voxels NOT occupied by wood
  AGB_voxels[, is_wood := FALSE]
  AGB_voxels[wood_voxel_keys, is_wood := TRUE, on = .(u, v, w_raw)]

  AGB_def_voxels <- AGB_voxels[is_wood == FALSE, .(u, v, w_raw, N, x, y, z)]

  message("    AGB_def voxels (foliage): ", format(nrow(AGB_def_voxels), big.mark = ","))
  message("    Removed wood voxels: ", format(nrow(AGB_voxels) - nrow(AGB_def_voxels), big.mark = ","))

  # Clean up temporary columns (keep u_15, v_15, w_raw in AGB_with_cls for point mapping)
  AGB_voxels[, is_wood := NULL]
  Woodpoints[, c("u", "v", "w_raw") := NULL]

  # Extract foliage POINTS (not wood) for later mapping back from voxel classification
  # These are the original points that will be mapped to DAV clusters
  foliage_points <- AGB_with_cls[is.na(cls), .(x, y, z, u = u_15, v = v_15, w_raw)]

  # Now clean up AGB_with_cls
  AGB_with_cls[, c("u_15", "v_15", "w_raw") := NULL]

  message("  Trees detected: ", nrow(Tronchi))
  message("  Wood points: ", format(nrow(Woodpoints), big.mark = ","))
  message("  Foliage points for DAV: ", format(nrow(foliage_points), big.mark = ","))
  message("  AGB_def voxels: ", format(nrow(AGB_def_voxels), big.mark = ","))

  # Free memory (AGB_voxels no longer needed)
  rm(AGB_voxels, wood_voxel_keys); gc(verbose = FALSE)
  
  # ===========================================================================
  # STAGE 4: TREE METRICS
  # ===========================================================================
  
  message("\nStage 4/8: Calculating tree metrics...")
  tree_metrics <- .calculate_tree_metrics_float(
    Tronchi, Woodpoints, dtm_grid, dsm_grid,
    COORD_DECIMALS, dtm_fine_res, dtm_coarse_res
  )
  
  # ===========================================================================
  # STAGE 5: DBH CALCULATION
  # ===========================================================================
  
  message("\nStage 5/8: Calculating DBH (multi-height system)...")
  tree_metrics <- .calculate_dbhx_float(
    tree_metrics, Woodpoints,
    dbh_heights, dbh_tolerance, dbh_max_rmse,
    dbh_min_radius, dbh_max_radius, dbh_min_points,
    COORD_DECIMALS
  )
  
  valid_dbh_count <- sum(tree_metrics$DBH_valid, na.rm = TRUE)
  message("  Valid DBH: ", valid_dbh_count, " / ", nrow(tree_metrics))
  
  # Convert to cm for output
  tree_metrics[, ':='(
    DBH_cm = round(DBH * 100, 1),
    DBH_RMSE_cm = round(DBH_RMSE * 100, 1)
  )]
  
  # ===========================================================================
  # STAGE 5.5: MARK AND SEPARATE INVALID DBH CLUSTERS AS NON_VALID_TREES
  # ===========================================================================

  message("\nStage 5.5/8: Processing invalid DBH clusters...")
  invalid_clusters <- tree_metrics[DBH_valid == FALSE, cls]

  # Initialize non_valid_trees points (for separate file export)
  non_valid_trees_points <- data.table(x = numeric(0), y = numeric(0), z = numeric(0), cls = integer(0))

  # Add valid_tree column to tree_metrics (all trees keep their metrics)
  tree_metrics[, valid_tree := DBH_valid]

  if (isTRUE(length(invalid_clusters) > 0)) {
    # Extract points from invalid clusters for separate layer export
    non_valid_trees_points <- Woodpoints[cls %in% invalid_clusters, .(x, y, z, cls)]

    # Remove invalid cluster points from main Woodpoints (for wood_valid.xyz)
    Woodpoints_valid <- Woodpoints[!(cls %in% invalid_clusters)]

    message("  Invalid clusters identified: ", length(invalid_clusters))
    message("  Non-valid tree points: ", format(nrow(non_valid_trees_points), big.mark = ","))
    message("  Valid trees: ", sum(tree_metrics$valid_tree))
    message("  Total trees in report: ", nrow(tree_metrics))
  } else {
    Woodpoints_valid <- Woodpoints
    message("  All clusters passed DBH validation")
  }

  # Keep all tree_metrics for report (both valid and invalid)
  # Only use Woodpoints_valid for wood_valid.xyz export
  
  # ===========================================================================
  # STAGE 6: DAV (Directional Anisotropy of Vegetation)
  # ===========================================================================

  message("\nStage 6/8: DAV (Directional Anisotropy of Vegetation)...")

  if (nrow(AGB_def_voxels) == 0) {
    message("  No AGB voxels for DAV classification")
    crown_points <- data.table(x = numeric(0), y = numeric(0), z = numeric(0), cluster_id = integer(0))
    understory_points <- data.table(x = numeric(0), y = numeric(0), z = numeric(0))
    noiseL_points <- data.table(x = numeric(0), y = numeric(0), z = numeric(0))
    noiseH_points <- data.table(x = numeric(0), y = numeric(0), z = numeric(0))
  } else {
    # NOTE: low_vegetation goes DIRECTLY to understory, NOT through DAV
    # DAV only processes foliage points (AGB_def without low_vegetation)

    # =========================================================================
    # HEIGHT NORMALIZATION of foliage voxels (required by DAV v3.1)
    # AGB_def_voxels has w_raw (raw height), DAV expects w (DTM-normalized)
    # Same pattern as .lor_create_voxels() lines 2493-2506
    # =========================================================================

    AGB_def_voxels[, ':='(
      gx_fine = as.integer(floor(x / dtm_fine_res)),
      gy_fine = as.integer(floor(y / dtm_fine_res))
    )]

    AGB_def_voxels[dtm_grid, z_dtm := i.z_dtm, on = .(gx_fine, gy_fine)]

    if (any(is.na(AGB_def_voxels$z_dtm))) {
      dtm_mean <- mean(dtm_grid$z_dtm, na.rm = TRUE)
      AGB_def_voxels[is.na(z_dtm), z_dtm := dtm_mean]
    }

    AGB_def_voxels[, z_norm := z - z_dtm]
    AGB_def_voxels[, w := as.integer(floor(z_norm / canopy_vox_dim)) + 1L]

    # Re-aggregate by normalized coordinates (multiple w_raw may map to same w)
    AGB_def_voxels <- AGB_def_voxels[, .(
      N = sum(N),
      x = mean(x),
      y = mean(y),
      z = mean(z),
      z_norm = mean(z_norm)
    ), by = .(u, v, w)]

    message("  Normalized foliage voxels: ", format(nrow(AGB_def_voxels), big.mark = ","))

    # =========================================================================
    # DAV CLASSIFICATION
    # =========================================================================

    dav_result <- .classify_dav_voxels(
      crown_voxels = AGB_def_voxels,
      tree_metrics = tree_metrics,
      canopy_vox_dim = canopy_vox_dim,
      canopy_min_density = canopy_min_density,
      dav_understory_max_start = dav_understory_max_start,
      dav_height_factor = dav_height_factor,
      dav_median_height_factor = dav_median_height_factor,
      dav_eps = dav_eps,
      dav_minPts = dav_minPts,
      decimals = COORD_DECIMALS
    )

    # --- PATCH: Reconversion voxels -> original points --------------------
    #
    # foliage_points has (x, y, z, u, v, w_raw) where w_raw = floor(z / vox) + 1
    # DAV classifies on (u, v, w) where w = floor(z_norm / vox) + 1 (normalized)
    # We must compute w_norm on each foliage point to match the DAV voxel keys.
    # -----------------------------------------------------------------------

    if (!is.null(dav_result$crown)) {

      # 1. Compute normalized w for each foliage point (same logic as lines 467-480)
      foliage_pts_map <- copy(foliage_points)

      foliage_pts_map[, ':='(
        gx_fine = as.integer(floor(x / dtm_fine_res)),
        gy_fine = as.integer(floor(y / dtm_fine_res))
      )]
      foliage_pts_map[dtm_grid, z_dtm := i.z_dtm, on = .(gx_fine, gy_fine)]

      if (any(is.na(foliage_pts_map$z_dtm))) {
        dtm_mean <- mean(dtm_grid$z_dtm, na.rm = TRUE)
        foliage_pts_map[is.na(z_dtm), z_dtm := dtm_mean]
      }

      foliage_pts_map[, z_norm := z - z_dtm]
      foliage_pts_map[, w_norm := as.integer(floor(z_norm / canopy_vox_dim)) + 1L]

      # 2. Build unique classified voxel keys from DAV result
      crown_vox_idx      <- unique(dav_result$crown[, .(u, v, w)])
      understory_vox_idx <- unique(dav_result$understory[, .(u, v, w)])
      noiseL_vox_idx     <- unique(dav_result$noiseL[, .(u, v, w)])
      noiseH_vox_idx     <- unique(dav_result$noiseH[, .(u, v, w)])

      # 3. Join on (u, v, w_norm == w) -- correct normalized match
      foliage_pts_map[, class := NA_character_]
      foliage_pts_map[crown_vox_idx,      class := "crown",      on = .(u, v, w_norm = w)]
      foliage_pts_map[understory_vox_idx, class := "understory", on = .(u, v, w_norm = w)]
      foliage_pts_map[noiseL_vox_idx,     class := "noiseL",     on = .(u, v, w_norm = w)]
      foliage_pts_map[noiseH_vox_idx,     class := "noiseH",     on = .(u, v, w_norm = w)]

      # 4. Points not matched by any DAV voxel (e.g. filtered by density)
      #    classify by height: low -> noiseL, high -> noiseH
      n_unmatched <- sum(is.na(foliage_pts_map$class))
      if (n_unmatched > 0) {
        # H_threshold may be absent if DAV took an early return (empty stats);
        # fall back to the configured understory ceiling so the split is always valid.
        H_threshold_understory <- if (!is.null(dav_result$stats$H_threshold))
          dav_result$stats$H_threshold[1] else NA_real_
        if (is.na(H_threshold_understory)) {
          H_threshold_understory <- dav_understory_max_start
        }
        foliage_pts_map[is.na(class) & z_norm <  H_threshold_understory, class := "noiseL"]
        foliage_pts_map[is.na(class) & z_norm >= H_threshold_understory, class := "noiseH"]
        message(sprintf("    Unmatched points classified as noise: %d (%.1f%%)",
                        n_unmatched, 100 * n_unmatched / nrow(foliage_pts_map)))
      }

      # 5. Split into components (only original coordinates)
      crown_points      <- foliage_pts_map[class == "crown",      .(x, y, z)]
      understory_points <- foliage_pts_map[class == "understory", .(x, y, z)]
      noiseL_points     <- foliage_pts_map[class == "noiseL",     .(x, y, z)]
      noiseH_points     <- foliage_pts_map[class == "noiseH",     .(x, y, z)]

      # 6. Conservation check
      n_total_foliage <- nrow(foliage_points)
      n_total_classified <- nrow(crown_points) + nrow(understory_points) +
                            nrow(noiseL_points) + nrow(noiseH_points)
      message(sprintf("  Point-level reclassification completed:"))
      message(sprintf("    Crown:      %s", format(nrow(crown_points), big.mark = ",")))
      message(sprintf("    Understory: %s", format(nrow(understory_points), big.mark = ",")))
      message(sprintf("    Noise low:  %s", format(nrow(noiseL_points), big.mark = ",")))
      message(sprintf("    Noise high: %s", format(nrow(noiseH_points), big.mark = ",")))
      message(sprintf("    TOTAL:      %s / %s foliage points (%s)",
                      format(n_total_classified, big.mark = ","),
                      format(n_total_foliage, big.mark = ","),
                      ifelse(n_total_classified == n_total_foliage, "OK", "MISMATCH!")))

      # Clean up temporary columns
      rm(foliage_pts_map); gc(verbose = FALSE)

    } else {
      warning("DAV result empty: skipping voxel to point mapping.")
      crown_points      <- data.table(x = numeric(0), y = numeric(0), z = numeric(0))
      understory_points <- crown_points
      noiseL_points     <- crown_points
      noiseH_points     <- crown_points
    }
    # --- END PATCH -------------------------------------------------------
    

    # Add low_vegetation DIRECTLY to understory (bypass DAV)
    if (isTRUE(nrow(low_vegetation) > 0)) {
      low_veg_pts <- low_vegetation[, .(x, y, z, z_norm = 0, cluster_id = 0L)]
      understory_points <- rbindlist(list(understory_points, low_veg_pts), use.names = TRUE, fill = TRUE)
      message("  Low vegetation added to understory: ", format(nrow(low_vegetation), big.mark = ","))
    }

    message("  Crown points: ", format(nrow(crown_points), big.mark = ","))
    message("  Understory points: ", format(nrow(understory_points), big.mark = ","))
    message("  Noise low: ", format(nrow(noiseL_points), big.mark = ","))
    message("  Noise high: ", format(nrow(noiseH_points), big.mark = ","))
  }
  
  # =============================================================================
  # STAGE 7: GAB (Geometrical Aggregation of Biomass) - CBH CALCULATION
  # =============================================================================

  cbh_results <- NULL

  # Pre-calculate CBH parameters (needed for logging even if CBH is skipped)
  # v3.0.4: layer = voxel w, so layer_height = canopy_vox_dim
  hex_area <- (3 * sqrt(3) / 2) * cbh_hex_side^2
  cell_volume <- hex_area * canopy_vox_dim  # v3.0.4: use voxel_size as layer height
  cbh_min_points <- as.integer(round(cell_volume * canopy_min_density))
  cbh_min_cluster_cells <- ceiling(cbh_min_branch_length / cbh_hex_side)

  if (calculate_cbh && isTRUE(nrow(crown_points) > 0) && isTRUE(nrow(tree_metrics) > 0)) {

    message("\nStage 7/8: GAB (Geometrical Aggregation of Biomass) - CBH...")

    message("  Hexagon side: ", cbh_hex_side, " m")
    message("  Cell volume: ", round(cell_volume, 5), " m\u00b3")
    message("  Min points/cell: ", cbh_min_points,
            " (from density: ", canopy_min_density, " pts/m\u00b3)")
    message("  Min cluster cells: ", cbh_min_cluster_cells,
            " (from branch_length=", cbh_min_branch_length, "m)")

    # ===========================================================================
    # CHECK: Need crown voxels from DAV
    # ===========================================================================

    if (!is.null(dav_result$crown) && nrow(dav_result$crown) > 0) {

      # =========================================================================
      # CREATE WOOD VOXELS FOR CBH (needed for trunk zone detection)
      # =========================================================================

      wood_voxels_cbh <- .lor_create_voxels(
        wood_points = Woodpoints,
        vox_dim = canopy_vox_dim,  # Use same resolution as crown voxels
        dtm_grid = dtm_grid,
        dtm_fine_res = dtm_fine_res,
        decimals = COORD_DECIMALS
      )
      message("  Wood voxels for CBH: ", nrow(wood_voxels_cbh))

      # =========================================================================
      # CALL GAB (Geometrical Aggregation of Biomass) - Forest_seg 3.3
      # =========================================================================

      cbh_results <- .calculate_cbh_gab_v4(
        crown_voxels = dav_result$crown,
        wood_voxels = wood_voxels_cbh,
        tree_metrics = tree_metrics,

        # NEW: voxel_size for coordinate conversion
        voxel_size = canopy_vox_dim,

        # User parameters
        hex_side = cbh_hex_side,
        min_cluster_cells = cbh_min_cluster_cells,  # Calculated from branch_length

        # Auto-calculated
        min_points = cbh_min_points,

        # Fixed
        max_gap_layers = 2,  # Handle gaps in sparse areas
        n_cores = NULL,  # Auto-detect
        decimals = COORD_DECIMALS
      )
      
      # =========================================================================
      # MERGE CBH RESULTS
      # =========================================================================
      
      if (!is.null(cbh_results$tree_cbh)) {
        
        # Merge ONLY CBH column (other columns are diagnostics)
        tree_metrics <- merge(
          tree_metrics, 
          cbh_results$tree_cbh[, .(cls, CBH)], 
          by = "cls", 
          all.x = TRUE
        )
        
        # Statistics
        n_valid_cbh <- sum(!is.na(tree_metrics$CBH) & 
                             tree_metrics$CBH > 0 & 
                             tree_metrics$CBH < 900)
        
        message("  CBH calculated for ", n_valid_cbh, " / ", 
                nrow(tree_metrics), " trees")
        
        # Optional: Save diagnostic info to separate file
        if (generate_reports) {
          cbh_diagnostics_file <- file.path(
            output_path, 
            paste0(plot_id, "_cbh_diagnostics.csv")
          )
          fwrite(cbh_results$tree_cbh, cbh_diagnostics_file, sep = ";")
          message("  CBH diagnostics saved: ", basename(cbh_diagnostics_file))
        }
      }
      
    } else {
      message("  No crown voxels available for CBH calculation")
    }
    
  } else {
    message("\nStage 7/8: CBH calculation skipped")
  }
  
  # ===========================================================================
  # STAGE 8: CANOPY ANALYSIS
  # ===========================================================================
  # Coverage area + crown/understory volumes, computed from the FINAL classified
  # points (re-voxelized at canopy_vox_dim) so they match what is exported.

  vox_vol_m3 <- canopy_vox_dim^3

  # Coverage area: 2D projection of crown points onto the XY plane
  coverage_area_m2 <- NA_real_
  if (isTRUE(nrow(crown_points) > 0)) {
    crown_xy <- unique(data.table(
      gx = as.integer(floor(crown_points$x / canopy_vox_dim)),
      gy = as.integer(floor(crown_points$y / canopy_vox_dim))
    ))
    coverage_area_m2 <- round(nrow(crown_xy) * canopy_vox_dim^2, 2)
  }

  # Crown volume: occupied 3D voxels of crown points x voxel volume
  crown_volume_m3 <- NA_real_
  if (isTRUE(nrow(crown_points) > 0)) {
    crown_vox <- unique(data.table(
      gx = as.integer(floor(crown_points$x / canopy_vox_dim)),
      gy = as.integer(floor(crown_points$y / canopy_vox_dim)),
      gz = as.integer(floor(crown_points$z / canopy_vox_dim))
    ))
    crown_volume_m3 <- round(nrow(crown_vox) * vox_vol_m3)
  }

  # Understory volume: occupied 3D voxels of understory points x voxel volume
  understory_volume_m3 <- NA_real_
  if (isTRUE(nrow(understory_points) > 0)) {
    under_vox <- unique(data.table(
      gx = as.integer(floor(understory_points$x / canopy_vox_dim)),
      gy = as.integer(floor(understory_points$y / canopy_vox_dim)),
      gz = as.integer(floor(understory_points$z / canopy_vox_dim))
    ))
    understory_volume_m3 <- round(nrow(under_vox) * vox_vol_m3)
  }

  message("  Crown volume: ", crown_volume_m3, " m\u00b3")
  message("  Understory volume: ", understory_volume_m3, " m\u00b3")

  # ===========================================================================
  # STAGE 9: PREPARE OUTPUTS WITH REVERSE SHIFT
  # ===========================================================================
  
  message("\nPreparing outputs (applying reverse shift)...")
  
  # Apply reverse shift to all outputs
  tree_metrics_out <- copy(tree_metrics)
  tree_metrics_out <- .reverse_global_shift(tree_metrics_out, shift_values)
  
  # Rename columns for backward compatibility
  setnames(tree_metrics_out, c("x", "y", "z"), c("X", "Y", "Z"))
  
  # Add tree numbering
  tree_metrics_out[, Tree_n := .I]
  
  # Export files with reverse shift
  crown_file <- NULL
  understory_file <- NULL
  noiseL_file <- NULL
  noiseH_file <- NULL
  wood_file <- NULL
  non_valid_trees_file <- NULL
  las_file <- NULL

  if (output_format == "las") {
    # =========================================================================
    # LAS OUTPUT: Single classified point cloud
    # ASPRS codes: floor=2, understory=3, wood=4, crown=5, noise=7
    # =========================================================================
    message("  Output format: LAS (single classified file)")

    parts <- list()

    if (isTRUE(nrow(Forest_floor) > 0)) {
      dt <- .reverse_global_shift(copy(Forest_floor), shift_values)[, .(x, y, z)]
      dt[, Classification := 2L]  # Ground
      parts <- c(parts, list(dt))
    }

    if (isTRUE(nrow(crown_points) > 0)) {
      dt <- .reverse_global_shift(copy(crown_points), shift_values)[, .(x, y, z)]
      dt[, Classification := 5L]  # High Vegetation
      parts <- c(parts, list(dt))
    }

    if (isTRUE(nrow(understory_points) > 0)) {
      dt <- .reverse_global_shift(copy(understory_points), shift_values)[, .(x, y, z)]
      dt[, Classification := 3L]  # Low Vegetation
      parts <- c(parts, list(dt))
    }

    if (Woodpoints_print && isTRUE(nrow(Woodpoints_valid) > 0)) {
      dt <- .reverse_global_shift(copy(Woodpoints_valid), shift_values)[, .(x, y, z)]
      dt[, Classification := 4L]  # Medium Vegetation (wood)
      parts <- c(parts, list(dt))
    }

    if (isTRUE(nrow(noiseL_points) > 0)) {
      dt <- .reverse_global_shift(copy(noiseL_points), shift_values)[, .(x, y, z)]
      dt[, Classification := 7L]  # Noise
      parts <- c(parts, list(dt))
    }

    if (isTRUE(nrow(noiseH_points) > 0)) {
      dt <- .reverse_global_shift(copy(noiseH_points), shift_values)[, .(x, y, z)]
      dt[, Classification := 7L]  # Noise
      parts <- c(parts, list(dt))
    }

    if (isTRUE(nrow(non_valid_trees_points) > 0)) {
      dt <- .reverse_global_shift(copy(non_valid_trees_points), shift_values)[, .(x, y, z)]
      dt[, Classification := 6L]  # Non valid trees, pending reassignment
      parts <- c(parts, list(dt))
    }

    if (isTRUE(length(parts) > 0)) {
      combined <- rbindlist(parts)
      setnames(combined, c("x", "y", "z"), c("X", "Y", "Z"))
      .require_lidR()
      las_obj <- lidR::LAS(as.data.frame(combined))
      las_file <- file.path(output_path, paste0(plot_id, "_classified.las"))
      lidR::writeLAS(las_obj, las_file)
      message("  LAS file: ", basename(las_file))
      message("    Total points: ", nrow(combined))
      message("    Classes: floor(2)=", sum(combined$Classification == 2L),
              " understory(3)=", sum(combined$Classification == 3L),
              " wood(4)=", sum(combined$Classification == 4L),
              " crown(5)=", sum(combined$Classification == 5L),
              " noise(7)=", sum(combined$Classification == 7L))
    }

  } else {
    # =========================================================================
    # XYZ OUTPUT: Separate files per class (original behavior)
    # =========================================================================
    message("  Output format: XYZ (separate files)")

    if (isTRUE(nrow(crown_points) > 0)) {
      crown_out <- .reverse_global_shift(copy(crown_points), shift_values)
      crown_out <- crown_out[, .(x, y, z)]
      crown_file <- file.path(output_path, paste0(plot_id, "_crown.xyz"))
      fwrite(crown_out, crown_file, sep = " ", col.names = FALSE)
      message("  Crown file: ", basename(crown_file))
    }

    if (isTRUE(nrow(understory_points) > 0)) {
      understory_out <- .reverse_global_shift(copy(understory_points), shift_values)
      understory_out <- understory_out[, .(x, y, z)]
      understory_file <- file.path(output_path, paste0(plot_id, "_understory.xyz"))
      fwrite(understory_out, understory_file, sep = " ", col.names = FALSE)
      message("  Understory file: ", basename(understory_file))
    }

    if (isTRUE(nrow(noiseL_points) > 0)) {
      noiseL_out <- .reverse_global_shift(copy(noiseL_points), shift_values)
      noiseL_out <- noiseL_out[, .(x, y, z)]
      noiseL_file <- file.path(output_path, paste0(plot_id, "_noiseL.xyz"))
      fwrite(noiseL_out, noiseL_file, sep = " ", col.names = FALSE)
      message("  NoiseL file: ", basename(noiseL_file))
    }

    if (isTRUE(nrow(noiseH_points) > 0)) {
      noiseH_out <- .reverse_global_shift(copy(noiseH_points), shift_values)
      noiseH_out <- noiseH_out[, .(x, y, z)]
      noiseH_file <- file.path(output_path, paste0(plot_id, "_noiseH.xyz"))
      fwrite(noiseH_out, noiseH_file, sep = " ", col.names = FALSE)
      message("  NoiseH file: ", basename(noiseH_file))
    }

    # Wood points (only valid trees)
    if (Woodpoints_print && isTRUE(nrow(Woodpoints_valid) > 0)) {
      wood_out <- .reverse_global_shift(copy(Woodpoints_valid), shift_values)
      wood_out <- wood_out[, .(x, y, z)]
      wood_file <- file.path(output_path, paste0(plot_id, "_wood_valid.xyz"))
      fwrite(wood_out, wood_file, sep = " ", col.names = FALSE)
      message("  Wood file: ", basename(wood_file))
    }

    # Non-valid trees (clusters that failed DBH validation)
    if (isTRUE(nrow(non_valid_trees_points) > 0)) {
      nvt_out <- .reverse_global_shift(copy(non_valid_trees_points), shift_values)
      cls_to_tree_n <- tree_metrics_out[valid_tree == FALSE, .(cls, Tree_n)]
      nvt_out <- merge(nvt_out, cls_to_tree_n, by = "cls", all.x = TRUE)
      nvt_out <- nvt_out[, .(x, y, z, Tree_n)]
      non_valid_trees_file <- file.path(output_path, paste0(plot_id, "_non_valid_trees.xyz"))
      fwrite(nvt_out, non_valid_trees_file, sep = " ", col.names = FALSE)
      message("  Non-valid trees file: ", basename(non_valid_trees_file))
    }

    # Floor points
    if (isTRUE(nrow(Forest_floor) > 0)) {
      floor_out <- .reverse_global_shift(copy(Forest_floor), shift_values)
      floor_out <- floor_out[, .(x, y, z)]
      floor_file <- file.path(output_path, paste0(plot_id, "_floor.xyz"))
      fwrite(floor_out, floor_file, sep = " ", col.names = FALSE)
      message("  Floor file: ", basename(floor_file))
    }
  }
  
  # ===========================================================================
  # GENERATE REPORTS
  # ===========================================================================
  
  tree_report_file <- NULL
  plot_report_file <- NULL
  
  if (generate_reports && isTRUE(nrow(tree_metrics_out) > 0)) {
    # Tree report - includes both valid and invalid trees with all metrics
    tree_cols <- c("Tree_n", "cls", "X", "Y", "Z", "Height",
                   "DBH_cm", "DBH_RMSE_cm", "valid_tree")
    if ("CBH" %in% names(tree_metrics_out)) tree_cols <- c(tree_cols, "CBH")

    tree_out <- copy(tree_metrics_out[, ..tree_cols])
    setnames(tree_out,
             c("DBH_cm", "DBH_RMSE_cm"),
             c("DBH (cm)", "RMSE (cm)"))

    tree_report_file <- file.path(output_path, paste0(plot_id, "_tree_report.csv"))
    fwrite(tree_out, tree_report_file, sep = ";")
    message("  Tree report: ", basename(tree_report_file))
    
    # Plot statistics (use crown+understory+noise as AGB_def equivalent)
    all_foliage_points <- rbindlist(list(
      crown_points[, .(x, y, z)],
      understory_points[, .(x, y, z)],
      noiseL_points[, .(x, y, z)],
      noiseH_points[, .(x, y, z)]
    ), use.names = TRUE)

    plot_stats <- .calculate_plot_statistics_float(
      tree_metrics_out, all_foliage_points, Forest_floor, shift_values,
      crown_volume_m3 = crown_volume_m3,
      understory_volume_m3 = understory_volume_m3,
      coverage_area_m2 = coverage_area_m2
    )
    
    plot_report_file <- file.path(output_path, paste0(plot_id, "_plot_report.csv"))
    fwrite(plot_stats, plot_report_file, sep = ";")
    message("  Plot report: ", basename(plot_report_file))
  }
  
  # Parameters log - includes ALL parameters (exposed and fixed)
  params_list <- list(
    version = "Forest_seg 3.3",
    input_info = list(
      filename = plot_id,
      n_points = n_points,
      integer_precision = integer_precision,
      coordinate_shift = paste0("x0=", round(shift_values$x_shift, 3),
                                ", y0=", round(shift_values$y_shift, 3),
                                ", z0=", round(shift_values$z_shift, 3))
    ),
    dtm_params = list(
      dtm_coarse_res = dtm_coarse_res,
      dtm_fine_res = dtm_fine_res,
      tolerance = tolerance
    ),
    lor_params = list(
      dimVox = dimVox,
      th = th,
      eps = eps,
      mpts = mpts,
      h_trunk = h_trunk,
      N = N,
      w_linear = w_linear,
      h_rescue_min = h_rescue_min,
      h_rescue_max = h_rescue_max,
      Vox_print = Vox_print,
      Woodpoints_print = Woodpoints_print
    ),
    dbh_params = list(
      dbh_tolerance_m = dbh_tolerance,
      dbh_max_rmse_cm = dbh_max_rmse,
      dbh_min_radius_m = dbh_min_radius,
      dbh_max_radius_m = dbh_max_radius,
      dbh_heights_m = paste(dbh_heights, collapse = ", "),
      dbh_min_points = dbh_min_points
    ),
    canopy_params = list(
      canopy_vox_dim = canopy_vox_dim,
      canopy_min_density = canopy_min_density
    ),
    dav_params = list(
      dav_understory_max_start = dav_understory_max_start,
      dav_height_factor = dav_height_factor,
      dav_median_height_factor = dav_median_height_factor,
      dav_eps = dav_eps,
      dav_minPts = dav_minPts
    ),
    cbh_params = list(
      calculate_cbh = calculate_cbh,
      cbh_method = "GAB_voxel_coordinates_v3.3",
      cbh_hex_side_m = cbh_hex_side,
      cbh_voxel_size_m = canopy_vox_dim,  # v3.0.4: layer = voxel w
      cbh_min_branch_length_m = cbh_min_branch_length,
      cbh_min_cluster_cells = cbh_min_cluster_cells,
      cbh_min_points = cbh_min_points,
      cbh_save_points = cbh_save_points,
      # Note: GAB uses voxel coordinates (u,v,w)
      # - Tree center = wood voxel with min(w) for each cls
      # - Voronoi on (u,v) coordinates
      cbh_max_gap_layers = 2
    ),
    output_params = list(
      output_format = output_format,
      output_path = output_path,
      generate_reports = generate_reports
    ),
    results = list(
      n_trees_total = nrow(tree_metrics_out),
      n_valid_trees = sum(tree_metrics_out$valid_tree == TRUE, na.rm = TRUE),
      n_non_valid_trees = sum(tree_metrics_out$valid_tree == FALSE, na.rm = TRUE),
      n_floor_points = nrow(Forest_floor),
      n_crown_points = nrow(crown_points),
      n_understory_points = nrow(understory_points),
      n_noiseL_points = nrow(noiseL_points),
      n_noiseH_points = nrow(noiseH_points)
    )
  )

  # Calculate timing BEFORE writing log
  time_total <- as.numeric(difftime(Sys.time(), tic_total, units = "secs"))

  # Add system info and timing to params_list
  params_list$system_info <- list(
    R_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    OS = Sys.info()["sysname"],
    PiC_version = "3.3.1"
  )

  params_list$timing <- list(
    total_time_seconds = round(time_total, 2),
    points_per_second = round(n_points / time_total, 0)
  )

  log_file <- .write_parameters_log(params_list, output_path, plot_id)
  message("  Parameters log: ", basename(log_file))

  message("\n=============================================================================")
  message("  Forest_seg 3.3 completed in ", round(time_total, 1), " seconds")
  message("=============================================================================\n")
  
  return(list(
    tree_metrics = tree_metrics_out,
    tree_report_file = tree_report_file,
    plot_report_file = plot_report_file,
    las_file = las_file,
    crown_file = crown_file,
    understory_file = understory_file,
    noiseL_file = noiseL_file,
    noiseH_file = noiseH_file,
    wood_file = wood_file,
    non_valid_trees_file = non_valid_trees_file,
    parameters_log = log_file,
    shift_info = shift_values
  ))
}

# ==============================================================================
# HELPER FUNCTIONS - COMPLETE FLOAT ARCHITECTURE
# ==============================================================================

# ------------------------------------------------------------------------------
# COORDINATE SHIFT MANAGEMENT
# Uses shared functions from shared_utils.R:
# - .apply_shift() -> .apply_global_shift()
# - .get_shift() -> .get_shift_values()
# - .reverse_shift() -> .reverse_global_shift()
# ------------------------------------------------------------------------------

.apply_global_shift <- function(point_cloud, decimals = 3) {
  # Wrapper for shared function
  .apply_shift(point_cloud, decimals)
}

.get_shift_values <- function(point_cloud) {
  # Wrapper for shared function
  .get_shift(point_cloud)
}

.reverse_global_shift <- function(output, shift_values) {
  # Wrapper for shared function
  .reverse_shift(output, shift_values)
}

.extract_dtm_float_optimized <- function(point_cloud, dtm_coarse_res, dtm_fine_res,
                                         tolerance, floor_vox_th, floor_min_cluster,
                                         decimals = 3) {

  setDT(point_cloud)

  # ========================================================================
  # Percentile function (DSM)
  # ========================================================================

  get_10th_highest_fast <- function(z_vals) {
    n <- length(z_vals)
    if (n < 10L) return(max(z_vals))
    k <- n - 9L
    sort.int(z_vals, partial = k)[k]
  }

  # ========================================================================
  # STEP 1: VOXELIZZAZIONE + CLASSIFICAZIONE COLONNA
  # ========================================================================

  # Voxelizza a dtm_coarse_res
  vox <- point_cloud[, .N, by = .(
    u = as.integer(floor(x / dtm_coarse_res)),
    v = as.integer(floor(y / dtm_coarse_res)),
    w = as.integer(floor(z / dtm_coarse_res))
  )]
  vox <- vox[N >= floor_vox_th]  # scarta voxel sparsi (rumore/margini)

  message("  Voxelization: ", dtm_coarse_res, "m (", nrow(vox),
          " voxels with N >= ", floor_vox_th, ")")

  # Per ogni colonna 2D (u,v): trova w minimo
  wmin_dt <- vox[, .(wmin = min(w)), by = .(u, v)]
  vox[wmin_dt, wmin := i.wmin, on = .(u, v)]
  vox[, w0 := w - wmin]

  # Candidati floor: w0 <= 1 (il voxel piu' basso + 1 sopra)
  floor_vox <- vox[w0 <= 1L]
  agb_vox   <- vox[w0 > 1L]

  message("  Floor candidate voxels: ", nrow(floor_vox),
          " | AGB voxels: ", nrow(agb_vox))

  rm(wmin_dt)

  # ========================================================================
  # STEP 2: DBSCAN + FILTRO CLUSTER
  # ========================================================================

  # DBSCAN 3D sui voxel floor candidati
  db <- dbscan::dbscan(floor_vox[, .(u, v, w)], eps = 1, minPts = 3)
  floor_vox[, cls := db$cluster]

  # Conta voxel per cluster, tieni solo cluster grandi
  cls_size <- floor_vox[cls > 0L, .N, by = cls]
  good_cls <- cls_size[N >= floor_min_cluster, cls]
  floor_good <- floor_vox[cls %in% good_cls]

  # Voxel floor rifiutati -> tornano in AGB
  floor_bad <- floor_vox[!(cls %in% good_cls)]
  agb_vox <- rbindlist(list(agb_vox, floor_bad[, .(u, v, w, N, wmin, w0)]))

  message("  DBSCAN clusters: ", length(unique(db$cluster[db$cluster > 0L])),
          " | Kept: ", length(good_cls),
          " (>= ", floor_min_cluster, " voxels)",
          " | Floor voxels kept: ", nrow(floor_good))

  rm(db, cls_size, floor_bad, floor_vox)

  # ========================================================================
  # STEP 3: RECUPERO PUNTI ORIGINALI
  # ========================================================================

  # Assegna coordinate voxel ai punti originali
  pc <- point_cloud[, .(x, y, z,
    u = as.integer(floor(x / dtm_coarse_res)),
    v = as.integer(floor(y / dtm_coarse_res)),
    w = as.integer(floor(z / dtm_coarse_res))
  )]

  # Join: punti in voxel floor -> floor_points
  floor_points <- pc[floor_good, on = .(u, v, w), nomatch = NULL][, .(x, y, z)]

  # Tutti gli altri punti (AGB + non in voxel validi)
  remaining <- pc[!floor_good, on = .(u, v, w)]

  message("  Floor points: ", format(nrow(floor_points), big.mark = ","),
          " | Remaining points: ", format(nrow(remaining), big.mark = ","))

  rm(pc, floor_good, agb_vox, vox)

  # ========================================================================
  # STEP 4: GENERAZIONE dtm_grid (dai floor_points puliti) + split low_veg/AGB
  # ========================================================================

  # DTM fine grid dai punti suolo (puliti)
  dtm_grid <- floor_points[, .(z_dtm = min(z)),
    by = .(gx_fine = as.integer(floor(x / dtm_fine_res)),
           gy_fine = as.integer(floor(y / dtm_fine_res)))]

  message("  DTM grid: ", dtm_fine_res, "m (", nrow(dtm_grid), " cells)")

  # Usa dtm_grid per separare low_vegetation e AGB dai remaining
  remaining[, ':='(
    gx_fine = as.integer(floor(x / dtm_fine_res)),
    gy_fine = as.integer(floor(y / dtm_fine_res))
  )]
  remaining[dtm_grid, z_dtm := i.z_dtm, on = .(gx_fine, gy_fine)]

  low_vegetation <- remaining[!is.na(z_dtm) & z <= (z_dtm + tolerance), .(x, y, z)]
  AGB <- remaining[is.na(z_dtm) | z > (z_dtm + tolerance), .(x, y, z)]

  message("  Floor: ", format(nrow(floor_points), big.mark = ","), " pts",
          " | Low veg: ", format(nrow(low_vegetation), big.mark = ","), " pts",
          " | AGB: ", format(nrow(AGB), big.mark = ","), " pts")

  rm(remaining)

  # ========================================================================
  # STEP 5: GENERAZIONE dsm_grid
  # ========================================================================

  dsm_grid <- point_cloud[, .(z_dsm = get_10th_highest_fast(z), n_pts = .N),
    by = .(gx_coarse = as.integer(floor(x / dtm_coarse_res)),
           gy_coarse = as.integer(floor(y / dtm_coarse_res)))]
  dsm_grid <- dsm_grid[n_pts >= 10 & !is.na(z_dsm), .(gx_coarse, gy_coarse, z_dsm)]

  if (nrow(dsm_grid) == 0) {
    dsm_grid <- data.table(gx_coarse = integer(0), gy_coarse = integer(0), z_dsm = numeric(0))
  }

  message("  DSM grid: ", dtm_coarse_res, "m (", nrow(dsm_grid), " cells, 10th highest)")

  # ========================================================================
  # RETURN
  # ========================================================================

  return(list(
    floor_points = floor_points,
    low_vegetation = low_vegetation,
    AGB = AGB,
    dtm_grid = dtm_grid,
    dsm_grid = dsm_grid
  ))
}
# ------------------------------------------------------------------------------
# LOR (Ligneous Object Recognition) - OPTIMIZED FLOAT
# ------------------------------------------------------------------------------

.lor_segment <- function(AGB, dimVox, th, eps, mpts, h_trunk, N,
                         w_linear, Vox_print, output_path, plot_id,
                         dtm_grid = NULL, dtm_fine_res,
                         h_rescue_min = 1, h_rescue_max = 5,
                         decimals = 3) {
  
  # Convert to data.table if not already
  data.table::setDT(AGB)
  
  voxel_size <- dimVox / 100  # cm -> m
  
  # ======================================================================
  # SUB-FUNCTION: DBSCAN + FILTERS (returns voxels with cls + valid cls ids)
  # ======================================================================
  .lor_dbscan_filter_voxels <- function(vox_dt, eps, mpts,
                                        h_trunk, voxel_size, N, w_linear,
                                        h_trunk_factor = 1,
                                        label = "DBSCAN") {
    
    # Initialize cluster column
    if (nrow(vox_dt) == 0) {
      vox_dt[, cls := integer(0)]
      return(list(voxels = vox_dt, valid_clusters = integer(0)))
    }
    
    message("  ", label, " (eps=", eps, ", mpts=", mpts, ")...")
    
    # DBSCAN clustering
    b <- dbscan::dbscan(vox_dt[, .(u, v, w)], eps = eps, minPts = mpts)
    vox_dt[, cls := b$cluster]
    
    n_clusters <- max(vox_dt$cls, na.rm = TRUE)
    message("  ", label, " clusters: ", n_clusters)
    
    # Skip if no clusters found
    if (n_clusters == 0) {
      return(list(voxels = vox_dt, valid_clusters = integer(0)))
    }
    
    # Cluster statistics
    cluster_stats <- vox_dt[cls > 0, .(
      n_voxels = .N,
      n_points = sum(N, na.rm = TRUE),
      delta_w = max(w, na.rm = TRUE) - min(w, na.rm = TRUE),
      delta_u = max(u, na.rm = TRUE) - min(u, na.rm = TRUE),
      delta_v = max(v, na.rm = TRUE) - min(v, na.rm = TRUE),
      w_min = min(w, na.rm = TRUE),
      w_max = max(w, na.rm = TRUE)
    ), by = cls]
    
    message("  ", label, " clusters before filtering: ", nrow(cluster_stats))
    
    # Height threshold in voxel units (scaled by factor)
    h_trunk_voxels <- (h_trunk * h_trunk_factor) / voxel_size
    
    # Apply basic filters: height and minimum voxel count
    candidate_clusters <- cluster_stats[
      delta_w >= h_trunk_voxels & n_voxels >= N,
      cls
    ]
    
    message("  ", label, " candidates after size/height: ", length(candidate_clusters),
            " (h_trunk_factor=", h_trunk_factor, ")")
    
    if (length(candidate_clusters) == 0) {
      return(list(voxels = vox_dt, valid_clusters = integer(0)))
    }
    
    # Apply shape/linearity filter
    if (w_linear < 0) {
      # FAST MODE: aspect ratio filtering
      cluster_stats[, shape_ratio := pmax(delta_u, delta_v, na.rm = TRUE) / 
                      pmax(delta_w, 1, na.rm = TRUE)]
      shape_threshold <- abs(w_linear)
      
      valid_clusters <- cluster_stats[
        cls %in% candidate_clusters & shape_ratio <= shape_threshold,
        cls
      ]
      message("  ", label, " fast mode: Shape ratio (threshold: ", shape_threshold, ")")
      message("  ", label, " valid trees after shape filter: ", length(valid_clusters))
      
    } else {
      # PCA MODE: linearity filtering
      message("  ", label, " PCA mode: linearity on ", length(candidate_clusters), " candidates...")
      
      pca_results <- vox_dt[cls %in% candidate_clusters, {
        if (.N >= 3) {
          n <- .N
          u_c <- u - sum(u)/n
          v_c <- v - sum(v)/n
          w_c <- w - sum(w)/n
          
          cov_scale <- 1.0 / (n - 1)
          cuu <- sum(u_c * u_c) * cov_scale
          cvv <- sum(v_c * v_c) * cov_scale
          cww <- sum(w_c * w_c) * cov_scale
          cuv <- sum(u_c * v_c) * cov_scale
          cuw <- sum(u_c * w_c) * cov_scale
          cvw <- sum(v_c * w_c) * cov_scale
          
          cov_mat <- matrix(c(
            cuu, cuv, cuw,
            cuv, cvv, cvw,
            cuw, cvw, cww
          ), nrow = 3, byrow = TRUE)
          
          eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
          linearity_val <- if (eig[1] > 1e-10) (eig[1] - eig[2]) / eig[1] else 0.0
          
          list(linearity = round(linearity_val, 3))
        } else {
          u_range <- if (.N >= 2) max(u) - min(u) else 0
          v_range <- if (.N >= 2) max(v) - min(v) else 0
          w_range <- if (.N >= 2) max(w) - min(w) else 1
          aspect_ratio <- max(u_range, v_range, 1) / max(w_range, 1)
          list(linearity = 1 / (1 + aspect_ratio))
        }
      }, by = cls]
      
      cluster_stats <- merge(cluster_stats, pca_results, by = "cls", all.x = TRUE)
      cluster_stats[is.na(linearity), linearity := 0.0]
      
      valid_clusters <- cluster_stats[
        cls %in% candidate_clusters & linearity >= w_linear,
        cls
      ]
      message("  ", label, " PCA linearity (threshold: ", w_linear, ")")
      message("  ", label, " valid trees after PCA: ", length(valid_clusters))
    }
    
    return(list(voxels = vox_dt, valid_clusters = valid_clusters))
  }
  
  # ======================================================================
  # STEP 1: VOXELIZATION
  # ======================================================================
  message("\n=== VOXELIZATION ===")
  
  # Assign voxel coordinates (1-based indexing)
  AGB[, ':='(
    u = as.integer(floor(x / voxel_size)) + 1L,
    v = as.integer(floor(y / voxel_size)) + 1L,
    w = as.integer(floor(z / voxel_size)) + 1L
  )]
  
  # Aggregate to voxels and filter by minimum points per voxel
  voxels <- AGB[, .(N = .N), by = .(u, v, w)]
  voxels <- voxels[N >= th]

  # Save voxel count immediately (before any conditional blocks)
  n_voxels_total <- nrow(voxels)
  
  message("  Voxels (", dimVox, "cm, th=", th, "): ", 
          format(nrow(voxels), big.mark = ","))
  
  # Sort voxels for consistency
  message("  Spatial sorting: Z -> Y -> X order...")
  data.table::setorder(voxels, w, v, u)
  
  # Optional: save voxel coordinates
  if (isTRUE(Vox_print)) {
    vox_file <- file.path(output_path, paste0(plot_id, "_voxels.xyz"))
    data.table::fwrite(voxels[, .(u, v, w)], vox_file, sep = " ", col.names = FALSE)
    message("  Voxels saved: ", basename(vox_file))
  }
  
  # ======================================================================
  # STEP 2: BASE PASS - Initial segmentation
  # ======================================================================
  message("\n=== BASE PASS ===")
  
  base <- .lor_dbscan_filter_voxels(
    vox_dt = copy(voxels),
    eps = eps, mpts = mpts,
    h_trunk = h_trunk, voxel_size = voxel_size,
    N = N, w_linear = w_linear,
    h_trunk_factor = 1,
    label = "Base"
  )
  
  voxels <- base$voxels
  valid_clusters <- base$valid_clusters
  
  message("  Base pass complete - valid trees: ", length(valid_clusters))

  # Save BASE valid clusters (needed for SAFE JOIN in RESCUE)
  base_valid_clusters <- valid_clusters

  # ======================================================================
  # STEP 3: RESCUE PASS - Recover trees in specific height range
  # ======================================================================
  if (!is.null(dtm_grid) && isTRUE(nrow(dtm_grid) > 0) && isTRUE(nrow(voxels) > 0)) {
    
    message("\n=== RESCUE PASS ===")
    
    # Select rejected clusters AND pure noise (cls == 0): everything not validated
    nonwood_vox <- voxels[
      !(cls %in% valid_clusters)
    ]
    
    message("  Non-wood voxels: ", format(nrow(nonwood_vox), big.mark = ","))
    
    if (isTRUE(nrow(nonwood_vox) > 0)) {
      
      # Prepare DTM for join
      data.table::setDT(dtm_grid)
      if (!data.table::haskey(dtm_grid)) {
        data.table::setkey(dtm_grid, gx_fine, gy_fine)
      }
      
      # Calculate DTM grid coordinates for voxel centers
      nonwood_vox[, ':='(
        gx_fine = as.integer(floor((((u - 1L) + 0.5) * voxel_size) / dtm_fine_res)),
        gy_fine = as.integer(floor((((v - 1L) + 0.5) * voxel_size) / dtm_fine_res))
      )]
      
      # FIXED: Correct syntax for data.table join with assignment
      # Use := assignment, NOT :=() function
      nonwood_vox[dtm_grid, z_dtm := i.z_dtm, on = .(gx_fine, gy_fine)]
      
      # Calculate normalized height
      nonwood_vox[!is.na(z_dtm), ':='(
        w_floor = as.integer(floor(z_dtm / voxel_size)) + 1L,
        w_norm = w - (as.integer(floor(z_dtm / voxel_size)) + 1L)
      )]
      
      # Define rescue height band in voxel units
      w_norm_min <- max(0L, as.integer(floor(h_rescue_min / voxel_size)))
      w_norm_max <- as.integer(floor(h_rescue_max / voxel_size))
      
      # Filter voxels within rescue band
      rescue_inband <- nonwood_vox[
        !is.na(w_norm) & w_norm >= w_norm_min & w_norm <= w_norm_max
      ]
      
      message("  Rescue band (", h_rescue_min, "-", h_rescue_max, "m) voxels: ",
              format(nrow(rescue_inband), big.mark = ","))
      
      if (isTRUE(nrow(rescue_inband) > 0)) {

        # Rescue h_trunk_factor from band width, clamped to [0.3, 1.0]
        # (never exceeds 1: rescue must not be stricter than the base pass on height)
        h_trunk_factor_rescue <- min(1.0, max(0.3, (h_rescue_max - h_rescue_min) / (2 * h_trunk)))

        # Relaxed DBSCAN minimum points for rescue: scaled down by two
        mpts_rescue <- max(2L, as.integer(round(mpts / 2)))

        # STRICTER shape filter for rescue: a low trunk-band segment is expected
        # to be markedly more linear than a whole tree, so the base threshold
        # (which may be lenient) must be raised here to avoid admitting bushes.
        if (w_linear >= 0) {
          # PCA mode: raise the linearity floor (higher = more linear required)
          w_linear_rescue <- min(0.95, max(0.7, w_linear + 0.2))
        } else {
          # FAST mode (aspect ratio): tighten the allowed horizontal/vertical ratio
          w_linear_rescue <- -max(0.1, abs(w_linear) - 0.2)
        }

        message("  Rescue thresholds: h_trunk_factor=", round(h_trunk_factor_rescue, 3),
                ", mpts=", mpts_rescue, ", N=", max(1L, floor(N / 3)),
                ", w_linear=", w_linear_rescue, " (base: ", w_linear, ")")

        # Run DBSCAN on rescue voxels with reduced thresholds
        rescue <- .lor_dbscan_filter_voxels(
          vox_dt = rescue_inband,
          eps = eps, mpts = mpts_rescue,
          h_trunk = h_trunk, voxel_size = voxel_size,
          N = max(1L, floor(N / 3)),
          w_linear = w_linear_rescue,
          h_trunk_factor = h_trunk_factor_rescue,
          label = "Rescue"
        )
        
        rescue_vox <- rescue$voxels
        rescue_valid <- rescue$valid_clusters
        
        if (isTRUE(length(rescue_valid) > 0)) {
          
          # Generate new cluster IDs (ensure no collision)
          max_cls_global <- max(voxels$cls, na.rm = TRUE)
          
          # Prepare rescue clusters for insertion
          rescue_keep <- rescue_vox[cls %in% rescue_valid, 
                                    .(u, v, w, 
                                      cls_new = cls + max_cls_global,
                                      cls_original = cls)]
          
          # SAFE JOIN: Preserve BASE valid clusters, overwrite everything else
          data.table::setkey(voxels, u, v, w)
          data.table::setkey(rescue_keep, u, v, w)

          # Overwrite noise + rejected clusters, keep only BASE valid
          voxels[rescue_keep, cls := data.table::fifelse(
            cls %in% base_valid_clusters,
            cls,          # preserve BASE valid clusters
            i.cls_new     # overwrite noise/rejected with RESCUE cls
          )]
          
          # Add new valid clusters to the list
          new_valid <- unique(rescue_keep$cls_new)
          valid_clusters <- sort(unique(c(valid_clusters, new_valid)))
          
          message("  Rescue pass recovered: ", length(new_valid), " trees")
          message("  Total valid trees after rescue: ", length(valid_clusters))
        }
      }
    }
  } else {
    message("\n=== RESCUE PASS SKIPPED ===")
    message("  Reason: ", ifelse(is.null(dtm_grid), "DTM not provided", "No voxels available"))
  }
  
  # ======================================================================
  # STEP 4: JOIN CLUSTERS BACK TO POINTS
  # ======================================================================
  message("\n=== JOINING RESULTS ===")

  # Ensure keys for efficient join
  if (!data.table::haskey(AGB)) {
    data.table::setkey(AGB, u, v, w)
  }

  # Create lookup table for valid clusters only
  if (isTRUE(length(valid_clusters) > 0)) {
    cluster_lookup <- voxels[cls %in% valid_clusters, .(u, v, w, cls)]
    data.table::setkey(cluster_lookup, u, v, w)

    # Left join: keep all AGB points, add cluster info where available
    AGB_with_cls <- cluster_lookup[AGB]

    # Reorder columns to put 'cls' at the end
    data.table::setcolorder(
      AGB_with_cls,
      c(setdiff(names(AGB_with_cls), "cls"), "cls")
    )
  } else {
    # No valid clusters found
    AGB_with_cls <- copy(AGB)
    AGB_with_cls[, cls := NA_integer_]
    message("  WARNING: No valid clusters found")
  }

  # Extract wood points (points belonging to valid clusters)
  if (isTRUE(length(valid_clusters) > 0)) {
    wood_valid <- AGB_with_cls[cls %in% valid_clusters]

    # Calculate tree base positions (includes BASE + RESCUE with unique cls)
    tree_bases <- wood_valid[, .(
      x = x[which.min(z)],
      y = y[which.min(z)],
      z = min(z, na.rm = TRUE),
      n_points = .N,
      height = max(z, na.rm = TRUE) - min(z, na.rm = TRUE)
    ), by = cls]

    # Round coordinates if requested
    if (!is.null(decimals) && is.numeric(decimals)) {
      tree_bases[, `:=`(
        x = round(x, decimals),
        y = round(y, decimals),
        z = round(z, decimals)
      )]
    }

    # Assign unique tree_id independent of cls
    tree_bases[, tree_id := .I]

    message("  Final tree count: ", nrow(tree_bases))
  } else {
    wood_valid <- data.table::data.table()
    tree_bases <- data.table::data.table()
    message("  Final tree count: 0")
  }
  
  # Clean up intermediate objects
  # Clean up intermediate objects (only if they exist)
  cleanup_objects <- c("voxels", "nonwood_vox", "rescue_inband", "rescue_vox", "rescue_keep")
  for (obj in cleanup_objects) {
    if (exists(obj)) rm(list = obj)
  }
  if (exists("cluster_lookup")) rm(cluster_lookup)
  if (exists("base")) rm(base)
  if (exists("rescue")) rm(rescue)
  
  return(list(
    tree_bases = tree_bases,
    wood_points_valid = wood_valid,
    AGB_with_cls = AGB_with_cls,
    parameters = list(
      voxel_size = voxel_size,
      n_voxels = n_voxels_total,
      n_valid_clusters = length(valid_clusters),
      valid_clusters = valid_clusters
    )
  ))
}

# ------------------------------------------------------------------------------
# TREE METRICS - FLOAT (with robust height calculation)
# ------------------------------------------------------------------------------
# Height calculation:
# - DSM (10th highest) already computed in .extract_dtm_float()
# - Multiple fallbacks if DSM/DTM missing at tree position
# - Final height = max(DSM-DTM, wood_top-wood_base)
# ------------------------------------------------------------------------------

.calculate_tree_metrics_float <- function(tree_bases, wood_points, dtm_grid, dsm_grid,
                                          decimals, dtm_fine_res, dtm_coarse_res) {

  setDT(tree_bases); setDT(wood_points); setDT(dtm_grid); setDT(dsm_grid)

  metrics <- copy(tree_bases)

  # =========================================================================
  # DTM AT TREE POSITIONS (fine resolution)
  # =========================================================================
  metrics[, ':='(
    gx_fine = as.integer(floor(x / dtm_fine_res)),
    gy_fine = as.integer(floor(y / dtm_fine_res))
  )]
  metrics[dtm_grid, z_dtm := i.z_dtm, on = .(gx_fine, gy_fine)]

  # Fallback: use wood base Z if DTM missing
  n_missing_dtm <- sum(is.na(metrics$z_dtm))
  if (n_missing_dtm > 0) {
    message("    DTM missing for ", n_missing_dtm, " trees, using fallback...")
    dtm_mean <- mean(dtm_grid$z_dtm, na.rm = TRUE)
    wood_base <- wood_points[, .(base_z = min(z, na.rm = TRUE)), by = cls]
    metrics[wood_base, on = "cls", base_z := i.base_z]
    metrics[is.na(z_dtm), z_dtm := fifelse(!is.na(base_z), base_z, dtm_mean)]
    metrics[, base_z := NULL]
  }

  # =========================================================================
  # DSM AT TREE POSITIONS (coarse resolution, same as dsm_grid)
  # =========================================================================
  metrics[, ':='(
    gx_coarse = as.integer(floor(x / dtm_coarse_res)),
    gy_coarse = as.integer(floor(y / dtm_coarse_res))
  )]
  metrics[dsm_grid, z_dsm := i.z_dsm, on = .(gx_coarse, gy_coarse)]

  # Fallback 1: search neighboring cells
  missing_idx <- which(is.na(metrics$z_dsm))
  if (isTRUE(length(missing_idx) > 0)) {
    message("    DSM missing for ", length(missing_idx), " trees, searching neighbors...")
    missing_trees <- metrics[missing_idx, .(cls, gx_coarse, gy_coarse)]
    offsets <- CJ(dx = -1:1, dy = -1:1)
    search_grid <- missing_trees[offsets, .(cls, sx = gx_coarse + dx, sy = gy_coarse + dy),
                                 allow.cartesian = TRUE]
    neighbor_max <- dsm_grid[search_grid, on = .(gx_coarse = sx, gy_coarse = sy), nomatch = NULL][
      , .(neighbor_z = max(z_dsm)), by = cls]
    metrics[neighbor_max, on = "cls", z_dsm := fifelse(is.na(z_dsm), i.neighbor_z, z_dsm)]
  }

  # Fallback 2: use wood top
  still_missing <- which(is.na(metrics$z_dsm))
  if (isTRUE(length(still_missing) > 0)) {
    message("    Still missing for ", length(still_missing), " trees, using wood top...")
    wood_top <- wood_points[, .(top_z = max(z)), by = cls]
    metrics[wood_top, on = "cls", z_dsm := fifelse(is.na(z_dsm), i.top_z, z_dsm)]
  }

  # =========================================================================
  # HEIGHT CALCULATION: max(DSM-DTM, wood_top-wood_base)
  # =========================================================================
  wood_base <- wood_points[, .(base_z = min(z, na.rm = TRUE)), by = cls]
  wood_top <- wood_points[, .(top_z = max(z, na.rm = TRUE)), by = cls]

  metrics[wood_base, on = "cls", wood_base_z := i.base_z]
  metrics[wood_top, on = "cls", wood_top_z := i.top_z]

  metrics[, ':='(
    height_dsm = z_dsm - z_dtm,
    height_wood = wood_top_z - wood_base_z
  )]

  # Use max of DSM-based and wood-based height
  metrics[, Height := round(fifelse(
    is.na(height_dsm), height_wood,
    fifelse(is.na(height_wood), height_dsm,
            pmax(height_dsm, height_wood))
  ), decimals)]

  metrics[is.na(Height) | Height < 0, Height := 0]

  # Initialize DBH fields
  metrics[, ':='(
    DBH               = NA_real_,
    DBH_height_m      = NA_real_,
    DBH_method        = NA_character_,
    DBH_RMSE          = NA_real_,
    DBH_valid         = FALSE,
    DBH_cyl_radius_cm = NA_real_,
    DBH_cyl_rmse_cm   = NA_real_
  )]

  # Cleanup
  metrics[, ':='(gx_fine = NULL, gy_fine = NULL, gx_coarse = NULL, gy_coarse = NULL,
                 z_dtm = NULL, z_dsm = NULL, wood_base_z = NULL, wood_top_z = NULL,
                 height_dsm = NULL, height_wood = NULL)]

  return(metrics)
}

# ------------------------------------------------------------------------------
# DBH CALCULATION - FLOAT
# ------------------------------------------------------------------------------

# ==============================================================================
# OPTIMIZED: DBH calculation with vectorized operations
# IMPROVEMENT: 30-40% faster through reduced iterations and vectorization
# ==============================================================================

.calculate_dbhx_float <- function(tree_metrics, woodpoint, dbh_heights,
                                  dbh_tolerance, dbh_max_rmse, dbh_min_radius,
                                  dbh_max_radius, dbh_min_points, decimals = 3) {

  setDT(tree_metrics)
  setDT(woodpoint)

  dbh_max_rmse_m <- dbh_max_rmse / 100

  setkey(woodpoint, cls)

  # Initialize result columns
  tree_metrics[, ':='(
    DBH               = NA_real_,
    DBH_height_m      = NA_real_,
    DBH_method        = NA_character_,
    DBH_RMSE          = NA_real_,
    DBH_valid         = FALSE,
    DBH_cyl_radius_cm = NA_real_,
    DBH_cyl_rmse_cm   = NA_real_
  )]

  message("  DBH calculation: ", nrow(tree_metrics), " trees, ",
          nrow(woodpoint), " wood points")
  message("  DBH heights: ", paste(dbh_heights, collapse=", "), " m")
  message("  DBH tolerance: +/- ", dbh_tolerance, " m, min_points: ", dbh_min_points)
  message("  DBH radius range: ", dbh_min_radius, "-", dbh_max_radius, " m")
  message("  DBH max RMSE: ", dbh_max_rmse, " cm (", dbh_max_rmse_m, " m)")
  message("  Unique cls in tree_metrics: ", length(unique(tree_metrics$cls)))
  message("  Unique cls in woodpoint: ", length(unique(woodpoint$cls)))
  cls_match <- length(intersect(unique(tree_metrics$cls), unique(woodpoint$cls)))
  message("  Matching cls: ", cls_match, " / ", length(unique(tree_metrics$cls)))

  n_no_wood  <- 0L
  n_no_result <- 0L
  n_success  <- 0L

  # NOTE: loop variable must NOT be named "tree_id" — data.table resolves
  # column names before local variables
  for (current_cls in tree_metrics$cls) {
    tree_wood <- woodpoint[.(current_cls)]
    if (nrow(tree_wood) == 0) { n_no_wood <- n_no_wood + 1L; next }

    tree_wood_dbh <- tree_wood[, .(x, y, z = z_norm)]
    tree_wood_dbh <- tree_wood_dbh[is.finite(z)]
    if (nrow(tree_wood_dbh) == 0) { n_no_result <- n_no_result + 1L; next }
    z_base <- min(tree_wood_dbh$z)

    res <- .calculate_dbh_core(
      wood_points = tree_wood_dbh,
      z_base      = z_base,
      heights     = dbh_heights,
      tolerance   = dbh_tolerance,
      max_rmse    = dbh_max_rmse_m,
      min_radius  = dbh_min_radius,
      max_radius  = dbh_max_radius,
      min_points  = dbh_min_points
    )

    # Always store cylinder fit diagnostics
    tree_metrics[cls == current_cls, ':='(
      DBH_cyl_radius_cm = res$DBH_cyl_radius_cm,
      DBH_cyl_rmse_cm   = res$DBH_cyl_rmse_cm
    )]

    if (res$DBH_valid) {
      tree_metrics[cls == current_cls, ':='(
        DBH          = res$DBH,
        DBH_height_m = res$DBH_height,
        DBH_method   = res$DBH_method,
        DBH_RMSE     = res$DBH_RMSE,
        DBH_valid    = TRUE
      )]
      n_success <- n_success + 1L
    } else {
      n_no_result <- n_no_result + 1L
    }
  }

  message("\n  DBH diagnostics:")
  message("    Trees with no wood points: ", n_no_wood, " / ", nrow(tree_metrics))
  message("    Trees with no valid DBH: ", n_no_result)
  message("    Successful DBH: ", n_success)

  return(tree_metrics)
}

# ==============================================================================
# DAV (Directional Anisotropy of Vegetation) - CROWN/UNDERSTORY CLASSIFICATION v3.1
# ==============================================================================
# CHANGELOG v3.1:
# - NEW: z_median calculation for mass distribution analysis
# - NEW: CASO 5 - Grounded clusters reaching H_median -> always CROWN
# - IMPROVED: 5-case classification logic with vertical mass distribution
# - NEW PARAMETER: dav_median_height_factor (default 0.50)
# 
# Classification logic:
#   CASO 1: Not grounded -> CROWN (aerial foliage)
#   CASO 2: z_max < H_threshold_understory -> UNDERSTORY (too low)
#   CASO 5: z_max >= H_median -> CROWN (reaches tree height)
#   CASO 3: z_median < H_median * dav_median_height_factor -> UNDERSTORY (mass low)
#   CASO 4: z_median >= H_median * dav_median_height_factor -> CROWN (mass high)
# ==============================================================================

.classify_dav_voxels <- function(crown_voxels, 
                                 tree_metrics,
                                 canopy_vox_dim,
                                 canopy_min_density,
                                 dav_understory_max_start,
                                 dav_height_factor,
                                 dav_median_height_factor,  # <- NEW parameter
                                 dav_eps,
                                 dav_minPts,
                                 decimals) {
  
  message("\n=== DAV (Directional Anisotropy of Vegetation) v3.1 ===")
  
  setDT(crown_voxels)
  setDT(tree_metrics)
  
  if (nrow(crown_voxels) == 0) {
    message("  No crown voxels to classify")
    return(list(
      crown = crown_voxels[0],
      understory = crown_voxels[0],
      noiseL = crown_voxels[0],
      noiseH = crown_voxels[0],
      stats = data.table(
        total_voxels = 0,
        crown_voxels = 0,
        understory_voxels = 0,
        noiseL_voxels = 0,
        noiseH_voxels = 0,
        crown_pct = 0,
        understory_pct = 0,
        H_threshold = NA_real_
      )
    ))
  }
  
  # ==========================================================================
  # STEP 1: CALCULATE MEDIAN TREE HEIGHT
  # ==========================================================================
  
  valid_heights <- tree_metrics[Height > 0 & !is.na(Height), Height]
  
  if (length(valid_heights) == 0) {
    message("  WARNING: No valid tree heights for DAV reference")
    H_median <- 10.0  # Fallback
  } else {
    H_median <- median(valid_heights)
  }
  
  # Thresholds
  H_threshold_understory <- H_median * (1 - dav_height_factor)
  H_median_threshold <- H_median * dav_median_height_factor  # <- NEW
  
  message(sprintf("  Tree height statistics:"))
  message(sprintf("    H_median: %.2f m", H_median))
  message(sprintf("    H_threshold_understory: %.2f m (top %.0f%%)", 
                  H_threshold_understory, dav_height_factor * 100))
  message(sprintf("    H_median_threshold: %.2f m (%.0f%% of H_median)", 
                  H_median_threshold, dav_median_height_factor * 100))
  
  # ==========================================================================
  # STEP 2: DENSITY FILTER
  # ==========================================================================
  
  vox_volume <- canopy_vox_dim^3
  crown_voxels[, density := N / vox_volume]
  
  valid_voxels <- crown_voxels[density >= canopy_min_density]
  
  n_total <- nrow(crown_voxels)
  n_valid <- nrow(valid_voxels)
  
  message(sprintf("  Density filter (>= %.0f pts/m\u00b3):", canopy_min_density))
  message(sprintf("    Total voxels: %d", n_total))
  message(sprintf("    Valid voxels: %d (%.1f%%)", n_valid, 100 * n_valid / n_total))
  
  if (n_valid == 0) {
    message("  No voxels pass density filter")
    return(list(
      crown = crown_voxels[0],
      understory = crown_voxels[0],
      noiseL = crown_voxels[0],
      noiseH = crown_voxels[0],
      stats = data.table(
        total_voxels = n_total,
        crown_voxels = 0,
        understory_voxels = 0,
        noiseL_voxels = 0,
        noiseH_voxels = 0,
        crown_pct = 0,
        understory_pct = 0,
        H_threshold = H_threshold_understory
      )
    ))
  }
  
  # ==========================================================================
  # STEP 3: DBSCAN CLUSTERING
  # ==========================================================================
  
  message(sprintf("  DBSCAN clustering (eps=%.1f voxels, minPts=%d)...", 
                  dav_eps, dav_minPts))
  
  coords <- as.matrix(valid_voxels[, .(u, v, w)])
  
  db <- dbscan::dbscan(coords, eps = dav_eps, minPts = dav_minPts)
  
  valid_voxels[, cluster_id := db$cluster]
  
  n_clusters <- max(valid_voxels$cluster_id)
  n_noise <- sum(valid_voxels$cluster_id == 0)
  
  message(sprintf("    Clusters found: %d", n_clusters))
  message(sprintf("    Noise voxels: %d (%.1f%%)", n_noise, 100 * n_noise / n_valid))
  
  # Noise -> separate into noiseL (below H_threshold) and noiseH (above)
  valid_voxels[cluster_id == 0 & z_norm < H_threshold_understory, classification := "noiseL"]
  valid_voxels[cluster_id == 0 & z_norm >= H_threshold_understory, classification := "noiseH"]

  n_noiseL <- sum(valid_voxels$classification == "noiseL", na.rm = TRUE)
  n_noiseH <- sum(valid_voxels$classification == "noiseH", na.rm = TRUE)
  message(sprintf("    Noise low (z < %.2f m): %d voxels", H_threshold_understory, n_noiseL))
  message(sprintf("    Noise high (z >= %.2f m): %d voxels", H_threshold_understory, n_noiseH))

  if (n_clusters == 0) {
    message("  No valid clusters found, all voxels are noise")

    return(list(
      crown = valid_voxels[0],
      understory = valid_voxels[0],
      noiseL = valid_voxels[classification == "noiseL"],
      noiseH = valid_voxels[classification == "noiseH"],
      stats = data.table(
        total_voxels = n_valid,
        crown_voxels = 0,
        understory_voxels = 0,
        noiseL_voxels = n_noiseL,
        noiseH_voxels = n_noiseH,
        crown_pct = 0,
        understory_pct = 0,
        H_threshold = H_threshold_understory
      )
    ))
  }
  
  # ==========================================================================
  # STEP 4: CLUSTER STATISTICS WITH Z_MEDIAN
  # ==========================================================================
  
  message("  Calculating cluster statistics (z_min, z_max, z_median)...")
  
  cluster_stats <- valid_voxels[cluster_id > 0, .(
    z_min = min(z_norm),
    z_max = max(z_norm),
    z_median = median(z_norm),    # <- NEW: vertical mass distribution
    n_voxels = .N,
    n_points = sum(N)
  ), by = cluster_id]
  
  # Check if grounded
  cluster_stats[, is_grounded := z_min < dav_understory_max_start]
  
  message(sprintf("    Grounded clusters: %d / %d", 
                  sum(cluster_stats$is_grounded), nrow(cluster_stats)))
  
  # ==========================================================================
  # STEP 5: CLASSIFICATION WITH 5 CASES
  # ==========================================================================
  
  message("\n  Applying 5-case classification logic:")
  message("    CASE 1: Not grounded -> CROWN")
  message("    CASE 2: Grounded & z_max < H_threshold -> UNDERSTORY")
  message(sprintf("    CASE 3: Grounded & z_max >= %.2f m -> CROWN", H_median))
  message(sprintf("    CASE 4: Grounded & z_median < %.2f m -> UNDERSTORY", H_median_threshold))
  message(sprintf("    CASE 5: Grounded & z_median >= %.2f m -> CROWN", H_median_threshold))
  
  cluster_stats[, classification := "understory"]  # Default
  
  # CASO 1: Not grounded -> CROWN
  caso1 <- cluster_stats[is_grounded == FALSE]
  cluster_stats[is_grounded == FALSE, classification := "crown"]
  
  # CASO 2: Grounded with very low z_max -> UNDERSTORY
  caso2 <- cluster_stats[is_grounded == TRUE & z_max < H_threshold_understory]
  cluster_stats[is_grounded == TRUE & z_max < H_threshold_understory, 
                classification := "understory"]
  
  # CASO 5: Grounded reaching/exceeding H_median -> CROWN
  caso5 <- cluster_stats[is_grounded == TRUE & 
                           z_max >= H_median & 
                           z_max >= H_threshold_understory]
  cluster_stats[is_grounded == TRUE & 
                  z_max >= H_median & 
                  z_max >= H_threshold_understory, 
                classification := "crown"]
  
  # CASO 3: Grounded intermediate with low z_median -> UNDERSTORY
  caso3 <- cluster_stats[is_grounded == TRUE & 
                           z_max >= H_threshold_understory &
                           z_max < H_median &
                           z_median < H_median_threshold]
  cluster_stats[is_grounded == TRUE & 
                  z_max >= H_threshold_understory &
                  z_max < H_median &
                  z_median < H_median_threshold, 
                classification := "understory"]
  
  # CASO 4: Grounded intermediate with high z_median -> CROWN
  caso4 <- cluster_stats[is_grounded == TRUE & 
                           z_max >= H_threshold_understory &
                           z_max < H_median &
                           z_median >= H_median_threshold]
  cluster_stats[is_grounded == TRUE & 
                  z_max >= H_threshold_understory &
                  z_max < H_median &
                  z_median >= H_median_threshold, 
                classification := "crown"]

  # --- MIN VOXEL FILTER FOR CROWN CLUSTERS ---
  # Small crown clusters (< 10 voxels) are likely artifacts.
  # Reclassify them as understory.
  dav_min_crown_voxels <- 5L
  small_crown <- cluster_stats[classification == "crown" & n_voxels < dav_min_crown_voxels]
  
  #if (isTRUE(nrow(small_crown) > 0)) {
  #   cluster_stats[classification == "crown" & n_voxels < dav_min_crown_voxels,
  #              classification := "understory"]
    
  if (isTRUE(nrow(small_crown) > 0)) {
      cluster_stats[classification == "crown" & n_voxels < dav_min_crown_voxels & 
                      z_median >= H_threshold_understory,
                    classification := "noiseH"]
      cluster_stats[classification == "crown" & n_voxels < dav_min_crown_voxels & 
                      z_median < H_threshold_understory,
                    classification := "noiseL"]  
      
    message(sprintf("    Small crown clusters reclassified to understory (n_voxels < %d): %d clusters, %d voxels",
                    dav_min_crown_voxels, nrow(small_crown), sum(small_crown$n_voxels)))
  }

  # Statistics by case
  message("\n  Classification results by case:")
  message(sprintf("    CASE 1 (aerial): %d clusters -> CROWN", nrow(caso1)))
  message(sprintf("    CASE 2 (too low): %d clusters -> UNDERSTORY", nrow(caso2)))
  message(sprintf("    CASE 3 (reaches H_median): %d clusters -> CROWN", nrow(caso5)))
  message(sprintf("    CASE 4 (mass low): %d clusters -> UNDERSTORY", nrow(caso3)))
  message(sprintf("    CASE 5 (mass high): %d clusters -> CROWN", nrow(caso4)))

  # ==========================================================================
  # STEP 6: ASSIGN CLASSIFICATION TO VOXELS
  # ==========================================================================
  
  valid_voxels[cluster_stats, classification := i.classification, 
               on = "cluster_id"]
  
  # valid_voxels[cluster_id == 0, classification := NA]  ### aggiunta 11 feb 26
  
  n_crown <- sum(valid_voxels$classification == "crown", na.rm = TRUE)
  n_understory <- sum(valid_voxels$classification == "understory", na.rm = TRUE)
  n_noiseL <- sum(valid_voxels$classification == "noiseL", na.rm = TRUE)
  n_noiseH <- sum(valid_voxels$classification == "noiseH", na.rm = TRUE)

  message("\n  Final voxel counts:")
  message(sprintf("    CROWN: %d voxels (%.1f%%)",
                  n_crown, 100 * n_crown / n_valid))
  message(sprintf("    UNDERSTORY: %d voxels (%.1f%%)",
                  n_understory, 100 * n_understory / n_valid))
  message(sprintf("    NOISE LOW: %d voxels (%.1f%%)",
                  n_noiseL, 100 * n_noiseL / n_valid))
  message(sprintf("    NOISE HIGH: %d voxels (%.1f%%)",
                  n_noiseH, 100 * n_noiseH / n_valid))
  
  # ==========================================================================
  # RETURN RESULTS
  # ==========================================================================
  
  crown_voxels_out <- valid_voxels[classification == "crown"]
  understory_voxels_out <- valid_voxels[classification == "understory"]
  noiseL_voxels_out <- valid_voxels[classification == "noiseL"]
  noiseH_voxels_out <- valid_voxels[classification == "noiseH"]

  stats <- data.table(
    total_voxels = n_valid,
    crown_voxels = n_crown,
    understory_voxels = n_understory,
    noiseL_voxels = n_noiseL,
    noiseH_voxels = n_noiseH,
    crown_pct = round(100 * n_crown / n_valid, 1),
    understory_pct = round(100 * n_understory / n_valid, 1),
    noiseL_pct = round(100 * n_noiseL / n_valid, 1),
    noiseH_pct = round(100 * n_noiseH / n_valid, 1),
    n_clusters = n_clusters,
    caso1_aerial = nrow(caso1),
    caso2_too_low = nrow(caso2),
    caso5_reaches_H = nrow(caso5),
    caso3_mass_low = nrow(caso3),
    caso4_mass_high = nrow(caso4),
    H_median = H_median,
    H_threshold = H_threshold_understory,
    H_median_threshold = H_median_threshold
  )

  return(list(
    crown = crown_voxels_out,
    understory = understory_voxels_out,
    noiseL = noiseL_voxels_out,
    noiseH = noiseH_voxels_out,
    stats = stats,
    cluster_stats = cluster_stats
  ))
}

.calculate_plot_statistics_float <- function(tree_metrics, AGB_def, Forest_floor, shift_values,
                                              crown_volume_m3 = NA_real_,
                                              understory_volume_m3 = NA_real_,
                                              coverage_area_m2 = NA_real_) {
  # Valid trees subset
  valid_trees <- tree_metrics[tree_metrics$valid_tree == TRUE, ]
  n_total <- nrow(tree_metrics)
  n_valid <- nrow(valid_trees)

  # ---------------------------------------------------------------------------
  # Area of interest (convex hull of all ground points)
  # ---------------------------------------------------------------------------
  area_of_interest <- NA_real_
  if (nrow(Forest_floor) > 2) {
    hull_idx <- grDevices::chull(Forest_floor$x, Forest_floor$y)
    hull_pts <- Forest_floor[c(hull_idx, hull_idx[1]), ]
    # Shoelace formula
    n_h <- nrow(hull_pts)
    area_of_interest <- round(abs(sum(
      hull_pts$x[-n_h] * hull_pts$y[-1] - hull_pts$x[-1] * hull_pts$y[-n_h]
    )) / 2, 2)
  }

  # ---------------------------------------------------------------------------
  # Coverage percentage
  # ---------------------------------------------------------------------------
  coverage_percentage <- NA_real_
  if (!is.na(coverage_area_m2) && !is.na(area_of_interest) && area_of_interest > 0) {
    coverage_percentage <- round(coverage_area_m2 / area_of_interest * 100, 2)
  }

  # ---------------------------------------------------------------------------
  # Crown / understory volumes (passed in, computed from final classified points)
  # ---------------------------------------------------------------------------
  # crown_volume_m3 and understory_volume_m3 are received directly as arguments.

  # ---------------------------------------------------------------------------
  # Height statistics (valid trees)
  # ---------------------------------------------------------------------------
  min_height  <- NA_real_
  max_height  <- NA_real_
  mean_height <- NA_real_
  median_height <- NA_real_
  sd_height   <- NA_real_

  if (n_valid > 0 && sum(!is.na(valid_trees$Height)) > 0) {
    h <- valid_trees$Height[!is.na(valid_trees$Height)]
    min_height    <- round(min(h), 2)
    max_height    <- round(max(h), 2)
    mean_height   <- round(mean(h), 2)
    median_height <- round(median(h), 2)
    sd_height     <- round(sd(h), 2)
  }

  # ---------------------------------------------------------------------------
  # DBH statistics (valid trees)
  # ---------------------------------------------------------------------------
  mean_dbh   <- NA_real_
  median_dbh <- NA_real_

  if (n_valid > 0 && sum(!is.na(valid_trees$DBH_cm)) > 0) {
    d <- valid_trees$DBH_cm[!is.na(valid_trees$DBH_cm)]
    mean_dbh   <- round(mean(d), 1)
    median_dbh <- round(median(d), 1)
  }

  # ---------------------------------------------------------------------------
  # CBH statistics (valid trees)
  # ---------------------------------------------------------------------------
  mean_cbh   <- NA_real_
  median_cbh <- NA_real_

  if (n_valid > 0 && "CBH" %in% names(valid_trees) &&
      sum(!is.na(valid_trees$CBH) & valid_trees$CBH != -999) > 0) {
    cbh_vals <- valid_trees$CBH[!is.na(valid_trees$CBH) & valid_trees$CBH != -999]
    mean_cbh   <- round(mean(cbh_vals), 2)
    median_cbh <- round(median(cbh_vals), 2)
  }

  # ---------------------------------------------------------------------------
  # Density and basal area
  # ---------------------------------------------------------------------------
  trees_per_hectare <- NA_real_
  basal_area_m2_ha  <- NA_real_

  if (!is.na(area_of_interest) && area_of_interest > 0 && n_valid > 0) {
    area_ha <- area_of_interest / 10000
    trees_per_hectare <- round(n_valid / area_ha, 1)

    if (sum(!is.na(valid_trees$DBH_cm)) > 0) {
      dbh_m <- valid_trees$DBH_cm[!is.na(valid_trees$DBH_cm)] / 100
      total_ba <- sum(pi * (dbh_m / 2)^2)
      basal_area_m2_ha <- round(total_ba / area_ha, 2)
    }
  }

  # ---------------------------------------------------------------------------
  # Assemble output
  # ---------------------------------------------------------------------------
  stats <- data.frame(
    metric = c(
      "area_of_interest_m2",
      "coverage_area_m2",
      "coverage_percentage",
      "crown_volume_m3",
      "understory_volume_m3",
      "min_height_m",
      "max_height_m",
      "mean_height_m",
      "median_height_m",
      "sd_height_m",
      "mean_dbh_cm",
      "median_dbh_cm",
      "mean_cbh_m",
      "median_cbh_m",
      "tree_count",
      "valid_tree_count",
      "trees_per_hectare",
      "basal_area_m2_ha"
    ),
    value = c(
      area_of_interest,
      coverage_area_m2,
      coverage_percentage,
      crown_volume_m3,
      understory_volume_m3,
      min_height,
      max_height,
      mean_height,
      median_height,
      sd_height,
      mean_dbh,
      median_dbh,
      mean_cbh,
      median_cbh,
      n_total,
      n_valid,
      trees_per_hectare,
      basal_area_m2_ha
    ),
    stringsAsFactors = FALSE
  )

  return(stats)
}

.write_parameters_log <- function(params_list, output_path, plot_id) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(output_path, paste0(plot_id, "_parameters_", timestamp, ".txt"))

  con <- file(log_file, "w")
  writeLines("# ============================================================================", con)
  writeLines(paste("# FOREST_SEG Analysis Parameters -", params_list$version), con)
  writeLines(paste("# Generated:", Sys.time()), con)
  writeLines("# ============================================================================", con)
  writeLines("", con)

  # Helper function to write a parameter section
  write_section <- function(section_name, section_data, con) {
    if (!is.null(section_data) && length(section_data) > 0) {
      writeLines(paste0("[", section_name, "]"), con)
      for (nm in names(section_data)) {
        val <- section_data[[nm]]
        if (is.character(val)) {
          writeLines(paste0(nm, " = ", val), con)
        } else if (is.logical(val)) {
          writeLines(paste0(nm, " = ", toupper(val)), con)
        } else {
          writeLines(paste0(nm, " = ", val), con)
        }
      }
      writeLines("", con)
    }
  }

  # Write all sections in requested order:
  # 1. SYSTEM INFO (before INPUT INFO)
  write_section("SYSTEM INFO", params_list$system_info, con)
  # 2. INPUT INFO
  write_section("INPUT INFO", params_list$input_info, con)
  # 3. TIMING (after INPUT INFO)
  write_section("PROCESSING TIME", params_list$timing, con)
  # 4. All other parameters
  write_section("DTM PARAMETERS", params_list$dtm_params, con)
  write_section("LOR PARAMETERS", params_list$lor_params, con)
  write_section("DBH PARAMETERS", params_list$dbh_params, con)
  write_section("CANOPY PARAMETERS", params_list$canopy_params, con)
  write_section("DAV (Directional Anisotropy of Vegetation) PARAMETERS", params_list$dav_params, con)
  write_section("GAB (Geometrical Aggregation of Biomass) PARAMETERS", params_list$cbh_params, con)
  write_section("OUTPUT PARAMETERS", params_list$output_params, con)
  write_section("RESULTS", params_list$results, con)

  close(con)
  return(log_file)
}

# Uses shared function from shared_utils.R
# .coerce_to_xyz_dt() handles file paths, data.frames, matrices

################################################################################
# GAB (Geometrical Aggregation of Biomass) - CBH CALCULATION (Forest_seg 3.3)
################################################################################
################################################################################
# GAB - VOXEL COORDINATES (u,v,w)
################################################################################
# New architecture using voxel coordinates instead of original x,y
# - Tree center = wood voxel with min(w) for each cls
# - Voronoi for crown_voxels based on (u,v), not (x,y)
# - No spatial distance filters: use cls directly
################################################################################

.calculate_cbh_gab_v4 <- function(crown_voxels,
                                  wood_voxels,
                                  tree_metrics,
                                  voxel_size,
                                  hex_side = 0.15,
                                  min_points,
                                  min_cluster_cells = 5,
                                  max_gap_layers = 2,
                                  n_cores = NULL,
                                  decimals = 3) {

  message("\n  === GAB (Geometrical Aggregation of Biomass) v3.3 ===")

  # Validate inputs
  if (nrow(crown_voxels) == 0) {
    message("  X No crown voxels available")
    return(NULL)
  }

  if (nrow(wood_voxels) == 0) {
    message("  X No wood voxels available")
    return(NULL)
  }

  if (nrow(tree_metrics) == 0) {
    message("  X No trees in tree_metrics")
    return(NULL)
  }

  # Setup parallelization
  # Detect Shiny environment - mclapply fork() conflicts with Shiny sessions

  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())

  if (is.null(n_cores)) {
    n_cores <- max(1L, parallel::detectCores() - 1L)
  }

  # Force single core in Shiny to prevent session crash
  if (in_shiny && n_cores > 1L) {
    message("  Note: Running in Shiny - using single core for stability")
    n_cores <- 1L
  } else if (Sys.info()["sysname"] == "Darwin" && n_cores > 1L) {
    message("  Note: Using ", n_cores, " cores on macOS (mclapply)")
  }

  message("  Processing ", nrow(tree_metrics), " trees with ", n_cores, " core(s)")
  message("  Hex side: ", hex_side, " m")
  message("  Min points/cell: ", min_points)
  message("  Min cluster cells: ", min_cluster_cells)
  message("  Vertical adjacency: direct only (no gap jumps)")

  # Ensure data.table
  setDT(crown_voxels)
  setDT(wood_voxels)
  setDT(tree_metrics)

  # ============================================================================
  # STEP 1: IDENTIFY TREE CENTERS FROM WOOD VOXELS
  # ============================================================================
  # Tree center = wood voxel with min(w) for each cls

  message("  Identifying tree centers from wood voxels...")

  tree_centers <- wood_voxels[, .(
    u_base = u[which.min(w)],
    v_base = v[which.min(w)],
    w_base = min(w),
    n_wood_voxels = .N
  ), by = cls]

  message("    Tree centers identified: ", nrow(tree_centers))

  # Verify all trees in tree_metrics have wood voxels
  trees_with_wood <- tree_metrics$cls %in% tree_centers$cls
  if (!all(trees_with_wood)) {
    n_missing <- sum(!trees_with_wood)
    message("    WARNING: ", n_missing, " trees have no wood voxels")
  }

  # ============================================================================
  # STEP 2: VORONOI ASSIGNMENT OF CROWN VOXELS (using u,v coordinates)
  # ============================================================================

  message("  Voronoi assignment of crown voxels (u,v space)...")

  n_trees <- nrow(tree_centers)
  n_voxels <- nrow(crown_voxels)

  if (n_trees <= 500 && n_voxels <= 100000) {
    # Fully vectorized approach
    du <- outer(crown_voxels$u, tree_centers$u_base, `-`)
    dv <- outer(crown_voxels$v, tree_centers$v_base, `-`)
    dist_sq <- du^2 + dv^2

    nearest_tree_idx <- max.col(-dist_sq, ties.method = "first")

  } else {
    # Chunked approach for large datasets
    if (requireNamespace("RANN", quietly = TRUE)) {
      tree_coords <- as.matrix(tree_centers[, .(u_base, v_base)])
      voxel_coords <- as.matrix(crown_voxels[, .(u, v)])
      nearest_tree_idx <- RANN::nn2(tree_coords, voxel_coords, k = 1)$nn.idx[, 1]
    } else {
      chunk_size <- 10000L
      nearest_tree_idx <- integer(n_voxels)
      tree_coords <- as.matrix(tree_centers[, .(u_base, v_base)])

      for (start in seq(1L, n_voxels, by = chunk_size)) {
        end <- min(start + chunk_size - 1L, n_voxels)
        chunk_u <- crown_voxels$u[start:end]
        chunk_v <- crown_voxels$v[start:end]

        du <- outer(chunk_u, tree_coords[, 1], `-`)
        dv <- outer(chunk_v, tree_coords[, 2], `-`)
        dist_sq <- du^2 + dv^2
        nearest_tree_idx[start:end] <- max.col(-dist_sq, ties.method = "first")
      }
    }
  }

  # Assign tree_id to each crown voxel
  crown_voxels[, tree_id := tree_centers$cls[nearest_tree_idx]]

  # Summary
  voxels_per_tree <- crown_voxels[, .N, by = tree_id]
  message("    Crown voxels assigned: ", nrow(crown_voxels))
  message("    Trees with crown voxels: ", nrow(voxels_per_tree))
  message("    Voxels per tree (median): ", round(median(voxels_per_tree$N)))

  # ============================================================================
  # STEP 3: CONVERT VOXELS TO HEXAGONAL COORDINATES (global, once)
  # ============================================================================

  message("  Converting to hexagonal coordinates...")

  # Convert voxel coordinates (u,v) to metric coordinates
  # Then apply axial hex formula
  wood_voxels[, ':='(
    x_m = u * voxel_size,
    y_m = v * voxel_size,
    z_m = w * voxel_size
  )]

  crown_voxels[, ':='(
    x_m = u * voxel_size,
    y_m = v * voxel_size,
    z_m = w * voxel_size
  )]

  # Apply axial hex formula (pointy-top)
  wood_voxels[, ':='(
    hex_q = as.integer(round((sqrt(3)/3 * x_m - 1/3 * y_m) / hex_side)),
    hex_r = as.integer(round(2/3 * y_m / hex_side)),
    layer = w  # w is already discretized
  )]

  crown_voxels[, ':='(
    hex_q = as.integer(round((sqrt(3)/3 * x_m - 1/3 * y_m) / hex_side)),
    hex_r = as.integer(round(2/3 * y_m / hex_side)),
    layer = w
  )]

  message("    Hex conversion complete")

  # ============================================================================
  # PRE-SPLIT DATA BY TREE (O(N) once, instead of O(N) per tree in loop)
  # ============================================================================

  setkey(tree_metrics, cls)
  setkey(tree_centers, cls)
  setkey(wood_voxels, cls)
  setkey(crown_voxels, tree_id)

  wood_by_tree   <- split(wood_voxels,  by = "cls")
  crown_by_tree  <- split(crown_voxels, by = "tree_id")

  # Build fast lookup for tree_metrics and tree_centers (small tables)
  metrics_lookup <- setNames(seq_len(nrow(tree_metrics)), as.character(tree_metrics$cls))
  centers_lookup <- setNames(seq_len(nrow(tree_centers)), as.character(tree_centers$cls))

  # ============================================================================
  # PRE-COMPUTE HEX CELLS GLOBALLY (one-pass aggregation, then split by tree)
  # ============================================================================

  message("  Pre-computing hex cell aggregation...")

  # Combine wood and crown voxels with integer type flag (avoids string ops)
  all_vox <- rbind(
    wood_voxels[, .(hex_q, hex_r, layer, N, z_m, vtype = 1L, tree_id = cls)],
    crown_voxels[, .(hex_q, hex_r, layer, N, z_m, vtype = 2L, tree_id)]
  )

  # Global aggregation: one pass over all voxels
  hex_cells_global <- all_vox[, .(
    N_total = sum(N),
    z_min = min(z_m),
    z_max = max(z_m),
    n_wood  = sum(N * (vtype == 1L)),
    n_crown = sum(N * (vtype == 2L))
  ), by = .(tree_id, hex_q, hex_r, layer)]

  # Filter by minimum points
  hex_cells_global <- hex_cells_global[N_total >= min_points]

  # Classify cells
  hex_cells_global[, cell_type := fifelse(n_wood > n_crown, "trunk", "crown")]

  # Split by tree
  setkey(hex_cells_global, tree_id)
  hex_by_tree <- split(hex_cells_global, by = "tree_id")

  rm(all_vox, hex_cells_global)

  message("    Hex pre-computation complete")

  # ============================================================================
  # PARALLEL LOOP OVER TREES
  # ============================================================================

  cbh_results <- parallel::mclapply(tree_metrics$cls, function(current_tree_id) {

    tryCatch({

    # Get tree data via pre-computed lookup (O(1) instead of O(N))
    tree_id_str <- as.character(current_tree_id)
    mi <- metrics_lookup[tree_id_str]
    ci <- centers_lookup[tree_id_str]

    if (is.na(mi)) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "no_tree_data"))
    }

    if (is.na(ci)) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "no_wood_voxels"))
    }

    tree_data <- tree_metrics[mi]
    tree_center <- tree_centers[ci]

    tree_height <- tree_data$Height

    if (is.na(tree_height) || tree_height <= 0) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "invalid_height"))
    }

    # ==========================================================================
    # STEP 4: GET WOOD AND CROWN VOXELS FOR THIS TREE (pre-split, O(1) access)
    # ==========================================================================

    tree_wood <- wood_by_tree[[tree_id_str]]
    tree_crown <- crown_by_tree[[tree_id_str]]

    if (is.null(tree_wood) || nrow(tree_wood) == 0) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "no_wood_for_tree"))
    }

    if (is.null(tree_crown) || nrow(tree_crown) < 50) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "insufficient_crown",
                        n_crown = if (is.null(tree_crown)) 0L else nrow(tree_crown)))
    }

    # ==========================================================================
    # STEP 5-6: GET PRE-COMPUTED HEX CELLS (already aggregated and classified)
    # ==========================================================================

    hex_cells <- hex_by_tree[[tree_id_str]]

    if (is.null(hex_cells) || nrow(hex_cells) == 0) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "no_valid_cells"))
    }

    trunk_cells <- hex_cells[cell_type == "trunk"]
    crown_cells <- hex_cells[cell_type == "crown"]

    if (nrow(trunk_cells) == 0) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "no_trunk_cells"))
    }

    if (nrow(crown_cells) == 0) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "no_crown_cells"))
    }

    # ==========================================================================
    # STEP 7: PER-BRANCH CBH - Find individual branches via crown-only BFS
    # ==========================================================================
    # Algorithm (v3.0.6):
    # 1. Build 3D lookup for ALL cells (trunk + crown)
    # 2. Find crown cells adjacent to trunk cells (contact points)
    # 3. From each contact point, BFS only through CROWN cells -> one branch
    # 4. Project each branch onto XY plane, measure extent
    # 5. CBH = lowest z_min among branches with XY extent >= threshold

    # Combine all cells for lookup
    all_cells <- rbind(
      trunk_cells[, .(hex_q, hex_r, layer, z_min, cell_type = "trunk")],
      crown_cells[, .(hex_q, hex_r, layer, z_min, cell_type = "crown")]
    )

    n_cells <- nrow(all_cells)

    # Extract columns into plain R vectors for fast access
    vec_hex_q <- all_cells$hex_q
    vec_hex_r <- all_cells$hex_r
    vec_layer <- all_cells$layer
    vec_z_min <- all_cells$z_min
    vec_is_trunk <- all_cells$cell_type == "trunk"
    vec_is_crown <- !vec_is_trunk

    # Integer-key lookup: encode (q, r, layer) as single integer
    q_min <- min(vec_hex_q)
    r_min <- min(vec_hex_r)
    l_min <- min(vec_layer)
    q_range <- max(vec_hex_q) - q_min + 1L
    r_range <- max(vec_hex_r) - r_min + 1L
    l_range <- max(vec_layer) - l_min + 1L

    stride_r <- l_range
    stride_q <- r_range * l_range

    # Build vectorized lookup: integer key -> row index (0 = no cell)
    cell_int_key <- (vec_hex_q - q_min) * stride_q + (vec_hex_r - r_min) * stride_r + (vec_layer - l_min)
    lookup_size <- as.integer(q_range) * as.integer(r_range) * as.integer(l_range)
    cell_lookup <- integer(lookup_size)
    cell_lookup[cell_int_key + 1L] <- seq_len(n_cells)

    # Neighbor offsets (computed once)
    lateral_dq <- c(1L, -1L, 0L, 0L, 1L, -1L)
    lateral_dr <- c(0L, 0L, 1L, -1L, -1L, 1L)
    lateral_key_offsets <- lateral_dq * stride_q + lateral_dr * stride_r
    # Vertical adjacency: direct neighbor only (no gap jumps).
    # Goal is the LOWEST foliated branch (CBH); an empty layer breaks the branch
    # rather than being bridged. max_gap_layers is therefore no longer used here.
    vertical_dl <- c(-1L, 1L)

    # ------------------------------------------------------------------
    # STEP 7a: Find crown cells that are adjacent to trunk cells
    # ------------------------------------------------------------------
    # These are the "contact points" where branches connect to the trunk.
    # A crown cell is a contact point if any of its neighbors is a trunk cell.

    trunk_indices <- which(vec_is_trunk)
    contact_crown_indices <- integer(0)

    for (ti in trunk_indices) {
      current_key <- cell_int_key[ti]

      # Check lateral neighbors
      for (j in seq_along(lateral_key_offsets)) {
        nk <- current_key + lateral_key_offsets[j]
        if (nk >= 0L && nk < lookup_size) {
          neighbor_idx <- cell_lookup[nk + 1L]
          if (neighbor_idx > 0L && vec_is_crown[neighbor_idx]) {
            contact_crown_indices <- c(contact_crown_indices, neighbor_idx)
          }
        }
      }

      # Check vertical neighbors
      for (dl in vertical_dl) {
        nk <- current_key + dl
        if (nk >= 0L && nk < lookup_size) {
          neighbor_idx <- cell_lookup[nk + 1L]
          if (neighbor_idx > 0L && vec_is_crown[neighbor_idx]) {
            contact_crown_indices <- c(contact_crown_indices, neighbor_idx)
          }
        }
      }
    }

    contact_crown_indices <- unique(contact_crown_indices)

    # ------------------------------------------------------------------
    # STEP 7b: BFS from each contact point through CROWN cells only
    # ------------------------------------------------------------------
    # Each unvisited contact point seeds a new branch.
    # The BFS expands only through crown cells (never enters trunk cells).
    # For each branch: track XY extent and z_min.

    vec_branch_id <- rep(NA_integer_, n_cells)  # only crown cells get assigned
    branch_counter <- 0L
    branch_info <- list()
    queue <- integer(max(1000L, n_cells))

    for (seed_idx in contact_crown_indices) {
      # Skip if this crown cell was already assigned to a branch
      if (!is.na(vec_branch_id[seed_idx])) next

      # Start new branch
      branch_counter <- branch_counter + 1L
      current_branch <- branch_counter

      # Initialize branch stats
      branch_z_min <- Inf
      bq_min <- Inf; bq_max <- -Inf
      br_min <- Inf; br_max <- -Inf

      # BFS - crown cells only
      queue[1] <- seed_idx
      queue_start <- 1L
      queue_end <- 1L

      while (queue_start <= queue_end) {
        idx <- queue[queue_start]
        queue_start <- queue_start + 1L

        # Skip if already assigned
        if (!is.na(vec_branch_id[idx])) next

        # Only traverse crown cells
        if (!vec_is_crown[idx]) next

        vec_branch_id[idx] <- current_branch

        # Update XY extent (projected onto XY plane)
        hq <- vec_hex_q[idx]
        hr <- vec_hex_r[idx]
        if (hq < bq_min) bq_min <- hq
        if (hq > bq_max) bq_max <- hq
        if (hr < br_min) br_min <- hr
        if (hr > br_max) br_max <- hr

        # Track z_min of this branch
        z_val <- vec_z_min[idx]
        if (z_val < branch_z_min) branch_z_min <- z_val

        # Expand to crown neighbors
        current_key <- cell_int_key[idx]

        # Lateral neighbors (6-way hexagonal)
        for (j in seq_along(lateral_key_offsets)) {
          nk <- current_key + lateral_key_offsets[j]
          if (nk >= 0L && nk < lookup_size) {
            neighbor_idx <- cell_lookup[nk + 1L]
            if (neighbor_idx > 0L && is.na(vec_branch_id[neighbor_idx]) && vec_is_crown[neighbor_idx]) {
              queue_end <- queue_end + 1L
              if (queue_end > length(queue)) {
                length(queue) <- length(queue) * 2L
              }
              queue[queue_end] <- neighbor_idx
            }
          }
        }

        # Vertical neighbors (gap tolerance)
        for (dl in vertical_dl) {
          nk <- current_key + dl
          if (nk >= 0L && nk < lookup_size) {
            neighbor_idx <- cell_lookup[nk + 1L]
            if (neighbor_idx > 0L && is.na(vec_branch_id[neighbor_idx]) && vec_is_crown[neighbor_idx]) {
              queue_end <- queue_end + 1L
              if (queue_end > length(queue)) {
                length(queue) <- length(queue) * 2L
              }
              queue[queue_end] <- neighbor_idx
            }
          }
        }
      }

      # Store branch info
      branch_info[[current_branch]] <- list(
        z_min = branch_z_min,
        q_range = bq_max - bq_min,
        r_range = br_max - br_min
      )
    }

    # ==========================================================================
    # STEP 8: DETERMINE CBH FROM VALID BRANCHES
    # ==========================================================================
    # A branch is valid if its XY extent (projected) >= min_branch_length.
    # CBH = lowest crown z_min among all valid branches.

    min_xy_extent_m <- min_cluster_cells * hex_side * sqrt(3)
    cbh_candidates <- numeric(0)

    for (br in branch_info) {
      # Measure XY extent of this branch (max of q_range, r_range in meters)
      xy_extent_m <- max(br$q_range, br$r_range) * hex_side * sqrt(3)

      if (xy_extent_m >= min_xy_extent_m) {
        cbh_candidates <- c(cbh_candidates, br$z_min)
      }
    }

    # CBH = lowest valid candidate across all branches
    if (isTRUE(length(cbh_candidates) > 0)) {
      cbh_value <- min(cbh_candidates)
    } else {
      cbh_value <- NA_real_
    }

    # Cleanup
    rm(all_cells, cell_lookup, cell_int_key, branch_info, queue,
       vec_hex_q, vec_hex_r, vec_layer, vec_z_min, vec_is_trunk,
       vec_is_crown, vec_branch_id, contact_crown_indices)

    # ==========================================================================
    # STEP 9: VALIDATION
    # ==========================================================================

    if (is.na(cbh_value)) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "no_connection"))
    }

    if (cbh_value < 0.5) {
      return(data.table(cls = current_tree_id, CBH = -999, error = "cbh_too_low",
                        cbh_raw = cbh_value))
    }

    if (cbh_value > tree_height * 0.90) {
      return(data.table(cls = current_tree_id, CBH = 999, error = "cbh_too_high"))
    }

    return(data.table(
      cls = current_tree_id,
      CBH = round(cbh_value, 2),
      n_trunk_cells = nrow(trunk_cells),
      n_crown_cells = nrow(crown_cells),
      n_wood_voxels = nrow(tree_wood),
      n_crown_voxels = nrow(tree_crown)
    ))

    }, error = function(e) {
      return(data.table(
        cls = current_tree_id,
        CBH = -999,
        error = paste("R_error:", conditionMessage(e))
      ))
    })

  }, mc.cores = n_cores)

  # Combine results
  cbh_table <- rbindlist(cbh_results, fill = TRUE)

  # Summary statistics
  n_valid <- sum(!is.na(cbh_table$CBH) & cbh_table$CBH > 0 & cbh_table$CBH < 900)
  n_low <- sum(cbh_table$CBH == -999, na.rm = TRUE)
  n_high <- sum(cbh_table$CBH == 999, na.rm = TRUE)

  message("\n  CBH Results:")
  message("    Valid: ", n_valid, " (", round(100 * n_valid / nrow(cbh_table), 1), "%)")
  message("    Failed (too low): ", n_low)
  message("    Failed (too high): ", n_high)

  return(list(tree_cbh = cbh_table))
}
# ==============================================================================

# ------------------------------------------------------------------------------
# LOR VOXELS CREATION
# ------------------------------------------------------------------------------

#' LOR - Create wood voxels from wood points for GAB (Geometrical Aggregation of Biomass) integration
#' @param wood_points Wood points data.table
#' @param vox_dim Voxel dimension (same as crown voxels)
#' @param dtm_grid DTM reference (uses gx_fine, gy_fine with dtm_fine_res)
#' @param dtm_fine_res DTM fine resolution (dtm_grid uses this)
#' @param decimals Coordinate precision
#' @return Wood voxels data.table
#' @keywords internal
#' @noRd
.lor_create_voxels <- function(wood_points, vox_dim, dtm_grid, dtm_fine_res, decimals) {

  setDT(wood_points)

  if (nrow(wood_points) == 0) {
    return(data.table(
      u = integer(0), v = integer(0), w = integer(0),
      cls = integer(0), N = integer(0),
      x = numeric(0), y = numeric(0), z_norm = numeric(0)
    ))
  }

  # Copy to avoid modifying original
  wp <- copy(wood_points)

  # Voxelize wood points
  wp[, ':='(
    u = as.integer(floor(x / vox_dim)) + 1L,
    v = as.integer(floor(y / vox_dim)) + 1L,
    w_orig = as.integer(floor(z / vox_dim)) + 1L
  )]

  # Aggregate
  wood_voxels <- wp[, .(
    N = .N,
    x = mean(x),
    y = mean(y),
    z = mean(z)
  ), by = .(u, v, w_orig, cls)]

  # Height normalization - use dtm_fine_res to match dtm_grid
  wood_voxels[, ':='(
    gx_fine = as.integer(floor(x / dtm_fine_res)),
    gy_fine = as.integer(floor(y / dtm_fine_res))
  )]

  wood_voxels[dtm_grid, z_dtm := i.z_dtm, on = .(gx_fine, gy_fine)]

  if (any(is.na(wood_voxels$z_dtm))) {
    dtm_mean <- mean(dtm_grid$z_dtm, na.rm = TRUE)
    wood_voxels[is.na(z_dtm), z_dtm := dtm_mean]
  }

  wood_voxels[, z_norm := z - z_dtm]
  wood_voxels[, w := as.integer(floor(z_norm / vox_dim)) + 1L]

  # Re-aggregate by normalized coordinates
  wood_voxels <- wood_voxels[, .(
    N = sum(N),
    x = mean(x),
    y = mean(y),
    z_norm = mean(z_norm)
  ), by = .(u, v, w, cls)]

  return(wood_voxels)
}

# ==============================================================================
# END OF FILE
# ==============================================================================
