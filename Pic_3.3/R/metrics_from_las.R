#' @name metrics_from_las
#' @title Calculate tree and plot metrics from a classified LAS file
#'
#' @description
#' Recalculates tree-level and plot-level metrics starting from a classified
#' LAS file. Intended for use after manual correction of an automatic
#' classification produced by \code{\link{Forest_seg}}.
#'
#' Wood points (class 4) are re-clustered into individual trees using DBSCAN
#' with fixed, permissive parameters (5 cm voxels, eps = 2, minPts = 5).
#' These parameters are intentionally loose because the classification is
#' already trusted; DBSCAN here only separates spatially distinct trunks.
#'
#' Tree height is computed as the difference between the tree base elevation
#' (minimum z of the wood cluster) and the maximum z of any point in the full
#' cloud within a 0.5 m XY buffer around the tree base, consistent with the
#' \code{\link{Forest_seg}} approach.
#'
#' Crown Base Height (CBH) is calculated with the GAB (Geometrical Aggregation
#' of Biomass) Voronoi method, identical to \code{\link{Forest_seg}}.
#'
#' Output CSV files use the same column layout as \code{Forest_seg} reports.
#'
#' @param las_file Character. Path to the classified LAS file.
#' @param output_path Character. Directory for output files.
#'   Default: same directory as \code{las_file}.
#' @param filename Character. Prefix for output file names.
#'   Default: derived from \code{las_file} (stripping \code{_classified}).
#' @param dbh_tolerance Vertical half-width of the DBH slice in metres
#'   (default = 0.05).
#' @param dbh_max_rmse Maximum acceptable RMSE for circle fit in cm
#'   (default = 5).
#' @param dbh_min_radius Minimum valid stem radius in metres (default = 0.025).
#' @param dbh_max_radius Maximum valid stem radius in metres (default = 0.8).
#' @param calculate_cbh Logical. If TRUE, calculate Crown Base Height using
#'   the GAB method (default = TRUE). Requires crown points (class 5).
#' @param cbh_hex_side Hexagonal cell side length in metres for GAB
#'   (default = 0.15).
#' @param cbh_min_branch_length Minimum horizontal foliage extent in metres
#'   to consider a branch valid for CBH (default = 2.0).
#' @param canopy_vox_dim Voxel size in metres for crown and wood voxelisation
#'   used in GAB CBH (default = 0.15).
#' @param canopy_min_density Minimum point density in pts/m\eqn{^3} used to
#'   derive the minimum points per hex cell for GAB (default = 100).
#' @param generate_reports Logical. If TRUE (default), saves
#'   \code{<filename>_tree_report.csv} and \code{<filename>_plot_report.csv}.
#'
#' @return Invisibly returns a named list:
#' \describe{
#'   \item{tree_metrics}{data.table with one row per detected tree.}
#'   \item{plot_stats}{data.frame with plot-level aggregate metrics.}
#'   \item{tree_report}{Path to the tree-level CSV, or NULL.}
#'   \item{plot_report}{Path to the plot-level CSV, or NULL.}
#' }
#'
#' @section LAS classification codes expected:
#' \itemize{
#'   \item 2 = Ground / forest floor (convex hull for plot area)
#'   \item 3 = Understory
#'   \item 4 = Wood (re-clustered into individual trees)
#'   \item 5 = Crown / foliage (used for height buffer and CBH)
#'   \item 6 = Non-valid trees (ignored)
#'   \item 7 = Noise (ignored)
#' }
#'
#' @seealso \code{\link{Forest_seg}}, \code{\link{SegOne}}
#'
#' @importFrom data.table data.table setDT setkey copy fwrite setnames
#' @importFrom data.table as.data.table fifelse rbindlist := .N .I
#' @importFrom dbscan dbscan
#'
#' @export
metrics_from_las <- function(las_file,
                             output_path       = NULL,
                             filename          = NULL,
                             dbh_tolerance     = 0.05,
                             dbh_max_rmse      = 5,
                             dbh_min_radius    = 0.025,
                             dbh_max_radius    = 0.8,
                             calculate_cbh     = TRUE,
                             cbh_hex_side      = 0.15,
                             cbh_min_branch_length = 2.0,
                             canopy_vox_dim    = 0.15,
                             canopy_min_density = 100,
                             generate_reports  = TRUE) {

  # ============================================================================
  # FIXED INTERNAL PARAMETERS
  # ============================================================================
  WOOD_VOX_M  <- 0.05   # 5 cm voxel for trunk clustering
  WOOD_EPS    <- 2L     # DBSCAN epsilon (voxel units)
  WOOD_MPTS   <- 5L     # DBSCAN minPts
  WOOD_N_MIN  <- 50L    # minimum voxels per cluster
  HEIGHT_BUF  <- 0.5    # XY buffer (m) for tree height lookup
  DBH_HEIGHTS <- c(1.3, 1.6)
  DBH_MIN_PTS <- 5L
  DECIMALS    <- 3L

  # Derived CBH parameters (same formulas as Forest_seg)
  cell_volume        <- (cbh_hex_side^2 * sqrt(3) / 2) * canopy_vox_dim
  cbh_min_points     <- max(1L, round(cell_volume * canopy_min_density))
  cbh_min_cluster_cells <- ceiling(cbh_min_branch_length / cbh_hex_side)

  # ============================================================================
  # STEP 0: RESOLVE PATHS
  # ============================================================================
  if (!file.exists(las_file))
    stop("LAS file not found: ", las_file, call. = FALSE)

  if (is.null(output_path)) output_path <- dirname(las_file)
  if (!dir.exists(output_path))
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

  if (is.null(filename)) {
    filename <- tools::file_path_sans_ext(basename(las_file))
    filename <- sub("_classified$", "", filename)
  }

  message("========================================")
  message("metrics_from_las")
  message("Input : ", basename(las_file))
  message("Output: ", filename)
  message("========================================")

  # ============================================================================
  # STEP 1: READ LAS - keep ALL points for height calculation
  # ============================================================================
  message("\nStep 1/6: Reading LAS...")
  .require_lidR()
  las <- lidR::readLAS(las_file, select = "xyzc")
  if (lidR::is.empty(las))
    stop("LAS file is empty.", call. = FALSE)

  all_pts <- as.data.table(las@data)
  setnames(all_pts, c("X", "Y", "Z", "Classification"), c("x", "y", "z", "cls_las"))
  rm(las); gc(verbose = FALSE)

  wood_pts       <- all_pts[cls_las == 4L, .(x, y, z)]
  crown_pts      <- all_pts[cls_las == 5L, .(x, y, z)]
  floor_pts      <- all_pts[cls_las == 2L, .(x, y, z)]
  understory_pts <- all_pts[cls_las == 3L, .(x, y, z)]

  message("  Wood (4):       ", format(nrow(wood_pts),       big.mark = ","))
  message("  Crown (5):      ", format(nrow(crown_pts),      big.mark = ","))
  message("  Floor (2):      ", format(nrow(floor_pts),      big.mark = ","))
  message("  Understory (3): ", format(nrow(understory_pts), big.mark = ","))

  if (nrow(wood_pts) == 0)
    stop("No wood points (class 4) found in the LAS file.", call. = FALSE)

  # ============================================================================
  # STEP 2: CLUSTER WOOD INTO INDIVIDUAL TREES
  # ============================================================================
  message("\nStep 2/6: Clustering wood into trees",
          " (voxel=5cm, eps=2, minPts=5)...")

  wood_vox <- .voxelize_core(copy(wood_pts), WOOD_VOX_M,
                              min_points = 1L, return_points = TRUE)
  vox_tbl <- wood_vox[, .N, by = .(u, v, w)]

  db        <- dbscan::dbscan(as.matrix(vox_tbl[, .(u, v, w)]),
                               eps = WOOD_EPS, minPts = WOOD_MPTS)
  vox_tbl[, cls_raw := db$cluster]

  cls_sizes <- vox_tbl[cls_raw > 0L, .N, by = cls_raw]
  valid_raw <- cls_sizes[N >= WOOD_N_MIN, cls_raw]
  vox_valid <- vox_tbl[cls_raw %in% valid_raw]
  cls_map   <- data.table(cls_raw = sort(valid_raw),
                           cls     = seq_along(valid_raw))
  vox_valid[cls_map, cls := i.cls, on = "cls_raw"]

  message("  Clusters detected:             ", max(db$cluster, 0L))
  message("  Clusters after size filter:    ", length(valid_raw))

  if (length(valid_raw) == 0L)
    stop("No wood clusters found. The wood points may be too sparse.",
         call. = FALSE)

  setkey(vox_valid, u, v, w)
  setkey(wood_vox,  u, v, w)
  wood_cls <- vox_valid[wood_vox, nomatch = 0L][, .(x, y, z, cls)]

  message("  Wood points assigned to trees: ",
          format(nrow(wood_cls), big.mark = ","),
          " / ", format(nrow(wood_pts), big.mark = ","))

  # ============================================================================
  # STEP 3: TREE BASE POSITION AND HEIGHT
  #   Height = max z of ANY point within HEIGHT_BUF m of tree base - z_base
  # ============================================================================
  message("\nStep 3/6: Computing tree position and height (buffer = ",
          HEIGHT_BUF, " m)...")

  tree_bases <- wood_cls[, .(
    x        = x[which.min(z)],
    y        = y[which.min(z)],
    z        = min(z, na.rm = TRUE),
    n_points = .N
  ), by = cls]
  setDT(tree_bases)

  # For each tree base, search all_pts within HEIGHT_BUF in XY
  tree_bases[, Height := {
    vapply(seq_len(.N), function(i) {
      buf <- all_pts[
        abs(x - tree_bases$x[i]) <= HEIGHT_BUF &
          abs(y - tree_bases$y[i]) <= HEIGHT_BUF
      ]
      if (nrow(buf) == 0L) return(NA_real_)
      round(max(buf$z, na.rm = TRUE) - tree_bases$z[i], DECIMALS)
    }, numeric(1L))
  }]

  # Add z_norm to wood points (height above tree base, used for DBH slicing)
  base_z_tbl <- wood_cls[, .(base_z = min(z, na.rm = TRUE)), by = cls]
  wood_cls[base_z_tbl, z_norm := z - i.base_z, on = "cls"]

  message("  Trees:  ", nrow(tree_bases))
  message("  Height: ", round(min(tree_bases$Height, na.rm = TRUE), 1),
          " - ", round(max(tree_bases$Height, na.rm = TRUE), 1), " m")

  # all_pts no longer needed for height; free memory
  rm(all_pts); gc(verbose = FALSE)

  # ============================================================================
  # STEP 4: DBH CALCULATION
  # ============================================================================
  message("\nStep 4/6: Calculating DBH...")

  tree_metrics <- copy(tree_bases)
  tree_metrics[, ':='(
    DBH               = NA_real_,
    DBH_height_m      = NA_real_,
    DBH_method        = NA_character_,
    DBH_RMSE          = NA_real_,
    DBH_valid         = FALSE,
    DBH_cyl_radius_cm = NA_real_,
    DBH_cyl_rmse_cm   = NA_real_
  )]

  setkey(wood_cls, cls)

  for (current_cls in tree_metrics$cls) {
    tree_wood <- wood_cls[.(current_cls), .(x, y, z = z_norm)]
    tree_wood <- tree_wood[is.finite(z)]
    if (nrow(tree_wood) < DBH_MIN_PTS) next

    res <- .calculate_dbh_core(
      wood_points = tree_wood,
      z_base      = min(tree_wood$z),
      heights     = DBH_HEIGHTS,
      tolerance   = dbh_tolerance,
      max_rmse    = dbh_max_rmse / 100,
      min_radius  = dbh_min_radius,
      max_radius  = dbh_max_radius,
      min_points  = DBH_MIN_PTS
    )

    tree_metrics[cls == current_cls, ':='(
      DBH_cyl_radius_cm = res$DBH_cyl_radius_cm,
      DBH_cyl_rmse_cm   = res$DBH_cyl_rmse_cm
    )]

    if (isTRUE(res$DBH_valid)) {
      tree_metrics[cls == current_cls, ':='(
        DBH          = res$DBH,
        DBH_height_m = res$DBH_height,
        DBH_method   = res$DBH_method,
        DBH_RMSE     = res$DBH_RMSE,
        DBH_valid    = TRUE
      )]
    }
  }

  tree_metrics[, ':='(
    DBH_cm      = round(DBH * 100, 1),
    DBH_RMSE_cm = round(DBH_RMSE * 100, 1),
    valid_tree  = DBH_valid
  )]

  n_valid_dbh <- sum(tree_metrics$DBH_valid, na.rm = TRUE)
  message("  Valid DBH: ", n_valid_dbh, " / ", nrow(tree_metrics))

  # ============================================================================
  # STEP 5: CBH via GAB (Geometrical Aggregation of Biomass)
  # ============================================================================
  if (calculate_cbh) {
    if (nrow(crown_pts) == 0L) {
      message("\nStep 5/6: CBH skipped - no crown points (class 5) found")
    } else {
      message("\nStep 5/6: CBH via GAB (hex_side=", cbh_hex_side,
              "m, min_branch=", cbh_min_branch_length, "m)...")

      tryCatch({
        # Reference altitude: use minimum floor z if available,
        # otherwise minimum of crown + wood z
        z_ref <- if (nrow(floor_pts) > 0L) {
          min(floor_pts$z, na.rm = TRUE)
        } else {
          min(c(wood_pts$z, crown_pts$z), na.rm = TRUE)
        }
        message("  Ground reference z: ", round(z_ref, 3), " m")

        # --- Crown voxels (class 5) at canopy_vox_dim resolution ---
        # w is normalised height above z_ref
        crown_vox_cbh <- copy(crown_pts)
        crown_vox_cbh[, ':='(
          u = as.integer(floor(x / canopy_vox_dim)) + 1L,
          v = as.integer(floor(y / canopy_vox_dim)) + 1L,
          w = as.integer(floor((z - z_ref) / canopy_vox_dim)) + 1L
        )]
        crown_vox_cbh <- crown_vox_cbh[w >= 1L,
                                        .(N = .N), by = .(u, v, w)]
        message("  Crown voxels: ", format(nrow(crown_vox_cbh), big.mark = ","))

        # --- Wood voxels (class 4, with cls) at canopy_vox_dim resolution ---
        wood_vox_cbh <- copy(wood_cls)
        wood_vox_cbh[, ':='(
          u = as.integer(floor(x / canopy_vox_dim)) + 1L,
          v = as.integer(floor(y / canopy_vox_dim)) + 1L,
          w = as.integer(floor((z - z_ref) / canopy_vox_dim)) + 1L
        )]
        wood_vox_cbh <- wood_vox_cbh[w >= 1L, .(
          N     = .N,
          x     = mean(x),
          y     = mean(y),
          z_norm = mean(z - z_ref)
        ), by = .(u, v, w, cls)]
        message("  Wood voxels:  ", format(nrow(wood_vox_cbh), big.mark = ","))

        # --- Call GAB ---
        cbh_res <- .calculate_cbh_gab_v4(
          crown_voxels      = crown_vox_cbh,
          wood_voxels       = wood_vox_cbh,
          tree_metrics      = tree_metrics,
          voxel_size        = canopy_vox_dim,
          hex_side          = cbh_hex_side,
          min_points        = cbh_min_points,
          min_cluster_cells = cbh_min_cluster_cells,
          max_gap_layers    = 2L,
          n_cores           = NULL,
          decimals          = DECIMALS
        )

        if (!is.null(cbh_res$tree_cbh)) {
          tree_metrics <- merge(
            tree_metrics,
            cbh_res$tree_cbh[, .(cls, CBH)],
            by = "cls", all.x = TRUE
          )
          n_valid_cbh <- sum(!is.na(tree_metrics$CBH) &
                               tree_metrics$CBH > 0 &
                               tree_metrics$CBH < 900,
                             na.rm = TRUE)
          message("  CBH calculated: ", n_valid_cbh, " / ", nrow(tree_metrics))

          if (generate_reports) {
            cbh_diag_file <- file.path(output_path,
                                        paste0(filename, "_cbh_diagnostics.csv"))
            fwrite(cbh_res$tree_cbh, cbh_diag_file, sep = ";")
            message("  CBH diagnostics: ", basename(cbh_diag_file))
          }
        }

      }, error = function(e) {
        message("  CBH calculation failed: ", e$message)
        message("  Continuing without CBH.")
      })
    }
  } else {
    message("\nStep 5/6: CBH skipped (calculate_cbh = FALSE)")
  }

  # ============================================================================
  # STEP 6: PLOT STATISTICS
  # ============================================================================
  message("\nStep 6/6: Computing plot statistics...")

  empty_dt <- data.table(x = numeric(0), y = numeric(0), z = numeric(0))

  plot_stats <- .calculate_plot_statistics_float(
    tree_metrics     = tree_metrics,
    AGB_def          = if (nrow(crown_pts) > 0L) crown_pts else empty_dt,
    Forest_floor     = if (nrow(floor_pts) > 0L) floor_pts else empty_dt,
    shift_values     = list(x_shift = 0, y_shift = 0, z_shift = 0),
    crown_volume_m3 = NA_real_,
    understory_volume_m3 = NA_real_,
    coverage_area_m2 = NA_real_
  )

  # ============================================================================
  # SAVE REPORTS
  # ============================================================================
  tree_report_file <- NULL
  plot_report_file <- NULL

  if (generate_reports) {
    tree_cols <- c("cls", "x", "y", "z", "Height",
                   "DBH_cm", "DBH_RMSE_cm", "valid_tree")
    if ("CBH" %in% names(tree_metrics)) tree_cols <- c(tree_cols, "CBH")

    tree_out <- copy(tree_metrics[, ..tree_cols])
    setnames(tree_out,
             c("cls", "x", "y", "z", "DBH_cm", "DBH_RMSE_cm"),
             c("Tree_n", "X", "Y", "Z", "DBH (cm)", "RMSE (cm)"))

    tree_report_file <- file.path(output_path,
                                   paste0(filename, "_tree_report.csv"))
    fwrite(tree_out, tree_report_file, sep = ";")
    message("  Tree report: ", basename(tree_report_file))

    plot_report_file <- file.path(output_path,
                                   paste0(filename, "_plot_report.csv"))
    fwrite(plot_stats, plot_report_file, sep = ";")
    message("  Plot report: ", basename(plot_report_file))
  }

  message("\n========================================")
  message("Done.")
  message("  Trees detected : ", nrow(tree_metrics))
  message("  Trees with DBH : ", n_valid_dbh)
  if ("CBH" %in% names(tree_metrics))
    message("  Trees with CBH : ",
            sum(!is.na(tree_metrics$CBH) & tree_metrics$CBH > 0 &
                  tree_metrics$CBH < 900, na.rm = TRUE))
  if (!is.null(tree_report_file))
    message("  Output dir     : ", output_path)
  message("========================================\n")

  invisible(list(
    tree_metrics = tree_metrics,
    plot_stats   = plot_stats,
    tree_report  = tree_report_file,
    plot_report  = plot_report_file
  ))
}
