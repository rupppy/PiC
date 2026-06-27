# ==============================================================================
# FLOSEG v2.3 - DTM-Based Forest Floor Segmentation
# ==============================================================================
#
# PiC Package - Pointcloud Interactive Computation
# Optimized forest floor extraction using fine DTM interpolation
#
# Version 2.3 - January 2026
#
# CHANGELOG v2.3:
# - Refactored to use shared_utils.R functions
# - Uses .coerce_to_xyz_dt() for input handling
# - Removed duplicate .coerce_to_xyz_floseg() function
# - Cleaner code with better separation of concerns
#
# Author: Roberto Ferrara, CNR-IBE
# Email: roberto.ferrara@cnr.it
# ==============================================================================

#' @name Floseg
#' @title Forest floor segmentation with DTM validation
#'
#' @description 
#' Segments the input point cloud into forest floor and above-ground biomass (AGB)
#' using a two-stage DTM approach:
#' 1. Create coarse DTM (e.g., 0.5m resolution) with outlier removal and gap filling
#' 2. Interpolate fine DTM_small (0.1m resolution) from filled coarse DTM
#' 3. Extract points within vertical tolerance of DTM_small surface
#' 
#' This approach is robust on sloped or irregular terrain.
#'
#' @param a Input point cloud data frame (x,y,z columns) or file path (.xyz)
#' @param filename Output file prefix (default = "XXX")
#' @param dtm_coarse_res Coarse DTM resolution in meters (default = 0.5)
#' @param dtm_fine_res Fine DTM resolution in meters (default = 0.1)
#' @param tolerance Vertical tolerance for floor extraction in meters (default = 0.4)
#' @param clean_outliers Remove local outliers from coarse DTM (default = TRUE)
#' @param outlier_k Z-score threshold for outlier removal (default = 1)
#' @param output_path Directory where output files will be written (default = tempdir())
#'
#' @return List with floor_points, agb_points, dtm_coarse, dtm_fine, and file paths
#'
#' @importFrom tictoc tic toc
#' @importFrom terra ext rast rasterize focal ifel as.data.frame
#' @importFrom data.table fwrite fread setDT data.table := setkey setnames CJ
#'
#' @export

Floseg <- function(a,
                   filename = "XXX",
                   dtm_coarse_res = 0.5,
                   dtm_fine_res = 0.1,
                   tolerance = 0.4,
                   clean_outliers = TRUE,
                   outlier_k = 1,
                   output_path = tempdir()) {

  # Start timing
  tictoc::tic('Forest Floor segmentation (DTM-based)')

  # Create output directory if needed
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Load and validate input using shared function
  a <- .coerce_to_xyz_dt(a)

  message("\n", rep("=", 70))
  message("  FLOSEG v2.3 - Simplified DTM-Based Floor Segmentation (Shared Utils)")
  message(rep("=", 70))
  message("\nInput: ", format(nrow(a), big.mark = ","), " points")
  message("Coarse DTM resolution: ", dtm_coarse_res, " m")
  message("Fine DTM resolution: ", dtm_fine_res, " m")
  message("Vertical tolerance: ", tolerance, " m")
  
  # ==========================================================================
  # STEP 1: COARSE DTM CREATION
  # ==========================================================================
  
  message("\n[1/4] Creating coarse DTM...")
  
  data.table::setDT(a)
  if (!all(c("x", "y", "z") %in% names(a))) {
    data.table::setnames(a, 1:3, c("x", "y", "z"))
  }
  
  # Calculate DTM grid coordinates for input points
  a[, ':='(
    u_dtm = as.integer(floor(x / dtm_coarse_res)),
    v_dtm = as.integer(floor(y / dtm_coarse_res))
  )]
  
  # Coarse DTM = minimum z in each cell
  DTM_coarse <- a[, .(z_dtm = min(z)), by = .(u_dtm, v_dtm)]
  data.table::setkey(DTM_coarse, u_dtm, v_dtm)
  
  message("  Coarse DTM cells: ", format(nrow(DTM_coarse), big.mark = ","))
  message("  Coarse DTM z range: ", round(min(DTM_coarse$z_dtm), 2), " - ", 
          round(max(DTM_coarse$z_dtm), 2), " m")
  
  # ==========================================================================
  # STEP 2: COARSE DTM OUTLIER REMOVAL AND GAP FILLING
  # ==========================================================================
  
  if (clean_outliers) {
    message("\n[2/4] Cleaning outliers and filling gaps in coarse DTM...")
    
    # SUBSTEP 2.1: Remove outliers
    DTM_coarse <- .clean_dtm_outliers_local(DTM_coarse, k = outlier_k)
    
    n_before_fill <- nrow(DTM_coarse)
    message("  DTM cells after outlier removal: ", format(n_before_fill, big.mark = ","))
    
    # SUBSTEP 2.2: Gap filling using terra rasterization and focal mean
    # Temporarily rename columns for rasterization
    colnames(DTM_coarse) <- c('u', 'v', 'w')
    DTM_coarse <- as.data.frame(DTM_coarse)
    
    # Create extent with small buffer
    ext <- terra::ext(
      range(DTM_coarse$u)[1] - 0.5, 
      range(DTM_coarse$u)[2] + 0.5,
      range(DTM_coarse$v)[1] - 0.5, 
      range(DTM_coarse$v)[2] + 0.5
    )
    
    # Create raster template (resolution = 1 grid unit)
    dtm_raster <- terra::rast(ext, resolution = 1)
    
    # Rasterize DTM values
    dtm_raster <- terra::rasterize(
      x = DTM_coarse[, c("u", "v")], 
      y = dtm_raster, 
      values = DTM_coarse$w, 
      fun = mean
    )
    
    # Fill gaps using 3x3 focal mean
    focal_filled <- terra::focal(
      x = dtm_raster, 
      w = 3, 
      fun = mean, 
      na.rm = TRUE, 
      na.policy = "only"
    )
    
    # Replace NA cells with focal estimates
    DTM_filled <- terra::ifel(is.na(dtm_raster), focal_filled, dtm_raster)
    
    # Convert back to data.table
    dtm_df <- terra::as.data.frame(DTM_filled, xy = TRUE, na.rm = TRUE)
    DTM_coarse <- data.table::as.data.table(dtm_df)
    colnames(DTM_coarse) <- c('u_dtm', 'v_dtm', 'z_dtm')
    
    n_after_fill <- nrow(DTM_coarse)
    n_filled <- n_after_fill - n_before_fill
    
    message("  DTM cells after gap filling: ", format(n_after_fill, big.mark = ","))
    if (n_filled > 0) {
      message("  Gaps filled: ", format(n_filled, big.mark = ","))
    }
    
  } else {
    message("\n[2/4] Skipping outlier removal and gap filling...")
  }
  
  # Ensure DTM_coarse has correct column names and is keyed
  data.table::setkey(DTM_coarse, u_dtm, v_dtm)
  
  # ==========================================================================
  # STEP 3: FINE DTM INTERPOLATION FROM FILLED COARSE DTM
  # ==========================================================================
  
  message("\n[3/4] Interpolating fine DTM from filled coarse DTM...")
  
  # Get spatial extent from COARSE DTM (not from original points!)
  # Convert grid indices back to actual coordinates
  u_min_coord <- min(DTM_coarse$u_dtm) * dtm_coarse_res
  u_max_coord <- (max(DTM_coarse$u_dtm) + 1) * dtm_coarse_res
  v_min_coord <- min(DTM_coarse$v_dtm) * dtm_coarse_res
  v_max_coord <- (max(DTM_coarse$v_dtm) + 1) * dtm_coarse_res
  
  # Create fine grid covering the same extent as coarse DTM
  u_fine_seq <- seq(
    floor(u_min_coord / dtm_fine_res), 
    ceiling(u_max_coord / dtm_fine_res)
  )
  v_fine_seq <- seq(
    floor(v_min_coord / dtm_fine_res), 
    ceiling(v_max_coord / dtm_fine_res)
  )
  
  DTM_fine <- data.table::CJ(u_fine = u_fine_seq, v_fine = v_fine_seq)
  
  # Convert fine grid indices to actual coordinates
  DTM_fine[, ':='(
    x_fine = (u_fine + 0.5) * dtm_fine_res,
    y_fine = (v_fine + 0.5) * dtm_fine_res
  )]
  
  # Map fine grid points to coarse grid
  DTM_fine[, ':='(
    u_coarse = as.integer(floor(x_fine / dtm_coarse_res)),
    v_coarse = as.integer(floor(y_fine / dtm_coarse_res))
  )]
  
  # Join with FILLED coarse DTM to get interpolated elevations
  data.table::setkey(DTM_fine, u_coarse, v_coarse)
  
  # Perform join - keep only fine cells that have coarse DTM data
  DTM_fine <- DTM_coarse[DTM_fine, nomatch = 0]
  
  # Rename for clarity
  data.table::setnames(DTM_fine, 
                       old = c("u_dtm", "v_dtm", "z_dtm"),
                       new = c("u_coarse_source", "v_coarse_source", "z_fine"))
  
  # Keep only essential columns
  DTM_fine <- DTM_fine[, .(u_fine, v_fine, x_fine, y_fine, z_fine)]
  
  message("  Fine DTM cells: ", format(nrow(DTM_fine), big.mark = ","))
  message("  Fine DTM z range: ", round(min(DTM_fine$z_fine), 2), " - ", 
          round(max(DTM_fine$z_fine), 2), " m")
  
  # ==========================================================================
  # STEP 4: POINT EXTRACTION BASED ON DTM_FINE
  # ==========================================================================
  
  message("\n[4/4] Extracting forest floor points...")
  
  # Map input points to fine grid
  a[, ':='(
    u_fine = as.integer(floor(x / dtm_fine_res)),
    v_fine = as.integer(floor(y / dtm_fine_res))
  )]
  
  # Join points with fine DTM
  data.table::setkey(a, u_fine, v_fine)
  data.table::setkey(DTM_fine, u_fine, v_fine)
  
  # Add DTM elevation to each point
  a_with_dtm <- DTM_fine[a, nomatch = 0]
  
  # Calculate vertical distance from DTM surface
  a_with_dtm[, z_diff := abs(z - z_fine)]
  
  # Classify points
  a_with_dtm[, is_floor := z_diff <= tolerance]
  
  # Extract floor and AGB points
  Forest_floor <- a_with_dtm[is_floor == TRUE, .(x, y, z)]
  AGB <- a_with_dtm[is_floor == FALSE, .(x, y, z)]
  
  message("  Floor points: ", format(nrow(Forest_floor), big.mark = ","))
  message("  AGB points: ", format(nrow(AGB), big.mark = ","))
  
  # ==========================================================================
  # VALIDATION
  # ==========================================================================
  
  total_extracted <- nrow(Forest_floor) + nrow(AGB)
  total_with_dtm <- nrow(a_with_dtm)
  total_original <- nrow(a)
  
  message("\nValidation:")
  message("  Original points: ", format(total_original, big.mark = ","))
  message("  Points with DTM coverage: ", format(total_with_dtm, big.mark = ","))
  message("  Extracted (floor + AGB): ", format(total_extracted, big.mark = ","))
  
  if (total_with_dtm < total_original) {
    n_outside <- total_original - total_with_dtm
    pct_outside <- round(100 * n_outside / total_original, 1)
    message("  Points outside DTM coverage: ", format(n_outside, big.mark = ","),
            " (", pct_outside, "%)")
  }
  
  if (total_extracted == total_with_dtm) {
    message("\n Validation passed: All DTM-covered points accounted for")
  }
  
  # ==========================================================================
  # SAVE OUTPUT FILES
  # ==========================================================================
  
  floor_file <- file.path(output_path, paste0(filename, '_Forest_floor.txt'))
  agb_file <- file.path(output_path, paste0(filename, '_AGB.txt'))
  dtm_file <- file.path(output_path, paste0(filename, '_DTM.txt'))
  
  data.table::fwrite(Forest_floor, floor_file, sep = " ", col.names = FALSE)
  data.table::fwrite(AGB, agb_file, sep = " ", col.names = FALSE)
  data.table::fwrite(DTM_coarse, dtm_file, sep = " ", col.names = FALSE)
  
  message("\nOutput files:")
  message("  Floor: ", floor_file)
  message("  AGB: ", agb_file)
  message("  DTM (coarse): ", dtm_file)
  
  message("\n", rep("=", 70))
  message("  SEGMENTATION COMPLETED")
  message(rep("=", 70))
  
  tictoc::toc()
  
  # Return results
  return(list(
    floor_points = Forest_floor,
    agb_points = AGB,
    dtm_coarse = DTM_coarse,
    dtm_fine = DTM_fine,
    floor_file = floor_file,
    agb_file = agb_file,
    dtm_file = dtm_file
  ))
}


# ==============================================================================
# HELPER FUNCTION: DTM OUTLIER REMOVAL (works on DTM grid, not input points)
# ==============================================================================

#' @title Clean local outliers from DTM grid
#' @description Removes outliers based on local neighborhood statistics (5x5 window)
#'              Applied to DTM grid cells, not to input point cloud
#' @param dtm DTM data.table with u_dtm, v_dtm, z_dtm columns
#' @param k Z-score threshold multiplier (default = 3)
#' @return Cleaned DTM data.table without outliers
#' @keywords internal
#' @noRd
.clean_dtm_outliers_local <- function(dtm, k = 3) {
  
  data.table::setDT(dtm)
  
  # Define 5x5 neighborhood offsets (24 neighbors, excluding center)
  offsets <- data.table::CJ(du = -2:2, dv = -2:2)[!(du == 0 & dv == 0)]
  
  # Create a list to store neighbor elevations for each DTM cell
  neighbor_lists <- vector("list", nrow(dtm))
  
  # Build a lookup table for fast DTM access
  dtm_lookup <- dtm[, .(u_dtm, v_dtm, z_dtm)]
  data.table::setkey(dtm_lookup, u_dtm, v_dtm)
  
  # For each DTM cell, collect all neighbor elevations
  for (offset_idx in seq_len(nrow(offsets))) {
    du <- offsets$du[offset_idx]
    dv <- offsets$dv[offset_idx]
    
    # Create shifted coordinates
    shifted <- data.table::data.table(
      idx = seq_len(nrow(dtm)),
      u_neighbor = dtm$u_dtm + du,
      v_neighbor = dtm$v_dtm + dv
    )
    
    # Find matching neighbors in DTM
    data.table::setkey(shifted, u_neighbor, v_neighbor)
    matches <- dtm_lookup[shifted, nomatch = 0]
    
    # Store neighbor elevations
    for (j in seq_len(nrow(matches))) {
      idx <- matches$idx[j]
      neighbor_lists[[idx]] <- c(neighbor_lists[[idx]], matches$z_dtm[j])
    }
  }
  
  # Calculate local statistics for each cell
  loc_median <- vapply(neighbor_lists, function(x) {
    if (length(x) >= 3) median(x, na.rm = TRUE) else NA_real_
  }, numeric(1))
  
  loc_sd <- vapply(neighbor_lists, function(x) {
    if (length(x) >= 3) sd(x, na.rm = TRUE) else NA_real_
  }, numeric(1))
  
  # Add statistics to DTM
  dtm[, ':='(loc_median = loc_median, loc_sd = loc_sd)]
  
  # Filter outliers: keep DTM cells within k standard deviations
  # Keep cells with NA statistics (edge cells with <3 neighbors)
  dtm_clean <- dtm[
    is.na(loc_sd) | (abs(z_dtm - loc_median) < k * loc_sd),
    .(u_dtm, v_dtm, z_dtm)
  ]
  
  n_removed <- nrow(dtm) - nrow(dtm_clean)
  if (n_removed > 0) {
    message("  Outliers removed: ", format(n_removed, big.mark = ","))
  }
  
  return(dtm_clean)
}


# ==============================================================================
# HELPER FUNCTION: INPUT VALIDATION
# ==============================================================================
# Uses shared function .coerce_to_xyz_dt() from shared_utils.R
# Removed duplicate .coerce_to_xyz_floseg() function