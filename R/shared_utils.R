# ==============================================================================
# SHARED_UTILS - Funzioni condivise per il pacchetto PiC
# ==============================================================================
#
# Questo file contiene tutte le funzioni di utilita' condivise tra:
# - Voxels.R
# - Floseg.R
# - Forest_seg.R
# - SegOne.R
#
# OBIETTIVO: Eliminare duplicazioni e garantire consistenza
#
# Author: Roberto Ferrara, CNR-IBE
# ==============================================================================

# ==============================================================================
# SEZIONE 1: INPUT/OUTPUT UTILITIES
# ==============================================================================

#' Ensure the optional 'lidR' package is available
#'
#' Reading and writing LAS/LAZ files relies on the suggested package 'lidR'.
#' It is not a hard dependency, so this helper stops with an informative
#' message when it is missing.
#'
#' @keywords internal
#' @noRd
.require_lidR <- function() {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop(
      "Reading or writing LAS/LAZ files requires the 'lidR' package, ",
      "which is not installed. Install it with install.packages('lidR'), ",
      "or use '.xyz'/'.txt' point-cloud files instead.",
      call. = FALSE
    )
  }
}

#' @title Coerce input to standardized xyz data.table
#' @description
#' Unified function to convert various input formats to a standardized
#' data.table with columns x, y, z. Used by all PiC functions.
#' Supports text files (.xyz, .txt, .csv, .asc) and LiDAR files (.las, .laz).
#'
#' @param a Input data: file path, data.frame, matrix, or data.table
#' @param validate If TRUE, performs validation checks (default TRUE)
#'
#' @return data.table with columns x, y, z (numeric)
#'
#' @importFrom tools file_ext
#' @keywords internal
#' @noRd
.coerce_to_xyz_dt <- function(a, validate = TRUE) {

  # Check for NULL
  if (is.null(a)) {
    stop("Input 'a' is NULL", call. = FALSE)
  }

  # Case 1: File path
  if (is.character(a) && length(a) == 1) {
    if (!file.exists(a)) {
      stop("File not found: ", a, call. = FALSE)
    }
    ext <- tolower(tools::file_ext(a))
    if (ext %in% c("las", "laz")) {
      .require_lidR()
      las <- lidR::readLAS(a, select = "xyz")
      a <- data.table::as.data.table(las@data[, .(X, Y, Z)])
      data.table::setnames(a, c("X", "Y", "Z"), c("x", "y", "z"))
    } else {
      a <- data.table::as.data.table(.read_xyz_optimized(a))
    }
  }

  # Case 2: Data.frame, matrix, or data.table
  if (is.data.frame(a) || is.matrix(a)) {
    if (ncol(a) < 3) {
      stop("Input must have at least 3 columns (x, y, z)", call. = FALSE)
    }

    # Convert to data.table and keep only first 3 columns
    if (!data.table::is.data.table(a)) {
      a <- data.table::as.data.table(a)
    }
    a <- a[, 1:3]

    # Standardize column names
    data.table::setnames(a, 1:3, c("x", "y", "z"))

    # Ensure numeric
    a[, ':='(
      x = as.numeric(x),
      y = as.numeric(y),
      z = as.numeric(z)
    )]

    # Validation
    if (validate) {
      if (any(!is.finite(a$x) | !is.finite(a$y) | !is.finite(a$z))) {
        n_invalid <- sum(!is.finite(a$x) | !is.finite(a$y) | !is.finite(a$z))
        if (n_invalid / nrow(a) > 0.1) {
          stop("More than 10% of points have non-numeric or infinite values",
               call. = FALSE)
        }
        warning("Removed ", n_invalid, " invalid points", immediate. = TRUE)
        a <- a[is.finite(x) & is.finite(y) & is.finite(z)]
      }

      if (nrow(a) == 0) {
        stop("Input has 0 valid rows", call. = FALSE)
      }
    }

    return(a)
  }

  # Unsupported type
  stop("Unsupported input type. Expected: data.frame, matrix, data.table, or file path",
       call. = FALSE)
}


# ------------------------------------------------------------------------------
# Optimized XYZ file reader (formerly in utils-io.R)
# ------------------------------------------------------------------------------

#' @title Optimized XYZ file reader
#' @description
#' Reads point cloud files with advanced optimization features:
#' - Multi-core reading via data.table::fread
#' - Progress bar for large files
#' - Memory usage estimation and warnings
#' - Automatic separator detection (space, tab, comma)
#' - Robust error handling with fallbacks
#'
#' @param path Character string with file path
#' @param show_progress Logical; show progress bar for large files (default TRUE)
#' @param max_memory_gb Numeric; warn if estimated memory exceeds this (default 8)
#'
#' @return Data.frame with columns x, y, z (numeric)
#'
#' @keywords internal
#' @noRd
.read_xyz_optimized <- function(path, show_progress = TRUE, max_memory_gb = 8) {

  # ===== FILE VALIDATION =====
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  # ===== FILE SIZE ANALYSIS =====
  file_size_bytes <- file.size(path)
  file_size_mb <- file_size_bytes / (1024^2)
  file_size_gb <- file_size_bytes / (1024^3)

  # Estimate memory usage (rough: 3x file size for text - numeric conversion)
  estimated_memory_gb <- file_size_gb * 3

  # Memory warning for large files
  if (estimated_memory_gb > max_memory_gb) {
    warning(
      sprintf("Large file detected (%.1f GB)\n", file_size_gb),
      sprintf("Estimated memory usage: ~%.1f GB\n", estimated_memory_gb),
      sprintf("This may exceed available RAM (threshold: %.1f GB)\n", max_memory_gb),
      "Consider:\n",
      "  Closing other applications\n",
      "  Using a machine with more RAM\n",
      "  Downsampling the point cloud\n",
      immediate. = TRUE
    )
  }

  # Progress feedback
  if (show_progress && file_size_mb > 100) {
    message(sprintf("Reading %.1f MB file with %d CPU cores...",
                    file_size_mb,
                    max(1, parallel::detectCores() - 1)))
  }

  # ===== OPTIMIZED READING WITH MULTI-CORE =====

  # Determine optimal number of threads
  n_threads <- max(1, parallel::detectCores() - 1)

  # First attempt: Space-delimited (most common format)
  dt <- tryCatch(
    data.table::fread(
      path,
      sep = " ",
      header = FALSE,
      data.table = FALSE,
      showProgress = show_progress,
      strip.white = TRUE,
      colClasses = c("numeric", "numeric", "numeric"),
      select = 1:3,
      fill = TRUE,
      quote = "",
      nThread = n_threads,
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  # Fallback 1: Auto-detect separator
  if (is.null(dt)) {
    message("Space separator failed, trying auto-detection...")
    dt <- tryCatch(
      data.table::fread(
        path,
        sep = "auto",
        header = FALSE,
        data.table = FALSE,
        showProgress = show_progress,
        strip.white = TRUE,
        select = 1:3,
        fill = TRUE,
        quote = "",
        nThread = n_threads,
        verbose = FALSE
      ),
      error = function(e) NULL
    )
  }

  # Fallback 2: Read as character and parse manually
  if (is.null(dt)) {
    message("Auto-detection failed, trying character reading with manual parsing...")
    dt <- tryCatch({
      lines <- readLines(path, warn = FALSE)
      lines <- lines[nzchar(lines) & !grepl("^#", lines)]
      parsed <- strsplit(lines, "\\s+")
      x <- sapply(parsed, function(row) as.numeric(row[1]))
      y <- sapply(parsed, function(row) as.numeric(row[2]))
      z <- sapply(parsed, function(row) as.numeric(row[3]))
      data.frame(x = x, y = y, z = z)
    }, error = function(e) NULL)
  }

  # If all methods fail
  if (is.null(dt)) {
    stop(
      "Unable to read file: ", path, "\n",
      "Tried methods: space-delimited, auto-detection, manual parsing\n",
      "Check file format and ensure it has at least 3 numeric columns"
    )
  }

  # ===== DATA VALIDATION AND CLEANUP =====

  df <- as.data.frame(dt[, 1:3, drop = FALSE])
  colnames(df) <- c("x", "y", "z")
  df[] <- lapply(df, function(col) suppressWarnings(as.numeric(col)))

  if (nrow(df) == 0) {
    stop("File is empty or contains no valid data: ", path)
  }

  n_invalid <- sum(!is.finite(df$x) | !is.finite(df$y) | !is.finite(df$z))
  if (n_invalid > 0) {
    invalid_pct <- (n_invalid / nrow(df)) * 100

    if (invalid_pct > 10) {
      bad_rows <- which(!is.finite(df$x) | !is.finite(df$y) | !is.finite(df$z))
      stop(
        sprintf("%.1f%% of points have non-numeric/NA values (%d out of %d points)\n",
                invalid_pct, n_invalid, nrow(df)),
        "Example invalid rows: ", paste(head(bad_rows, 5), collapse = ", "), "\n",
        "Check file format, decimal separators (. vs ,), or corrupted data"
      )
    } else {
      warning(
        sprintf("Removed %d invalid points (%.2f%% of total)\n",
                n_invalid, invalid_pct),
        "These points had non-numeric or NA values",
        immediate. = TRUE
      )
      df <- df[is.finite(df$x) & is.finite(df$y) & is.finite(df$z), ]
    }
  }

  # ===== MEMORY USAGE REPORT =====
  actual_memory_gb <- as.numeric(object.size(df)) / (1024^3)

  if (show_progress) {
    message(sprintf(
      " Loaded %s points (%.2f MB file - %.2f GB in memory)",
      format(nrow(df), big.mark = ","),
      file_size_mb,
      actual_memory_gb
    ))
  }

  if (nrow(df) > 50e6) {
    message(" Large point cloud detected (>50M points)\n")
  }

  df
}


#' @title Legacy reader for compatibility
#' @description
#' Original reader function maintained for backward compatibility.
#' New code should use .read_xyz_optimized() instead.
#'
#' @param path Character string with file path
#'
#' @return Data.frame with columns x, y, z (numeric)
#'
#' @keywords internal
#' @noRd
.read_xyz_spaces <- function(path) {
  .read_xyz_optimized(path, show_progress = FALSE)
}


#' @title Estimate file reading time
#' @description
#' Estimates how long it will take to read a point cloud file
#' based on file size and system capabilities.
#'
#' @param path Character string with file path
#'
#' @return Character string with estimated time (e.g., "2 minutes")
#'
#' @keywords internal
#' @noRd
.estimate_read_time <- function(path) {

  if (!file.exists(path)) {
    return("File not found")
  }

  file_size_mb <- file.size(path) / (1024^2)
  reading_speed_mb_per_sec <- 50

  seconds <- file_size_mb / reading_speed_mb_per_sec

  if (seconds < 1) {
    return("< 1 second")
  } else if (seconds < 60) {
    return(paste0(round(seconds), " seconds"))
  } else if (seconds < 3600) {
    return(paste0(round(seconds/60), " minutes"))
  } else {
    hours <- floor(seconds / 3600)
    mins <- round((seconds %% 3600) / 60)
    return(paste0(hours, " hour", ifelse(hours > 1, "s", ""),
                  " ", mins, " minute", ifelse(mins != 1, "s", "")))
  }
}


#' @title Get file statistics
#' @description
#' Returns detailed statistics about a point cloud file without loading it.
#' Useful for previewing large files.
#'
#' @param path Character string with file path
#'
#' @return List with file statistics
#'
#' @keywords internal
#' @noRd
.get_file_stats <- function(path) {

  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  file_size_bytes <- file.size(path)
  file_size_mb <- file_size_bytes / (1024^2)
  file_size_gb <- file_size_bytes / (1024^3)

  sample_lines <- readLines(path, n = 100, warn = FALSE)
  sample_lines <- sample_lines[nzchar(sample_lines) & !grepl("^#", sample_lines)]

  if (length(sample_lines) > 0) {
    avg_line_bytes <- mean(nchar(sample_lines)) + 1
    estimated_rows <- floor(file_size_bytes / avg_line_bytes)
  } else {
    estimated_rows <- NA
  }

  estimated_memory_gb <- file_size_gb * 3
  read_time <- .estimate_read_time(path)

  list(
    file_path = path,
    file_size_mb = round(file_size_mb, 2),
    file_size_gb = round(file_size_gb, 2),
    estimated_rows = format(estimated_rows, big.mark = ","),
    estimated_memory_gb = round(estimated_memory_gb, 2),
    estimated_read_time = read_time,
    sample_lines = head(sample_lines, 3)
  )
}


# ==============================================================================
# SEZIONE 2: VOXELIZATION CORE
# ==============================================================================

#' @title Fast voxelization with data.table
#' @description
#' Core voxelization function used by all PiC tools.
#' Converts point cloud to voxel representation.
#'
#' @param points data.table with columns x, y, z
#' @param voxel_size Voxel size in meters
#' @param min_points Minimum points per voxel (default = 1)
#' @param return_points If TRUE, returns original points with voxel indices
#'
#' @return data.table with voxel indices (u, v, w) and counts (N)
#'
#' @keywords internal
#' @noRd
.voxelize_core <- function(points, voxel_size, min_points = 1L, return_points = FALSE) {

  # Ensure data.table
  if (!data.table::is.data.table(points)) {
    points <- data.table::as.data.table(points)
  }

  # Calculate voxel indices (vectorized)
  # +1L offset for positive indices and backward compatibility
  points[, ':='(
    u = as.integer(floor(x / voxel_size)) + 1L,
    v = as.integer(floor(y / voxel_size)) + 1L,
    w = as.integer(floor(z / voxel_size)) + 1L
  )]

  if (return_points) {
    return(points)
  }

  # Aggregate to voxel counts (fast data.table operation)
  voxels <- points[, .N, by = .(u, v, w)]

  # Apply threshold
  if (min_points > 1L) {
    voxels <- voxels[N >= min_points]
  }

  return(voxels)
}


#' @title Voxelize with statistics
#' @description
#' Voxelization with comprehensive statistics reporting.
#'
#' @param points data.table with columns x, y, z
#' @param voxel_size Voxel size in meters
#' @param min_points Minimum points per voxel
#'
#' @return List with voxels and stats
#'
#' @keywords internal
#' @noRd
.voxelize_with_stats <- function(points, voxel_size, min_points = 1L) {

  # First pass: all voxels
  voxels_all <- .voxelize_core(points, voxel_size, min_points = 1L)

  # Statistics
  stats <- list(
    n_voxels_total = nrow(voxels_all),
    mean_density = mean(voxels_all$N),
    median_density = stats::median(voxels_all$N),
    min_density = min(voxels_all$N),
    max_density = max(voxels_all$N)
  )

  # Apply filter
  if (min_points > 1L) {
    voxels_filtered <- voxels_all[N >= min_points]
    stats$n_voxels_filtered <- nrow(voxels_filtered)
    stats$n_voxels_removed <- stats$n_voxels_total - stats$n_voxels_filtered
    stats$removal_percent <- 100 * stats$n_voxels_removed / stats$n_voxels_total
  } else {
    voxels_filtered <- voxels_all
    stats$n_voxels_filtered <- stats$n_voxels_total
    stats$n_voxels_removed <- 0
    stats$removal_percent <- 0
  }

  return(list(voxels = voxels_filtered, stats = stats))
}


# ==============================================================================
# SEZIONE 3: PCA/LINEARITY CALCULATION
# ==============================================================================

#' @title Calculate PCA linearity for clusters
#' @description
#' Vectorized PCA-based linearity calculation for identifying cylindrical
#' structures (tree stems). Processes all clusters in one pass.
#'
#' @param voxels data.table with columns u, v, w, cls
#' @param cluster_ids Vector of cluster IDs to process
#'
#' @return data.table with columns cls, linearity
#'
#' @keywords internal
#' @noRd
.calculate_linearity <- function(voxels, cluster_ids) {

  data.table::setDT(voxels)

  # Process all clusters in one data.table operation
  pca_results <- voxels[cls %in% cluster_ids, {
    if (.N >= 3) {
      # Vectorized centering
      n <- .N
      u_mean <- sum(u) / n
      v_mean <- sum(v) / n
      w_mean <- sum(w) / n

      u_c <- u - u_mean
      v_c <- v - v_mean
      w_c <- w - w_mean

      # Covariance elements
      cov_scale <- 1.0 / (n - 1)
      cuu <- sum(u_c * u_c) * cov_scale
      cvv <- sum(v_c * v_c) * cov_scale
      cww <- sum(w_c * w_c) * cov_scale
      cuv <- sum(u_c * v_c) * cov_scale
      cuw <- sum(u_c * w_c) * cov_scale
      cvw <- sum(v_c * w_c) * cov_scale

      # Build covariance matrix
      cov_mat <- matrix(c(cuu, cuv, cuw,
                          cuv, cvv, cvw,
                          cuw, cvw, cww), nrow = 3, byrow = TRUE)

      # Eigenvalues only (faster)
      eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values

      # Linearity: (lambda1 - lambda2) / lambda1
      lin_val <- if (eig[1] > 1e-10) (eig[1] - eig[2]) / eig[1] else 0.0

      list(linearity = round(lin_val, 3))
    } else {
      list(linearity = 0.0)
    }
  }, by = cls]

  return(pca_results)
}


#' @title Calculate shape parameter R for clusters
#' @description
#' Alternative shape metric: R = sqrt(lambda1) * (lambda1 / sum(lambda))
#' Used by SegOne for single tree analysis.
#'
#' @param voxels data.table with columns u, v, w, cls
#' @param cluster_ids Vector of cluster IDs to process
#'
#' @return data.table with columns cls, r_param
#'
#' @keywords internal
#' @noRd
.calculate_shape_r <- function(voxels, cluster_ids) {

  data.table::setDT(voxels)

  shape_results <- voxels[cls %in% cluster_ids, {
    if (.N >= 3) {
      # Centering
      u_c <- u - mean(u)
      v_c <- v - mean(v)
      w_c <- w - mean(w)

      # Covariance elements
      cuu <- sum(u_c * u_c)
      cvv <- sum(v_c * v_c)
      cww <- sum(w_c * w_c)
      cuv <- sum(u_c * v_c)
      cuw <- sum(u_c * w_c)
      cvw <- sum(v_c * w_c)

      # Build covariance matrix
      cov_mat <- matrix(c(cuu, cuv, cuw,
                          cuv, cvv, cvw,
                          cuw, cvw, cww), nrow = 3, byrow = TRUE) / (.N - 1)

      # Eigenvalues
      eig_vals <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values

      # Shape parameter R
      total_var <- sum(eig_vals)
      if (total_var > 0) {
        p <- sqrt(eig_vals[1])
        q <- eig_vals[1] / total_var
        r_param <- p * q
      } else {
        r_param <- 0
      }

      list(r = round(r_param, 3), n_voxels = .N)
    } else {
      list(r = 0, n_voxels = .N)
    }
  }, by = cls]

  return(shape_results)
}


# ==============================================================================
# SEZIONE 4: COORDINATE SHIFT MANAGEMENT
# ==============================================================================

#' @title Apply global coordinate shift
#' @description
#' Shifts coordinates to origin for numerical stability.
#' Stores shift values as attributes.
#'
#' @param points data.table with x, y, z columns
#' @param decimals Decimal precision (default 3)
#'
#' @return data.table with shifted coordinates and shift attributes
#'
#' @keywords internal
#' @noRd
.apply_shift <- function(points, decimals = 3L) {

  data.table::setDT(points)

  # Calculate shift
  x_shift <- round(min(points$x), decimals)
  y_shift <- round(min(points$y), decimals)
  z_shift <- round(min(points$z), decimals)

  # Store as attributes
  attr(points, "x_shift") <- x_shift
  attr(points, "y_shift") <- y_shift
  attr(points, "z_shift") <- z_shift

  # Apply shift
  points[, ':='(
    x = round(x - x_shift, decimals),
    y = round(y - y_shift, decimals),
    z = round(z - z_shift, decimals)
  )]

  return(points)
}


#' @title Get shift values from data
#' @description
#' Retrieves stored shift values from attributes.
#'
#' @param points data.table with shift attributes
#'
#' @return List with x_shift, y_shift, z_shift
#'
#' @keywords internal
#' @noRd
.get_shift <- function(points) {

  x_shift <- attr(points, "x_shift")
  y_shift <- attr(points, "y_shift")
  z_shift <- attr(points, "z_shift")

  if (is.null(x_shift) || is.null(y_shift) || is.null(z_shift)) {
    return(list(x_shift = 0, y_shift = 0, z_shift = 0))
  }

  return(list(x_shift = x_shift, y_shift = y_shift, z_shift = z_shift))
}


#' @title Reverse global coordinate shift
#' @description
#' Restores original coordinates from shifted values.
#'
#' @param points data.table with x, y, z columns
#' @param shift_values List with x_shift, y_shift, z_shift
#'
#' @return data.table with original coordinates
#'
#' @keywords internal
#' @noRd
.reverse_shift <- function(points, shift_values) {

  data.table::setDT(points)

  points[, ':='(
    x = x + shift_values$x_shift,
    y = y + shift_values$y_shift,
    z = z + shift_values$z_shift
  )]

  return(points)
}


# ==============================================================================
# SEZIONE 5: DTM UTILITIES
# ==============================================================================

#' @title Extract DTM from point cloud
#' @description
#' Creates a Digital Terrain Model using minimum Z per grid cell.
#' Supports both coarse and fine resolution passes.
#'
#' @param points data.table with x, y, z columns
#' @param resolution Grid cell size in meters
#' @param clean_outliers Remove outliers (default TRUE)
#' @param outlier_k Z-score threshold (default 1)
#'
#' @return data.table with gx, gy, z_dtm columns
#'
#' @keywords internal
#' @noRd
.extract_dtm_grid <- function(points, resolution, clean_outliers = TRUE, outlier_k = 1) {

  data.table::setDT(points)

  # Calculate grid indices
  points[, ':='(
    gx = as.integer(floor(x / resolution)),
    gy = as.integer(floor(y / resolution))
  )]

  # DTM = minimum z per cell
  dtm <- points[, .(z_dtm = min(z)), by = .(gx, gy)]

  # Outlier removal
  if (clean_outliers && nrow(dtm) > 10) {
    # Rolling statistics for local outlier detection
    data.table::setorder(dtm, gx, gy)
    dtm[, z_smooth := data.table::frollmean(z_dtm, n = 5, align = "center", fill = NA)]
    dtm[is.na(z_smooth), z_smooth := z_dtm]
    dtm[, z_residual := z_dtm - z_smooth]

    residual_sd <- stats::sd(dtm$z_residual, na.rm = TRUE)
    if (!is.na(residual_sd) && residual_sd > 0) {
      outliers <- abs(dtm$z_residual) > (outlier_k * residual_sd)
      n_outliers <- sum(outliers, na.rm = TRUE)
      if (n_outliers > 0) {
        dtm <- dtm[!outliers]
      }
    }

    # Cleanup
    dtm[, ':='(z_smooth = NULL, z_residual = NULL)]
  }

  # Cleanup grid indices from points
  points[, ':='(gx = NULL, gy = NULL)]

  return(dtm)
}


#' @title Classify points using DTM
#' @description
#' Classifies points into floor, low vegetation, and AGB based on DTM.
#'
#' @param points data.table with x, y, z columns
#' @param dtm data.table with gx, gy, z_dtm columns
#' @param resolution Grid resolution used for DTM
#' @param floor_tolerance Max height for floor (default 0.1m)
#' @param vegetation_tolerance Max height for low vegetation (default 0.4m)
#'
#' @return List with floor_points, low_vegetation, AGB
#'
#' @keywords internal
#' @noRd
.classify_by_dtm <- function(points, dtm, resolution,
                              floor_tolerance = 0.10,
                              vegetation_tolerance = 0.40) {

  data.table::setDT(points)
  data.table::setDT(dtm)

  # Add grid indices to points
  points[, ':='(
    gx = as.integer(floor(x / resolution)),
    gy = as.integer(floor(y / resolution))
  )]

  # Join with DTM
  points[dtm, z_dtm := i.z_dtm, on = .(gx, gy)]

  # Classify
  floor_pts <- points[!is.na(z_dtm) & z <= (z_dtm + floor_tolerance), .(x, y, z)]
  low_veg <- points[!is.na(z_dtm) & z > (z_dtm + floor_tolerance) & z <= (z_dtm + vegetation_tolerance), .(x, y, z)]
  agb <- points[!is.na(z_dtm) & z > (z_dtm + vegetation_tolerance), .(x, y, z)]

  # Cleanup
  points[, ':='(gx = NULL, gy = NULL, z_dtm = NULL)]

  return(list(
    floor_points = floor_pts,
    low_vegetation = low_veg,
    AGB = agb,
    dtm_grid = dtm
  ))
}


# ==============================================================================
# SEZIONE 6: LOR (Ligneous Object Recognition) CORE
# ==============================================================================

#' @title LOR - Ligneous Object Recognition (core)
#' @description
#' Core LOR function used by Forest_seg and SegOne.
#' Identifies ligneous (wood) clusters through voxelization, DBSCAN, and shape filtering.
#'
#' @param AGB data.table with x, y, z columns
#' @param voxel_size Voxel size in meters
#' @param min_points Minimum points per voxel
#' @param eps DBSCAN epsilon (voxel units)
#' @param mpts DBSCAN minPts
#' @param min_height Minimum cluster height in meters (0 = no filter)
#' @param min_voxels Minimum voxels per cluster
#' @param quality_mode "linearity" or "shape_r"
#' @param quality_threshold Threshold for quality filter
#'
#' @return List with wood_voxels, cluster_stats, valid_clusters
#'
#' @keywords internal
#' @noRd
.lor_segment_core <- function(AGB, voxel_size, min_points = 2L,
                                eps = 2, mpts = 9,
                                min_height = 3.0, min_voxels = 10000L,
                                quality_mode = "linearity",
                                quality_threshold = 0.5) {

  data.table::setDT(AGB)

  # Voxelize
  voxels <- .voxelize_core(AGB, voxel_size, min_points, return_points = FALSE)

  if (nrow(voxels) == 0) {
    stop("No voxels after filtering", call. = FALSE)
  }

  # Spatial sorting (Z -> Y -> X for vertical structure)
  data.table::setorder(voxels, w, v, u)

  # DBSCAN clustering
  b <- dbscan::dbscan(voxels[, .(u, v, w)], eps = eps, minPts = mpts)
  voxels[, cls := b$cluster]

  n_clusters <- max(voxels$cls)
  if (n_clusters == 0) {
    stop("No clusters found by DBSCAN", call. = FALSE)
  }

  # Cluster statistics
  cluster_stats <- voxels[cls > 0, .(
    n_voxels = .N,
    n_points = sum(N),
    delta_w = max(w) - min(w),
    delta_u = max(u) - min(u),
    delta_v = max(v) - min(v)
  ), by = cls]

  # Size and height filtering
  min_height_voxels <- min_height / voxel_size
  candidates <- cluster_stats[n_voxels >= min_voxels & delta_w >= min_height_voxels, cls]

  if (length(candidates) == 0) {
    stop("No clusters meet size/height requirements", call. = FALSE)
  }

  # Quality filtering
  if (quality_mode == "linearity") {
    quality_results <- .calculate_linearity(voxels, candidates)
    data.table::setnames(quality_results, "linearity", "quality")
    valid_clusters <- quality_results[quality > quality_threshold, cls]
  } else {
    # Shape R mode (SegOne)
    quality_results <- .calculate_shape_r(voxels, candidates)
    data.table::setnames(quality_results, "r", "quality")
    valid_clusters <- quality_results[quality > quality_threshold, cls]
  }

  if (length(valid_clusters) == 0) {
    stop("No clusters pass quality filter", call. = FALSE)
  }

  # Merge quality to cluster_stats
  cluster_stats <- merge(cluster_stats, quality_results, by = "cls", all.x = TRUE)

  return(list(
    voxels = voxels,
    cluster_stats = cluster_stats,
    valid_clusters = valid_clusters
  ))
}


#' @title Map voxel clusters back to points
#' @description
#' Assigns cluster IDs to original points based on voxel membership.
#'
#' @param points data.table with x, y, z columns (and u, v, w from voxelization)
#' @param voxels data.table with u, v, w, cls columns
#' @param valid_clusters Vector of valid cluster IDs
#'
#' @return data.table with x, y, z, cls columns
#'
#' @keywords internal
#' @noRd
.map_clusters_to_points <- function(points, voxels, valid_clusters) {

  data.table::setDT(points)
  data.table::setDT(voxels)

  # Ensure points have voxel indices
  if (!all(c("u", "v", "w") %in% names(points))) {
    stop("Points must have u, v, w columns", call. = FALSE)
  }

  # Create lookup table
  cluster_lookup <- voxels[cls %in% valid_clusters, .(u, v, w, cls)]
  data.table::setkey(cluster_lookup, u, v, w)
  data.table::setkey(points, u, v, w)

  # Join
  points_with_cls <- cluster_lookup[points]

  return(points_with_cls)
}


# ==============================================================================
# SEZIONE 7: OUTPUT UTILITIES
# ==============================================================================

#' @title Format number with thousands separator
#' @description
#' Helper for consistent number formatting in messages.
#'
#' @param x Numeric value
#'
#' @return Formatted string
#'
#' @keywords internal
#' @noRd
.fmt_num <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}


#' @title Print voxelization summary
#' @description
#' Consistent summary message for voxelization operations.
#'
#' @param stats Voxelization statistics list
#' @param voxel_size Voxel size in meters
#' @param min_points Minimum points threshold
#'
#' @keywords internal
#' @noRd
.print_voxel_summary <- function(stats, voxel_size, min_points) {

  message("  Voxelization (", round(voxel_size * 100, 1), " cm, min=", min_points, " pts):")
  message("    Total voxels: ", .fmt_num(stats$n_voxels_total))
  message("    Filtered: ", .fmt_num(stats$n_voxels_filtered),
          " (removed ", .fmt_num(stats$n_voxels_removed),
          ", ", round(stats$removal_percent, 1), "%)")
  message("    Density: mean=", round(stats$mean_density, 1),
          ", median=", round(stats$median_density, 0),
          ", range=", stats$min_density, "-", stats$max_density, " pts/voxel")
}


# ==============================================================================
# SEZIONE 8: DBH CALCULATION
# ==============================================================================

#' @title Calculate DBH using circle fitting with cylindrical pre-filter
#' @description
#' Core DBH calculation using Pratt and Landau circle fitting methods.
#' A cylindrical fit is first performed on points in [cyl_z_low, cyl_z_high]
#' (default 1.2 m to 1.8 m, 60 cm window). If the cylinder fit fails the
#' function returns NA with no result. Only heights within the cylinder window
#' are processed; points at those heights are filtered to cylinder inliers
#' (radial distance within one cylinder-RMSE of the fitted radius) before the
#' final circle fit.
#'
#' @param wood_points data.table with x, y, z columns for a single tree
#' @param z_base Tree base elevation
#' @param heights Heights to try in order (default: c(1.3, 1.6)); only
#'   heights within [cyl_z_low, cyl_z_high] are processed — others are skipped
#' @param tolerance Vertical tolerance for the DBH slice (default: 0.05 m)
#' @param max_rmse Maximum acceptable RMSE for the circle fit (default: 0.05 m)
#' @param min_radius Minimum valid radius (default: 0.025 m)
#' @param max_radius Maximum valid radius (default: 1.5 m)
#' @param min_points Minimum points required (default: 5)
#' @param cyl_z_low Lower bound of the cylindrical fit window above base
#'   (default: 1.2 m)
#' @param cyl_z_high Upper bound of the cylindrical fit window above base
#'   (default: 1.8 m)
#'
#' @return List with DBH, DBH_height, DBH_method, DBH_RMSE, DBH_valid,
#'   DBH_cyl_radius_cm, DBH_cyl_rmse_cm
#'
#' @keywords internal
#' @noRd
.calculate_dbh_core <- function(wood_points, z_base,
                                 heights = c(1.3, 1.6),
                                 tolerance = 0.05,
                                 max_rmse = 0.05,
                                 min_radius = 0.025,
                                 max_radius = 1.5,
                                 min_points = 5L,
                                 cyl_z_low  = 1.2,
                                 cyl_z_high = 1.8) {

  result <- list(
    DBH             = NA_real_,
    DBH_height      = NA_real_,
    DBH_method      = NA_character_,
    DBH_RMSE        = NA_real_,
    DBH_valid       = FALSE,
    DBH_cyl_radius_cm = NA_real_,
    DBH_cyl_rmse_cm   = NA_real_
  )

  data.table::setDT(wood_points)

  # ===========================================================================
  # STEP 1: Cylindrical fit on [cyl_z_low, cyl_z_high] (assuming vertical axis)
  # ===========================================================================
  cyl_pts <- wood_points[
    z >= (z_base + cyl_z_low) & z <= (z_base + cyl_z_high),
    .(x, y)
  ]

  cx        <- NA_real_
  cy        <- NA_real_
  R_cyl     <- NA_real_
  rmse_cyl  <- NA_real_

  if (nrow(cyl_pts) >= min_points) {
    cyl_matrix <- as.matrix(cyl_pts)

    fit_cyl <- tryCatch(
      suppressWarnings(conicfit::CircleFitByPratt(cyl_matrix)),
      error = function(e) NULL
    )

    if (!is.null(fit_cyl) && fit_cyl[3] >= min_radius && fit_cyl[3] <= max_radius) {
      cx    <- fit_cyl[1]
      cy    <- fit_cyl[2]
      R_cyl <- fit_cyl[3]
      r_res <- sqrt((cyl_matrix[, 1] - cx)^2 + (cyl_matrix[, 2] - cy)^2)
      rmse_cyl <- sqrt(mean((r_res - R_cyl)^2))
      result$DBH_cyl_radius_cm <- round(R_cyl * 100, 1)
      result$DBH_cyl_rmse_cm   <- round(rmse_cyl * 100, 2)
    }
  }

  # If the cylinder fit failed, return immediately with no result
  if (is.na(R_cyl) || is.na(rmse_cyl)) return(result)

  # ===========================================================================
  # STEP 2: Circle fit — only heights within the cylinder window are processed
  # ===========================================================================
  for (h in heights) {
    if (result$DBH_valid) break

    # Skip heights outside the cylinder window
    if (h < cyl_z_low || h > cyl_z_high) next

    z_min <- z_base + h - tolerance
    z_max <- z_base + h + tolerance

    # Apply cylinder inlier filter: keep points within one RMSE of the
    # cylinder radius (adaptive band based on the cylinder fit quality)
    all_slice <- wood_points[z >= z_min & z <= z_max]
    r_axis    <- sqrt((all_slice$x - cx)^2 + (all_slice$y - cy)^2)
    slice     <- all_slice[abs(r_axis - R_cyl) <= rmse_cyl, .(x, y)]

    if (nrow(slice) < min_points) next

    pts_matrix <- as.matrix(slice)

    # Try Pratt fit
    fit_pratt <- tryCatch(
      suppressWarnings(conicfit::CircleFitByPratt(pts_matrix)),
      error = function(e) NULL
    )

    r_pratt    <- NA_real_
    rmse_pratt <- NA_real_
    valid_pratt <- FALSE

    if (!is.null(fit_pratt)) {
      r_pratt <- fit_pratt[3]
      dist_sq <- (pts_matrix[, 1] - fit_pratt[1])^2 + (pts_matrix[, 2] - fit_pratt[2])^2
      rmse_pratt <- sqrt(mean((sqrt(dist_sq) - r_pratt)^2))
      valid_pratt <- rmse_pratt <= max_rmse && r_pratt >= min_radius && r_pratt <= max_radius
    }

    # Try Landau fit
    fit_landau <- tryCatch(
      suppressWarnings(conicfit::CircleFitByLandau(pts_matrix)),
      error = function(e) NULL
    )

    r_landau    <- NA_real_
    rmse_landau <- NA_real_
    valid_landau <- FALSE

    if (!is.null(fit_landau)) {
      r_landau <- fit_landau[3]
      dist_sq <- (pts_matrix[, 1] - fit_landau[1])^2 + (pts_matrix[, 2] - fit_landau[2])^2
      rmse_landau <- sqrt(mean((sqrt(dist_sq) - r_landau)^2))
      valid_landau <- rmse_landau <= max_rmse && r_landau >= min_radius && r_landau <= max_radius
    }

    # Select best fit
    if (valid_pratt || valid_landau) {
      if (valid_pratt && valid_landau) {
        use_pratt <- rmse_pratt <= rmse_landau
      } else {
        use_pratt <- valid_pratt
      }

      if (use_pratt) {
        result$DBH        <- 2 * r_pratt
        result$DBH_height <- h
        result$DBH_method <- "Pratt"
        result$DBH_RMSE   <- rmse_pratt
        result$DBH_valid  <- TRUE
      } else {
        result$DBH        <- 2 * r_landau
        result$DBH_height <- h
        result$DBH_method <- "Landau"
        result$DBH_RMSE   <- rmse_landau
        result$DBH_valid  <- TRUE
      }
    }
  }

  return(result)
}


# ==============================================================================
# GLOBAL VARIABLES DECLARATION
# ==============================================================================

utils::globalVariables(c(
  "u", "v", "w", "N", "x", "y", "z", "cls",
  "X", "Y", "Z",
  "gx", "gy", "z_dtm", "z_smooth", "z_residual",
  "x_shift", "y_shift", "z_shift"
))
