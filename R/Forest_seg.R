#' @title Forest component segmentation
#' @name Forest_seg
#' @description Segments an input .xyz point cloud file into different forestry layers (soil, wood, foliage), computes individual tree metrics, and provides summary statistics and canopy metrics.
#' @param a Input point cloud data frame (.xyz) or file path
#' @param filename Output file prefix
#' @param dimVox Voxel dimension (cm) for wood segmentation (default = 2)
#' @param th Minimum number of points to generate a voxel (default = 2)
#' @param eps Epsilon neighborhood radius for DBSCAN (default = 2)
#' @param mpts Minimum points required in eps neighborhood for core points (default = 9)
#' @param h_tree Minimum trunk length in meters (default = 1)
#' @param soil_dim Voxel dimension (m) for forest floor segmentation (default = 0.1)
#' @param N Minimum number of voxels in a wood cluster (default = 500)
#' @param R Cluster shape parameter threshold (default = 30)
#' @param Vox_print Logical; if TRUE, saves point cloud voxelization (default = FALSE)
#' @param WoodVox_print Logical; if TRUE, saves wood voxelization (default = FALSE)
#' @param output_path Output directory (default = tempdir())
#' @param analyze_canopy Logical; if TRUE, performs canopy analysis (default = TRUE)
#' @param canopy_voxel_size Voxel size for canopy analysis in meters (default = 0.1)
#' @param min_canopy_height Minimum height threshold for canopy analysis (default = 1.5)
#' @param coverage_method Method for calculating coverage degree (default = "mean_normalized")
#' @return List containing file paths and metrics for trees and canopy.
#' @importFrom tictoc tic toc
#' @importFrom dbscan dbscan
#' @importFrom data.table fwrite fread setDT setkey data.table as.data.table fdroplevels between fifelse setnames
#' @importFrom magrittr %>%
#' @importFrom dplyr anti_join
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom collapse fcount na_omit
#' @importFrom stats prcomp na.omit
#' @importFrom utils read.csv
#' @importFrom conicfit CircleFitByPratt
#' @importFrom sf st_convex_hull st_area st_multipoint
#' @export
Forest_seg <- function(a, filename = "XXX", dimVox = 2, th = 2,
                       eps = 2, mpts = 9, h_tree = 1, soil_dim = 0.1,
                       N = 500, R = 30, Vox_print = FALSE, WoodVox_print = FALSE,
                       output_path = tempdir(), analyze_canopy = TRUE, 
                       canopy_voxel_size = 0.1, min_canopy_height = 1.5, 
                       coverage_method = "mean_normalized") {
  
  tic('Total time')
  
  # Normalize and create output directory
  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  if (!dir.exists(output_path)) {
    ok <- dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    if (!ok) stop("Cannot create output directory: ", output_path)
  }
  
  # Coerce input to x,y,z data frame (assumes .coerce_to_xyz defined elsewhere)
  a <- .coerce_to_xyz(a)
  
  plot <- paste0(filename, "_dim", dimVox, "_th", th)
  dim <- dimVox / 100
  
  # Stage 1: Forest floor segmentation
  message("Stage 1: Forest floor segmentation")
  floor_results <- extract_forest_floor(a, soil_dim, th, N, output_path, plot)
  Forest_floor <- floor_results$floor_points
  AGB <- floor_results$agb_points
  
  # Stage 2: Wood segmentation
  message("Stage 2: Wood segmentation")
  wood_results <- segment_wood(AGB, dim, th, eps, mpts, h_tree, N, R,
                               Vox_print, WoodVox_print, plot, output_path)
  woodpoint <- wood_results$wood_points
  
  # Stage 3: Foliage segmentation
  message("Stage 3: Separating foliage from wood")
  AGB_def <- separate_foliage(AGB, woodpoint, plot, eps, mpts, output_path)
  
  # Stage 4: Tree and canopy metrics
  if(isTRUE(analyze_canopy)) {
    message("Stage 4: Calculating tree metrics and canopy analysis")
    reports <- Calculate_trees_metrics(
      woodpoint = woodpoint,
      a = a, 
      AGB_def = AGB_def,
      Forest_floor = Forest_floor,
      plot = plot,
      filename = filename,
      output_path = output_path,
      canopy_voxel_size = canopy_voxel_size,
      min_canopy_height = min_canopy_height,
      coverage_method = coverage_method
    )
    
    tree_report_file <- file.path(output_path, paste0(plot, "_tree_report.csv"))
    plot_report_file <- file.path(output_path, paste0(plot, "_plot_report.csv"))
    
    message("\nGenerated reports:")
    message("1. Individual tree report: ", tree_report_file)
    message("2. Study area summary report: ", plot_report_file)
    
    toc()
    return(list(
      tree_report = tree_report_file,
      plot_report = plot_report_file,
      tree_metrics = reports$tree_metrics,
      canopy_metrics = reports$canopy_metrics
    ))
  } else {
    toc()
    return(list(
      forest_floor = file.path(output_path, paste0(plot, '_Forest_floor_soil_dim_', soil_dim, '.txt')),
      wood = file.path(output_path, paste0(plot, "_Wood_eps", eps, "_mpts", mpts, ".txt")),
      foliage = file.path(output_path, paste0(plot, "_AGBnoWOOD_eps", eps, "_mpts", mpts, ".txt"))
    ))
  }
}

################################
# Calculate_trees_metrics
################################
#' @title Calculate tree and canopy metrics
#' @description Computes metrics for individual trees and forest canopy from segmented point clouds.
#' @param woodpoint Wood points (trunks and branches) with cluster attribute
#' @param a Original point cloud
#' @param AGB_def Non-wood (foliage) points
#' @param Forest_floor Forest floor points
#' @param plot Plot/output file prefix
#' @param filename Original file prefix
#' @param output_path Output directory
#' @param canopy_voxel_size Voxel size for canopy analysis
#' @param min_canopy_height Minimum height for canopy analysis
#' @param coverage_method Coverage degree calculation method
#' @return List containing tree metrics, canopy metrics, and file paths
#' @export
Calculate_trees_metrics <- function(woodpoint, a, AGB_def, Forest_floor, plot, filename, 
                                    output_path, canopy_voxel_size = 0.1, 
                                    min_canopy_height = 1.5, 
                                    coverage_method = "mean_normalized") {
  
  tic('metrics')
  
  # Calculate individual tree coordinates
  message("Calculating individual tree coordinates...")
  setDT(woodpoint)
  tree_metrics <- woodpoint[, .SD[which.min(z)], by = cls][, .(X = x, Y = y, cls)]
  
  # Calculate tree heights
  message("Calculating tree heights...")
  setDT(a)
  colnames(a) <- c('x', 'y', 'z')
  a[, join_key := .I]
  
  tree_metrics[, c("min_z", "max_z") := {
    xrng <- c(X - 0.5, X + 0.5)
    yrng <- c(Y - 0.5, Y + 0.5)
    nearby <- a[between(x, xrng[1], xrng[2]) & between(y, yrng[1], yrng[2])]
    .(
      fifelse(nrow(nearby) > 0, min(nearby$z), min(woodpoint[cls == .BY$cls, z])),
      fifelse(nrow(nearby) > 0, max(nearby$z), max(woodpoint[cls == .BY$cls, z]))
    )
  }, by = .(cls, X, Y)]
  
  tree_metrics[, Height := round(max_z - min_z, 2)]
  tree_metrics <- unique(tree_metrics)
  setnames(tree_metrics, c("X", "Y", "cls", "min_z", "max_z", "Height"))
  
  # Calculate DBH
  message("Calculating DBH...")
  tree_metrics <- calculate_dbh(tree_metrics, woodpoint)
  
  plot_metrics <- data.table(
    Tree_n = tree_metrics$cls,
    X_tree = round(tree_metrics$X, 2),
    Y_tree = round(tree_metrics$Y, 2),
    Z_min = round(tree_metrics$min_z, 2),
    Height_m = round(tree_metrics$Height, 2),
    DBH_cm = round(tree_metrics$DBH_cm, 1)
  )
  
  # Filter trees by point count
  message("Filtering trees by point count...")
  plot_metrics <- filter_trees_by_point_count(plot_metrics, woodpoint)
  
  # Calculate crown base height
  message("Calculating crown base height...")
  if (nrow(plot_metrics) > 0) {
    plot_metrics[, Crown_Base_m := calculate_crown_base_heights(
      tree_ids = Tree_n,
      plot_metrics = plot_metrics,
      AGB_def = AGB_def
    )]
  } else {
    warning("No valid trees found for crown base height calculation")
  }
  
  # Save tree report
  fwrite(plot_metrics, file.path(output_path, paste0(plot, "_tree_report.csv")), sep = ";")
  message("Tree metrics report saved to: ", file.path(output_path, paste0(plot, "_tree_report.csv")))
  
  # Calculate plot-level tree statistics
  message("Calculating plot-level statistics...")
  tree_summary <- data.frame(
    metric = c("tree_count", "valid_tree_count"),
    value = c(nrow(plot_metrics), sum(!is.na(plot_metrics$DBH_cm)))
  )
  
  if (sum(!is.na(plot_metrics$DBH_cm)) > 0) {
    valid_dbh <- plot_metrics[!is.na(DBH_cm), DBH_cm]
    dbh_stats <- data.frame(
      metric = c("mean_dbh_cm", "median_dbh_cm"),
      value = c(round(mean(valid_dbh, na.rm = TRUE), 1), 
                round(median(valid_dbh, na.rm = TRUE), 1))
    )
    tree_summary <- rbind(tree_summary, dbh_stats)
  }
  
  if (sum(!is.na(plot_metrics$Crown_Base_m)) > 0) {
    valid_cbh <- plot_metrics[!is.na(Crown_Base_m), Crown_Base_m]
    cbh_stats <- data.frame(
      metric = c("mean_cbh_m", "median_cbh_m"),
      value = c(round(mean(valid_cbh, na.rm = TRUE), 1), 
                round(median(valid_cbh, na.rm = TRUE), 1))
    )
    tree_summary <- rbind(tree_summary, cbh_stats)
  }
  
  # Run canopy analysis
  message("Running canopy analysis...")
  canopy_results <- tryCatch({
    analyze_forest_canopy(
      AGB_def = AGB_def,
      Forest_floor = Forest_floor,
      voxel_size = canopy_voxel_size,
      min_height = min_canopy_height,
      coverage_method = coverage_method,
      area_of_interest = NULL,
      output_path = output_path
    )
  }, error = function(e) {
    message("Canopy analysis error: ", e$message)
    return(NULL)
  })
  
  # Combine results
  combined_report <- tree_summary
  
  if (!is.null(canopy_results) && !is.null(canopy_results$summary_metrics)) {
    # Prepend canopy metrics to combined report
    canopy_metrics <- canopy_results$summary_metrics
    combined_report <- rbind(canopy_metrics, combined_report)
    
    # Calculate stand density metrics if area is available
    if (sum(!is.na(plot_metrics$DBH_cm)) > 0) {
      # Extract area from canopy metrics
      area_row <- which(canopy_metrics$metric == "area_of_interest_m2")
      
      if (length(area_row) > 0) {
        area_m2 <- as.numeric(canopy_metrics$value[area_row[1]])
        
        if (!is.na(area_m2) && area_m2 > 0) {
          valid_dbh <- plot_metrics[!is.na(DBH_cm), DBH_cm]
          trees_per_ha <- length(valid_dbh) / (area_m2 / 10000)
          basal_area <- sum(pi * (valid_dbh/200)^2, na.rm = TRUE)  # m²
          basal_area_ha <- basal_area / (area_m2 / 10000)          # m²/ha
          
          stand_metrics <- data.frame(
            metric = c("trees_per_hectare", "basal_area_m2_ha"),
            value = c(round(trees_per_ha, 1), round(basal_area_ha, 2))
          )
          combined_report <- rbind(combined_report, stand_metrics)
        }
      }
    }
  } else {
    message("Canopy analysis unavailable; report includes only tree metrics")
  }
  
  # Save combined report
  report_file <- file.path(output_path, paste0(plot, "_plot_report.csv"))
  write.csv(combined_report, report_file, row.names = FALSE)
  message("Plot report saved to: ", report_file)
  
  toc()
  
  return(list(
    tree_metrics = plot_metrics,
    canopy_metrics = combined_report,
    tree_report_file = file.path(output_path, paste0(plot, "_tree_report.csv")),
    plot_report_file = report_file
  ))
}

################################
# INTERNAL HELPER FUNCTIONS (NOT EXPORTED)
################################

#' @title Analyze forest canopy (internal)
#' @keywords internal
analyze_forest_canopy <- function(AGB_def, Forest_floor, voxel_size = 0.1, 
                                  min_height = 1.5, coverage_method = "mean_normalized", 
                                  area_of_interest = NULL, output_path = tempdir()) {
  
  # Verify required columns
  required_cols <- c("x", "y", "z")
  for (df_name in c("AGB_def", "Forest_floor")) {
    df <- get(df_name)
    missing_cols <- required_cols[!required_cols %in% names(df)]
    if (length(missing_cols) > 0) {
      stop("Missing columns in ", df_name, ": ", paste(missing_cols, collapse = ", "))
    }
  }
  
  # Standardize column names
  colnames(AGB_def) <- c("x", "y", "z")
  colnames(Forest_floor) <- c("x", "y", "z")
  
  # Create output directory if needed
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  start_time <- Sys.time()
  
  # Voxelize AGB data
  AAvox_agb <- data.frame(
    x = AGB_def$x, 
    y = AGB_def$y, 
    z = AGB_def$z, 
    u = as.integer(AGB_def$x / voxel_size) + 1, 
    v = as.integer(AGB_def$y / voxel_size) + 1, 
    w = as.integer(AGB_def$z / voxel_size) + 1
  )
  
  AGB_vox <- data.frame(AAvox_agb %>% fcount(u, v, w))
  
  # Voxelize Forest floor data
  AAvox_floor <- data.frame(
    x = Forest_floor$x, 
    y = Forest_floor$y, 
    z = Forest_floor$z, 
    u = as.integer(Forest_floor$x / voxel_size) + 1, 
    v = as.integer(Forest_floor$y / voxel_size) + 1, 
    w = as.integer(Forest_floor$z / voxel_size) + 1
  )
  
  setDT(AAvox_floor)
  floor_bins <- AAvox_floor[, .(floor_w = min(w)), by = .(u, v)]
  
  # Create complete grid
  u_range <- range(floor_bins$u)
  v_range <- range(floor_bins$v)
  full_grid <- expand.grid(
    u = u_range[1]:u_range[2],
    v = v_range[1]:v_range[2]
  )
  
  # Merge with existing data
  floor_elevation <- merge(full_grid, floor_bins, by = c("u", "v"), all.x = TRUE)
  
  # Fill missing values with nearest neighbor
  if(any(is.na(floor_elevation$floor_w))) {
    valid_cells <- floor_elevation[!is.na(floor_elevation$floor_w), ]
    missing_cells <- floor_elevation[is.na(floor_elevation$floor_w), ]
    
    for(i in 1:nrow(missing_cells)) {
      u_val <- missing_cells$u[i]
      v_val <- missing_cells$v[i]
      
      valid_cells$dist <- sqrt((valid_cells$u - u_val)^2 + (valid_cells$v - v_val)^2)
      closest <- valid_cells[which.min(valid_cells$dist), ]
      
      idx <- which(floor_elevation$u == u_val & floor_elevation$v == v_val)
      floor_elevation$floor_w[idx] <- closest$floor_w
    }
  }
  
  # Merge terrain elevations with AGB voxels
  voxel_data <- merge(AGB_vox, floor_elevation, by = c("u", "v"), all.x = TRUE)
  
  # Interpolate remaining missing values
  if (any(is.na(voxel_data$floor_w))) {
    cat("Interpolating missing terrain elevations...\n")
    
    missing_idx <- which(is.na(voxel_data$floor_w))
    
    for (idx in missing_idx) {
      u_val <- voxel_data$u[idx]
      v_val <- voxel_data$v[idx]
      
      found_neighbors <- FALSE
      radius <- 1
      max_radius <- 10
      
      while (!found_neighbors && radius <= max_radius) {
        neighbors <- floor_elevation[
          floor_elevation$u >= (u_val - radius) & 
            floor_elevation$u <= (u_val + radius) & 
            floor_elevation$v >= (v_val - radius) & 
            floor_elevation$v <= (v_val + radius), 
        ]
        
        if (nrow(neighbors) > 0) {
          neighbors$dist <- sqrt((neighbors$u - u_val)^2 + (neighbors$v - v_val)^2)
          neighbors$weight <- 1 / (neighbors$dist + 0.1)
          
          voxel_data$floor_w[idx] <- sum(neighbors$floor_w * neighbors$weight) / sum(neighbors$weight)
          found_neighbors <- TRUE
        } else {
          radius <- radius + 1
        }
      }
      
      if (!found_neighbors) {
        voxel_data$floor_w[idx] <- min(floor_elevation$floor_w, na.rm = TRUE)
      }
    }
    cat("Interpolation completed.\n")
  }
  
  # Calculate normalized height
  voxel_data$w_normalized <- voxel_data$w - voxel_data$floor_w
  
  # Calculate coverage degree
  voxel_data <- calculate_coverage_degree(voxel_data, method = coverage_method)
  
  # Filter by minimum height
  min_height_voxel <- min_height / voxel_size
  if (min_height > 0) {
    voxel_data <- voxel_data[voxel_data$w_normalized >= min_height_voxel, ]
  }
  
  # Calculate 5th percentile threshold
  q5 <- quantile(voxel_data$coverage_degree, 0.05, na.rm = TRUE)
  voxel_data <- voxel_data[voxel_data$coverage_degree >= q5, ]
  
  # Check if there are voxels after filtering
  if (nrow(voxel_data) == 0) {
    message("No voxels exceed thresholds (height >= ", min_height, 
            " m, coverage_degree >= ", q5, ")")
    return(NULL)
  }
  
  # Calculate canopy volume
  base_voxel_volume <- voxel_size^3
  canopy_volume <- nrow(voxel_data) * base_voxel_volume
  
  # Calculate ground projection
  xy_positions <- unique(voxel_data[, c("u", "v")])
  
  # Convert to meters for convex hull calculation
  xy_m <- data.frame(
    x = (xy_positions$u - 1) * voxel_size,
    y = (xy_positions$v - 1) * voxel_size
  )
  
  # Calculate area using convex hull
  pts <- sf::st_multipoint(as.matrix(xy_m))
  convex_hull <- sf::st_convex_hull(pts)
  area_of_interest <- as.numeric(sf::st_area(convex_hull))
  
  # Calculate coverage metrics
  effective_coverage <- nrow(xy_positions)
  coverage_area_m2 <- effective_coverage * (voxel_size^2)
  coverage_percentage <- (coverage_area_m2 / area_of_interest) * 100
  
  # Height statistics
  z_stats <- list(
    min_height = min(voxel_data$w_normalized) * voxel_size,
    max_height = max(voxel_data$w_normalized) * voxel_size,
    mean_height = weighted.mean(voxel_data$w_normalized, voxel_data$coverage_degree) * voxel_size,
    median_height = median(voxel_data$w_normalized) * voxel_size,
    sd_height = sd(voxel_data$w_normalized) * voxel_size
  )
  
  # Calculate vertical profile
  vertical_profile <- calculate_vertical_profile(voxel_data, "w_normalized", voxel_size)
  
  # Execution time
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "secs")
  
  # Summary metrics
  summary_metrics <- data.frame(
    metric = c(
      "min_height_threshold_m",
      "coverage_method",
      "area_of_interest_m2",
      "coverage_threshold_q5",
      "coverage_area_m2",
      "coverage_percentage",
      "canopy_volume_m3",
      "min_height_m",
      "max_height_m",
      "mean_height_m",
      "median_height_m",
      "sd_height_m",
      "execution_time_sec"
    ),
    value = c(
      min_height,
      coverage_method,
      round(area_of_interest, 1),
      round(q5, 3),
      round(coverage_area_m2, 1),
      round(coverage_percentage, 2),
      round(canopy_volume, 1),
      round(z_stats$min_height, 2),
      round(z_stats$max_height, 2),
      round(z_stats$mean_height, 1),
      round(z_stats$median_height, 2),
      round(z_stats$sd_height, 1),
      round(as.numeric(execution_time), 2)
    )
  )
  
  # Save results
  #write.csv(summary_metrics, file.path(output_path, "canopy_metrics_summary.csv"), row.names = FALSE)
  #cat("Canopy results saved in", output_path, "\n")
  
  # Return structured list
  return(list(
    summary_metrics = summary_metrics,
    voxel_data = voxel_data,
    vertical_profile = vertical_profile,
    ground_projection = xy_m,
    execution_time = execution_time
  ))
}

#' @title Calculate coverage degree (internal)
#' @keywords internal
calculate_coverage_degree <- function(voxel_data, method = "mean_normalized") {
  
  voxel_data$N <- as.numeric(voxel_data$N)
  
  xy_coords <- paste(voxel_data$u, voxel_data$v)
  max_by_xy <- tapply(voxel_data$N, xy_coords, max)
  voxel_data$max_col <- max_by_xy[match(xy_coords, names(max_by_xy))]
  
  mean_N <- mean(voxel_data$N)
  
  if (method == "linear") {
    voxel_data$coverage_degree <- voxel_data$N / voxel_data$max_col
  } else if (method == "mean_normalized") {
    voxel_data$coverage_degree <- voxel_data$N / mean_N
  } else if (method == "exponential") {
    voxel_data$coverage_degree <- 1 - exp(-voxel_data$N / mean_N)
  } else if (method == "threshold") {
    threshold <- quantile(voxel_data$N, 0.5)
    voxel_data$coverage_degree <- ifelse(voxel_data$N >= threshold, 1, 0.1)
  } else if (method == "mediterranean") {
    voxel_data$coverage_degree <- (voxel_data$N / voxel_data$max_col)^0.7
  } else {
    stop("Unknown coverage calculation method: ", method)
  }
  
  voxel_data$max_col <- NULL
  voxel_data$coverage_degree <- pmin(pmax(voxel_data$coverage_degree, 0), 1)
  
  return(voxel_data)
}

#' @title Calculate vertical profile (internal)
#' @keywords internal
calculate_vertical_profile <- function(voxel_data, w_column, voxel_size, height_interval = 1) {
  
  w_min <- floor(min(voxel_data[[w_column]]))
  w_max <- ceiling(max(voxel_data[[w_column]]))
  
  if ((w_max - w_min) / height_interval > 100) {
    height_interval <- ceiling((w_max - w_min) / 50)
    cat("Warning: Height interval increased to", height_interval, "units\n")
  }
  
  height_breaks <- seq(w_min, w_max, by = height_interval)
  
  voxel_data$height_bin <- cut(voxel_data[[w_column]], breaks = height_breaks, 
                               include.lowest = TRUE, right = FALSE)
  
  profile <- aggregate(cbind(coverage_degree, N) ~ height_bin, data = voxel_data, 
                       FUN = function(x) c(sum = sum(x), mean = mean(x), count = length(x)))
  
  result <- data.frame(height_bin = as.character(profile$height_bin))
  result$lower_bound <- NA
  result$upper_bound <- NA
  
  for (i in 1:nrow(result)) {
    bin_range <- as.character(result$height_bin[i])
    bin_range <- gsub("\\[|\\)|\\]", "", bin_range)
    range_vals <- as.numeric(strsplit(bin_range, ",")[[1]])
    result$lower_bound[i] <- range_vals[1]
    result$upper_bound[i] <- range_vals[2]
  }
  
  result$count_voxels <- profile$N[, "count"]
  result$mean_points <- profile$N[, "mean"]
  result$sum_points <- profile$N[, "sum"]
  result$mean_coverage <- profile$coverage_degree[, "mean"]
  result$sum_coverage <- profile$coverage_degree[, "sum"]
  
  result$lower_bound_m <- result$lower_bound * voxel_size
  result$upper_bound_m <- result$upper_bound * voxel_size
  
  return(result)
}

#' @title Calculate DBH (internal)
#' @keywords internal
calculate_dbh <- function(tree_metrics, woodpoint) {
  
  dbh_height <- 1.3
  dbh_tol <- 0.05
  min_radius <- 0.025
  max_radius <- 0.4
  
  compute_dbh <- function(cls_id, wood_data, tree_data) {
    cluster_pts <- wood_data[cls == cls_id]
    base_z <- tree_data[cls == cls_id, min_z]
    target_z <- base_z + dbh_height
    pts <- cluster_pts[z >= (target_z - dbh_tol) & z <= (target_z + dbh_tol), .(x, y)]
    
    if(nrow(pts) < 5) return(NA_real_)
    
    fit <- tryCatch({
      params <- conicfit::CircleFitByPratt(as.matrix(pts))
      if(params[3] < min_radius || params[3] > max_radius) return(NA_real_)
      round(params[3] * 2, 3)
    }, error = function(e) NA_real_)
  }
  
  tree_metrics[, DBH := compute_dbh(
    cls_id = .BY$cls,
    wood_data = woodpoint,
    tree_data = tree_metrics
  ), by = cls]
  
  tree_metrics[, ":="(
    DBH_cm = fifelse(DBH >= 0.05 & DBH <= 2, round(DBH * 100, 1), NA_real_),
    DBH_valido = fifelse(DBH >= 0.05 & DBH <= 2, TRUE, FALSE)
  )]
  
  return(tree_metrics)
}

#' @title Filter trees by point count (internal)
#' @keywords internal
filter_trees_by_point_count <- function(plot_metrics, woodpoint) {
  
  point_count <- woodpoint[, .N, by = cls]
  setnames(point_count, c("Tree_n", "n_points"))
  setDT(plot_metrics)
  setDT(point_count)
  
  median_points_valid_trees <- point_count[
    Tree_n %in% plot_metrics[!is.na(DBH_cm), Tree_n], median(n_points)
  ]
  
  min_points_threshold <- median_points_valid_trees * 0.7
  
  filtered_metrics <- plot_metrics[
    Tree_n %in% point_count[n_points >= min_points_threshold, Tree_n] | !is.na(DBH_cm)
  ]
  
  return(filtered_metrics)
}

#' @title Calculate crown base heights (internal)
#' @keywords internal
calculate_crown_base_heights <- function(tree_ids, plot_metrics, AGB_def) {
  
  calculate_cbh <- function(tree_id) {
    tryCatch({
      tree <- plot_metrics[Tree_n == tree_id]
      crown_buffer <- 1.5
      
      agb_buffer <- AGB_def[
        x >= (tree$X_tree - crown_buffer) & 
          x <= (tree$X_tree + crown_buffer) & 
          y >= (tree$Y_tree - crown_buffer) & 
          y <= (tree$Y_tree + crown_buffer)
      ]
      
      if(nrow(agb_buffer) < 10) return(NA_real_)
      
      z_normalized <- agb_buffer$z - tree$Z_min
      z_sorted <- sort(z_normalized)
      tree_height <- max(z_normalized)
      lower_points <- z_sorted[z_sorted <= (tree_height/2)]
      
      if(length(lower_points) < 10) return(NA_real_)
      
      gaps_lower <- diff(lower_points)
      max_gap_idx <- which.max(gaps_lower)
      cbh <- lower_points[max_gap_idx + 1]
      
      if(cbh < 0.5 || cbh > (tree_height * 0.9)) return(NA_real_)
      
      return(round(cbh, 2))
    }, error = function(e) NA_real_)
  }
  
  cbh_values <- vapply(tree_ids, calculate_cbh, FUN.VALUE = numeric(1))
  return(cbh_values)
}

#' @title Extract forest floor (internal)
#' @keywords internal
extract_forest_floor <- function(a, soil_dim, th = 20, N = 500, output_path, plot) {
  
  colnames(a) <- c('x', 'y', 'z')
  
  AAvox <- data.frame(
    a$x, a$y, a$z, 
    as.integer(a$x / soil_dim) + 1, 
    as.integer(a$y / soil_dim) + 1, 
    as.integer(a$z / soil_dim) + 1
  )
  colnames(AAvox) <- c('x', 'y', 'z', 'u', 'v', 'w')
  
  AAvox1 <- data.frame(AAvox %>% fcount(u, v, w))
  AAvoxels <- AAvox1[AAvox1["N"] >= th, ]
  
  voxel_minimi <- aggregate(w ~ u + v, AAvoxels, min)
  colnames(voxel_minimi) <- c("u", "v", "wmin")
  
  p0 <- suppressMessages(inner_join(AAvoxels, voxel_minimi))
  p1 <- data.frame(p0['u'], p0['v'], p0['w'], p0['w'] - p0['wmin'])
  colnames(p1) <- c('u', 'v', 'w', 'w0')
  
  Forest_floor0 <- p1[p1['w0'] <= 1, ]
  Forest_floor00 <- data.frame(Forest_floor0$u, Forest_floor0$v, Forest_floor0$w)
  
  b <- dbscan(Forest_floor00, eps = 1, minPts = 3)
  y1 <- cbind(Forest_floor00, b$cluster)
  colnames(y1) <- c('u', 'v', 'w', 'cls')
  
  nc <- data.frame(y1 %>% fcount(cls))
  good_cluster <- nc[nc['N'] > N & nc['cls'] != 0, ]
  y2 <- suppressMessages(inner_join(y1, good_cluster))
  colnames(y2) <- c('u', 'v', 'w', 'cls', 'N')
  
  Forest_floor1 <- suppressMessages(inner_join(AAvox, y2))
  Forest_floor <- data.frame(Forest_floor1$x, Forest_floor1$y, Forest_floor1$z)
  colnames(Forest_floor) <- c('x', 'y', 'z')
  
  AGB0 <- p1[p1['w0'] > 1, ]
  ant <- suppressMessages(anti_join(Forest_floor0, y2))
  AGB1 <- rbind(AGB0, ant)
  AGB2 <- suppressMessages(inner_join(AAvox, AGB1))
  AGB <- data.frame(AGB2$x, AGB2$y, AGB2$z)
  colnames(AGB) <- c('x', 'y', 'z')
  
  fwrite(Forest_floor, file.path(output_path, paste0(plot, "_Forest_floor_soil_dim_", soil_dim, ".txt")))
  message("Forest floor saved to: ", file.path(output_path, paste0(plot, "_Forest_floor_soil_dim_", soil_dim, ".txt")))
  
  return(list(
    floor_points = Forest_floor,
    agb_points = AGB
  ))
}

#' @title Segment wood (internal)
#' @keywords internal
segment_wood <- function(AGB, dim, th, eps, mpts, h_tree, N, R, 
                         Vox_print, WoodVox_print, plot, output_path) {
  
  setDT(AGB)
  colnames(AGB) <- c('x', 'y', 'z')
  
  AAvox <- data.table(
    x = AGB$x, y = AGB$y, z = AGB$z,
    u = as.integer(AGB$x / dim) + 1,
    v = as.integer(AGB$y / dim) + 1,
    w = as.integer(AGB$z / dim) + 1
  )
  
  AAvox1 <- AAvox[, .N, by = .(u, v, w)]
  AAvoxels <- AAvox1[N >= th]
  
  if (Vox_print) {
    fwrite(AAvoxels, file.path(output_path, paste0(plot, "_vox.txt")))
    message("Voxelized point cloud saved")
  }
  
  agbw <- as.matrix(AAvoxels[, .(u, v, w)])
  b <- dbscan(agbw, eps = eps, minPts = mpts)
  y <- data.table(u = agbw[,1], v = agbw[,2], w = agbw[,3], cls = b$cluster)
  
  freq_cls <- y[, .N, by = cls]
  setnames(freq_cls, "N", "num")
  good_cluster <- freq_cls[num > N]
  
  cluster <- data.table()
  
  for (CLS in good_cluster$cls) {
    if (CLS == 0) next
    
    pop_cls <- freq_cls[cls == CLS, num]
    valCls <- y[cls == CLS]
    d <- as.matrix(valCls[, .(u, v, w)])
    
    if ((max(d[,3]) - min(d[,3])) * dim < h_tree) next
    
    h <- prcomp(d)
    sdev <- h$sdev
    p <- sdev[1]
    s <- summary(h)
    w <- s$importance
    q <- w[2, 1]
    r <- p * q
    
    if (r < R) next
    
    cluster0 <- data.table(
      u = valCls$u, 
      v = valCls$v, 
      w = valCls$w, 
      cls = valCls$cls, 
      r = r, 
      pop_cls = pop_cls
    )
    cluster <- rbind(cluster, cluster0)
  }
  
  if(nrow(cluster) == 0) {
    stop('No wood clusters found. Adjust eps and mpts parameters.')
  }
  
  if(WoodVox_print) {
    fwrite(cluster, file.path(output_path, paste0(plot, '_WoodVox_eps', eps, '_mpts', mpts, '.txt')))
    message("Wood voxels saved")
  }
  
  woodpoint0 <- merge(AAvox, cluster, by = c("u", "v", "w"))
  woodpoint <- woodpoint0[, .(x, y, z, cls)]
  
  fwrite(woodpoint, file.path(output_path, paste0(plot, "_Wood_eps", eps, "_mpts", mpts, ".txt")))
  message("Wood points saved to: ", file.path(output_path, paste0(plot, "_Wood_eps", eps, "_mpts", mpts, ".txt")))
  
  return(list(
    wood_points = woodpoint,
    wood_voxels = cluster
  ))
}

#' @title Separate foliage (internal)
#' @keywords internal
separate_foliage <- function(AGB, woodpoint, plot, eps, mpts, output_path) {
  
  voxel_size <- 0.2
  setDT(woodpoint)
  
  AAvoxL <- woodpoint[, .(
    u = as.integer(x / voxel_size) + 1,
    v = as.integer(y / voxel_size) + 1, 
    w = as.integer(z / voxel_size) + 1
  )]
  
  AAvox2L <- AAvoxL[, .N, by = .(u, v, w)]
  
  setDT(AGB)
  colnames(AGB) <- c('x', 'y', 'z')
  
  AAvoxA <- AGB[, .(
    x, y, z,
    u = as.integer(x / voxel_size) + 1,
    v = as.integer(y / voxel_size) + 1,
    w = as.integer(z / voxel_size) + 1
  )]
  
  AAvox2A <- AAvoxA[, .N, by = .(u, v, w)]
  
  setkey(AAvox2L, u, v, w)
  setkey(AAvox2A, u, v, w)
  AAvoxD <- AAvox2A[!AAvox2L]
  
  setkey(AAvoxD, u, v, w)
  setkey(AAvoxA, u, v, w)
  AGB_def0 <- AAvoxD[AAvoxA, nomatch = 0]
  AGB_def <- AGB_def0[, .(x, y, z)]
  
  fwrite(AGB_def, file.path(output_path, paste0(plot, "_AGBnoWOOD_eps", eps, "_mpts", mpts, ".txt")))
  message("Foliage points saved to: ", file.path(output_path, paste0(plot, "_AGBnoWOOD_eps", eps, "_mpts", mpts, ".txt")))
  
  return(AGB_def)
}

