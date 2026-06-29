# ==============================================================================
# PIC_ANALYZE_CLOUD - Point Cloud Diagnostic Analysis
# ==============================================================================
# 
# Purpose: Pre-processing diagnostic tool for point cloud quality assessment
# Author: Roberto Ferrara, CNR-IBE
# 
# Architecture:
# - Float-based coordinates (no integer conversion)
# - Generates plots ALWAYS (for Shiny visualization)
# - PDF generation is OPTIONAL (controlled by generate_pdf parameter)
# - Loaded cloud remains in memory for subsequent Forest_seg analysis
# 
# Dependencies: ggplot2, terra, gridExtra, grid, grDevices, data.table
# ==============================================================================

#' @title Point cloud diagnostic analysis
#' @name pic_analyze_cloud
#' @description 
#' Analyzes point cloud quality and structure with voxel density analysis.
#' Generates density maps and distribution curves.
#' Results are always returned as plots (for Shiny) and optionally exported as PDF.
#' 
#' @param points Data frame with columns x, y, z (or will be coerced)
#' @param voxel_sizes Numeric vector of voxel sizes in meters for analysis
#' @param generate_pdf Logical, whether to generate PDF report (default = TRUE)
#' @param pdf_output Character, PDF filename (if NULL and generate_pdf=TRUE, auto-generated)
#' @param output_path Character, directory for PDF output (default = tempdir())
#' 
#' @return List containing:
#'   - summary: Basic statistics (n_points, extent, area)
#'   - density_analysis: Density per m^2 statistics and raster
#'   - voxel_analysis: Multi-resolution voxel statistics
#'   - plots: Named list of ggplot2 objects (density_raster, density_curve)
#'   - text_output: Character vector with formatted statistics
#'   - pdf_file: Path to PDF if generated, NULL otherwise
#' 
#' @export
pic_analyze_cloud <- function(points,
                              voxel_sizes = 0.1,
                              generate_pdf = TRUE,
                              pdf_output = NULL,
                              output_path = tempdir()) {
  
  # Check required packages (Suggests)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' required")
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' required")
  if (!requireNamespace("gridExtra", quietly = TRUE)) stop("Package 'gridExtra' required")
  
  # ===== COERCE INPUT =====
  # Unified coercion: handles file paths (.xyz, .txt, .csv, .las, .laz),
  # data.frames, matrices, data.tables -> standardized data.table with x, y, z
  points <- .coerce_to_xyz_dt(points)
  # ===== END COERCE =====
  
  cat("\n========================================\n")
  cat("POINT CLOUD ANALYSIS - PIC\n")
  cat("========================================\n\n")
  
  # Capture text output for PDF and return
  text_lines <- c()
  add_line <- function(...) {
    line <- paste0(...)
    text_lines <<- c(text_lines, line)
    cat(line)
  }
  
  # ===========================================================================
  # SECTION 1: GENERAL STATISTICS
  # ===========================================================================
  
  add_line("1. GENERAL STATISTICS\n")
  add_line("----------------------------------------\n")
  n_points <- nrow(points)
  add_line(sprintf("Total number of points: %s\n", format(n_points, big.mark = ",", decimal.mark = ".")))
  
  # Spatial extent
  extent <- points |>
    summarise(
      x_min = min(x), x_max = max(x),
      y_min = min(y), y_max = max(y),
      z_min = min(z), z_max = max(z)
    )
  
  add_line(sprintf("X extent: %.2f - %.2f m (range: %.2f m)\n", 
              extent$x_min, extent$x_max, extent$x_max - extent$x_min))
  add_line(sprintf("Y extent: %.2f - %.2f m (range: %.2f m)\n", 
              extent$y_min, extent$y_max, extent$y_max - extent$y_min))
  add_line(sprintf("Z extent: %.2f - %.2f m (range: %.2f m)\n", 
              extent$z_min, extent$z_max, extent$z_max - extent$z_min))
  
  area_xy <- (extent$x_max - extent$x_min) * (extent$y_max - extent$y_min)
  add_line(sprintf("Planimetric area: %.2f m\u00b2\n", area_xy))
  
  # ===========================================================================
  # SECTION 2: POINT DISTRIBUTION (1m grid)
  # ===========================================================================
  
  add_line("\n2. POINT DISTRIBUTION AND DENSITY\n")
  add_line("----------------------------------------\n")
  
  # ===========================================================================
  # OPTIMIZED: Density calculation with data.table and terra
  # IMPROVEMENT: 3-5x faster, especially for large datasets
  # ===========================================================================

  calculate_density_and_raster <- function(xyz, resolution = 1) {
    # Ensure data.table format
    if (!data.table::is.data.table(xyz)) {
      xyz <- data.table::as.data.table(xyz)
    }

    # Keep only first 3 columns (x, y, z) and rename
    # This handles files with extra columns (RGB, intensity, etc.)
    xyz <- xyz[, 1:3, with = FALSE]
    data.table::setnames(xyz, 1:3, c("x", "y", "z"))

    n_points <- nrow(xyz)

    # OPTIMIZED: Vectorized grid calculation
    x_min <- floor(min(xyz$x))
    x_max <- ceiling(max(xyz$x))
    y_min <- floor(min(xyz$y))
    y_max <- ceiling(max(xyz$y))

    # Compute grid dimensions first (needed for clamping)
    n_col <- as.integer((x_max - x_min) / resolution)
    n_row <- as.integer((y_max - y_min) / resolution)
    n_col <- max(n_col, 1L)
    n_row <- max(n_row, 1L)

    # Calculate cell indices with clamping to valid range
    xyz[, ':='(
      x_cell = pmin(as.integer(floor((x - x_min) / resolution)) + 1L, n_col),
      y_cell = pmin(as.integer(floor((y - y_min) / resolution)) + 1L, n_row)
    )]

    # OPTIMIZED: Count points per cell with data.table
    cells_dt <- xyz[, .N, by = .(x_cell, y_cell)]
    data.table::setnames(cells_dt, "N", "density")

    n_cells <- nrow(cells_dt)

    # Statistics (vectorized)
    density_median <- median(cells_dt$density)
    density_mean <- mean(cells_dt$density)

    add_line("=== POINT CLOUD DENSITY (GRID ", resolution, " x ", resolution, " m) ===\n")
    add_line("Total points: ", format(n_points, big.mark = ",", decimal.mark = "."), "\n")
    add_line("Occupied cells: ", format(n_cells, big.mark = ",", decimal.mark = "."), "\n")
    add_line("Median density: ", round(density_median, 2), " points/m\u00b2\n")
    add_line("Mean density: ", round(density_mean, 2), " points/m\u00b2\n\n")

    # Create sparse matrix (only occupied cells)
    mat <- matrix(NA_real_, nrow = n_row, ncol = n_col)

    # Vectorized matrix filling (avoid loop)
    cells_dt[, mat_idx := (n_row - y_cell + 1L) + (x_cell - 1L) * n_row]
    mat[cells_dt$mat_idx] <- cells_dt$density

    # Create terra raster
    r <- terra::rast(mat)
    terra::ext(r) <- c(x_min, x_max, y_min, y_max)
    names(r) <- "density_points_m2"

    # Clean up temporary columns
    xyz[, c("x_cell", "y_cell") := NULL]

    return(list(
      density_median = as.numeric(density_median),
      density_mean = density_mean,
      raster = r,
      points_per_cell = cells_dt$density
    ))
  }
  
  # Calculate density at 1m resolution
  density_result <- calculate_density_and_raster(points, resolution = 1)
  
  # ===========================================================================
  # SECTION 3: VOXEL DENSITY ANALYSIS
  # ===========================================================================
  
  add_line("\n3. VOXEL DENSITY ANALYSIS\n")
  add_line("----------------------------------------\n")
  
  # ===========================================================================
  # OPTIMIZED: Voxel analysis with data.table
  # ===========================================================================

  voxel_stats_list <- list()

  # Convert to data.table for performance
  data.table::setDT(points)

  for (vsize in voxel_sizes) {
    add_line(sprintf("\n>>> Voxel size: %.2f m\n", vsize))

    # OPTIMIZED: Voxelize using data.table (vectorized operations)
    points_vox <- data.table::copy(points)
    points_vox[, ':='(
      ix = as.integer(floor(x / vsize)),
      iy = as.integer(floor(y / vsize)),
      iz = as.integer(floor(z / vsize))
    )]

    # OPTIMIZED: Count density per voxel (fast data.table aggregation)
    voxels <- points_vox[, .N, by = .(ix, iy, iz)]
    data.table::setnames(voxels, "N", "density")

    n_voxels <- nrow(voxels)

    # OPTIMIZED: Vectorized quantile calculation (single pass)
    quantiles <- quantile(voxels$density, probs = c(0.25, 0.5, 0.75, 0.9))

    dens_stats <- list(
      n_voxels = n_voxels,
      dens_mean = mean(voxels$density),
      dens_median = median(voxels$density),
      dens_sd = sd(voxels$density),
      dens_min = min(voxels$density),
      dens_max = max(voxels$density),
      dens_q25 = quantiles[1],
      dens_q50 = quantiles[2],
      dens_q75 = quantiles[3],
      dens_q90 = quantiles[4]
    )

    # Density per cubic meter
    voxel_volume <- vsize^3
    dens_mean_m3 <- dens_stats$dens_mean / voxel_volume
    dens_median_m3 <- dens_stats$dens_median / voxel_volume

    add_line(sprintf("  N. voxels: %s\n", format(n_voxels, big.mark = ",", decimal.mark = ".")))
    add_line(sprintf("  Mean density: %.1f points/voxel (%.0f points/m\u00b3)\n", dens_stats$dens_mean, dens_mean_m3))
    add_line(sprintf("  Median density: %.0f points/voxel (%.0f points/m\u00b3)\n", dens_stats$dens_median, dens_median_m3))
    add_line(sprintf("  Min-max density: %d - %d points/voxel\n",
                dens_stats$dens_min, dens_stats$dens_max))
    add_line(sprintf("  Quantiles (Q25/Q50/Q75/Q90): %.0f / %.0f / %.0f / %.0f\n",
                dens_stats$dens_q25, dens_stats$dens_q50,
                dens_stats$dens_q75, dens_stats$dens_q90))

    # Save for comparison
    voxel_stats_list[[as.character(vsize)]] <- list(
      voxel_size = vsize,
      stats = dens_stats,
      voxels = voxels
    )
  }
  
  # ===========================================================================
  # SECTION 4: GENERATE PLOTS (ALWAYS - for Shiny visualization)
  # ===========================================================================
  
  cat("\n\n4. GENERATING PLOTS...\n")
  cat("----------------------------------------\n")
  
  plots <- list()
  
  # Plot 1: Density raster (convert to ggplot for consistency)
  if(!is.null(density_result$raster)) {
    raster_df <- as.data.frame(density_result$raster, xy = TRUE)
    colnames(raster_df) <- c("x", "y", "density")
    
    p1 <- ggplot2::ggplot(raster_df, ggplot2::aes(x = x, y = y, fill = density)) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_gradientn(colors = grDevices::hcl.colors(100, "YlOrRd", rev = TRUE),
                           na.value = "white",
                           trans = "log10") +
      ggplot2::labs(title = "Point Density per m\u00b2",
           x = "X (m)", y = "Y (m)",
           fill = "Density\n(points/m\u00b2)") +
      ggplot2::coord_equal() +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right")
    
    plots[["density_raster"]] <- p1
  }
  
  # Plot 2: Voxel density histogram (0.1m resolution)
  if ("0.1" %in% names(voxel_stats_list)) {
    v01 <- voxel_stats_list[["0.1"]]

    # Define logarithmic breaks for x axis
    log_breaks <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)

    # Histogram bins evenly spaced in log10 space
    log_range <- range(log10(v01$voxels$density[v01$voxels$density > 0]))
    n_bins <- 40
    log_bin_edges <- seq(log_range[1], log_range[2], length.out = n_bins + 1)

    p2 <- ggplot2::ggplot(v01$voxels, ggplot2::aes(x = density)) +
      ggplot2::geom_histogram(breaks = 10^log_bin_edges,
                     fill = "grey70", color = "black", alpha = 0.7) +
      ggplot2::scale_x_log10(breaks = log_breaks, labels = log_breaks) +
      ggplot2::geom_vline(xintercept = v01$stats$dens_median,
                 color = "red", linetype = "dashed", linewidth = 1) +
      ggplot2::geom_vline(xintercept = v01$stats$dens_mean,
                 color = "blue", linetype = "dashed", linewidth = 1) +
      ggplot2::labs(title = "Voxel Density Distribution (0.1 m)",
           subtitle = sprintf("Median: %.0f | Mean: %.1f points/voxel",
                              v01$stats$dens_median, v01$stats$dens_mean),
           x = "Points per voxel (log scale)",
           y = "Number of voxels") +
      ggplot2::theme_minimal()

    plots[["density_curve"]] <- p2
  }
  
  cat("Plots generated!\n")
  
  # ===========================================================================
  # SECTION 5: CREATE PDF REPORT (OPTIONAL)
  # ===========================================================================
  
  pdf_file_path <- NULL
  
  if (generate_pdf) {
    cat("\n5. CREATING PDF REPORT...\n")
    cat("----------------------------------------\n")
    
    # Generate filename if not provided
    if (is.null(pdf_output)) {
      pdf_output <- file.path(output_path, 
                              paste0("cloud_diagnostics_", 
                                     format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                     ".pdf"))
    } else {
      # Ensure .pdf extension
      if (!grepl("\\.pdf$", pdf_output, ignore.case = TRUE)) {
        pdf_output <- paste0(pdf_output, ".pdf")
      }
      # Add output_path if not absolute
      if (!grepl("^(/|[A-Z]:)", pdf_output)) {
        pdf_output <- file.path(output_path, pdf_output)
      }
    }
    
    grDevices::pdf(pdf_output, width = 8.5, height = 11)  # Portrait orientation (Letter size)

    # Page 1: Summary statistics (text) - Better layout
    grid::grid.newpage()

    # Title at top
    grid::grid.text("PIC POINT CLOUD ANALYSIS",
              x = 0.5, y = 0.96,
              gp = grid::gpar(fontsize = 18, fontface = "bold"),
              just = "top")

    grid::grid.text("Diagnostic Report",
              x = 0.5, y = 0.92,
              gp = grid::gpar(fontsize = 14, fontface = "bold"),
              just = "top")
    
    # Text content with proper margins and line wrapping
    # Split text into lines and ensure proper wrapping at 95 characters
    text_content <- paste(text_lines, collapse = "")
    
    # Manual line wrapping for long lines
    wrapped_lines <- character()
    for(line in strsplit(text_content, "\n")[[1]]) {
      if(nchar(line) > 95) {
        # Split long lines
        while(nchar(line) > 95) {
          wrapped_lines <- c(wrapped_lines, substr(line, 1, 95))
          line <- substr(line, 96, nchar(line))
        }
        if(nchar(line) > 0) wrapped_lines <- c(wrapped_lines, line)
      } else {
        wrapped_lines <- c(wrapped_lines, line)
      }
    }
    text_wrapped <- paste(wrapped_lines, collapse = "\n")
    
    grid::grid.text(text_wrapped,
              x = 0.08, y = 0.87,
              just = c("left", "top"),
              gp = grid::gpar(fontsize = 6.5, fontfamily = "mono"))
    
    # Page 2: Density raster + Density curve (2 plots)
    if("density_raster" %in% names(plots) && "density_curve" %in% names(plots)) {
      gridExtra::grid.arrange(plots[["density_raster"]],
                   plots[["density_curve"]],
                   ncol = 1)
    }
    
    grDevices::dev.off()
    
    cat(sprintf("PDF report saved to: %s\n", pdf_output))
    pdf_file_path <- pdf_output
  } else {
    cat("\n5. PDF generation skipped (generate_pdf = FALSE)\n")
  }
  
  # ===========================================================================
  # RETURN RESULTS
  # ===========================================================================
  
  result <- list(
    summary = list(
      n_points = n_points,
      extent = extent,
      area_xy = area_xy
    ),
    density_analysis = density_result,
    voxel_analysis = voxel_stats_list,
    plots = plots,
    text_output = text_lines,
    pdf_file = pdf_file_path
  )
  
  invisible(result)
}
