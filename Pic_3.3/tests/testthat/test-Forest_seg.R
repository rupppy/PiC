library(testthat)
library(data.table)
library(PiC)
library(withr)


generate_trunk <- function(base_x, base_y, height, radius, points_per_meter = 200) {
  z_points <- seq(0, height, length.out = height * points_per_meter)
  
  trunk_points <- lapply(z_points, function(z) {
    angles <- seq(0, 2*pi, length.out = 120)
    x <- base_x + radius * cos(angles) + rnorm(length(angles), 0, 0.01)
    y <- base_y + radius * sin(angles) + rnorm(length(angles), 0, 0.01)
    data.frame(x = x, y = y, z = rep(z, length(angles)))
  })
  
  do.call(rbind, trunk_points)
}

generate_crown <- function(trunk_x, trunk_y, trunk_height, crown_radius, points_per_volume = 1000) {
  n_points <- points_per_volume * crown_radius^3
  
  theta <- runif(n_points, 0, 2*pi)
  phi <- runif(n_points, 0, pi)
  r <- crown_radius * runif(n_points)^(1/3)
  
  x <- trunk_x + r * sin(phi) * cos(theta)
  y <- trunk_y + r * sin(phi) * sin(theta)
  z <- trunk_height + r * cos(phi)
  
  data.frame(x = x, y = y, z = z)
}

generate_forest_floor <- function(x_range, y_range, resolution = 0.1, roughness = 0.05) {
  x_coords <- seq(x_range[1], x_range[2], by = resolution)
  y_coords <- seq(y_range[1], y_range[2], by = resolution)
  
  floor_points <- expand.grid(x = x_coords, y = y_coords)
  floor_points$z <- (0.4 + sin(floor_points$x/2) * 0.1 +
                       cos(floor_points$y/2) * 0.1 +
                       rnorm(nrow(floor_points), 0, roughness))/2
  
  floor_points
}

# Test principale
test_that("Forest_seg funziona correttamente con una scena forestale realistica", {
  # Setup
  temp_path <- file.path(tempdir(), "test_forest_seg")
  dir.create(temp_path, recursive = TRUE)
  
  # Cleanup dopo il test
  defer({
    unlink(temp_path, recursive = TRUE)
    files_to_remove <- list.files(pattern = "test_forest", full.names = TRUE)
    unlink(files_to_remove)
  })
  
  # Generazione dati
  x_range <- c(0, 20)
  y_range <- c(0, 20)
  floor <- generate_forest_floor(x_range, y_range)
  
  trees <- list()
  crowns <- list()
  for(i in 1:4) {
    trunk_x <- runif(1, x_range[1], x_range[2])
    trunk_y <- runif(1, y_range[1], y_range[2])
    height <- runif(1, 5, 15)
    radius <- runif(1, 0.2, 0.4)
    
    trees[[i]] <- generate_trunk(trunk_x, trunk_y, height, radius)
    crowns[[i]] <- generate_crown(trunk_x, trunk_y, height*0.7, height/3)
  }
  
  forest_data <- rbind(
    floor,
    do.call(rbind, trees),
    do.call(rbind, crowns)
  )
  
  # Aggiungi rumore
  forest_data$x <- forest_data$x + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$y <- forest_data$y + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$z <- forest_data$z + rnorm(nrow(forest_data), 0, 0.01)
  
  # Test esecuzione - UPDATED for Forest_seg v3.0.4
  expect_no_error(
    Forest_seg(
      forest_data,
      filename = "test_forest",
      integer_precision = "mm",

      # DTM parameters
      dtm_coarse_res = 0.5,
      tolerance = 0.4,

      # Wood segmentation
      dimVox = 2,
      th = 2,
      eps = 2,
      mpts = 6,
      h_trunk = 1,  # v3.0.4: renamed from h_tree
      N = 500,
      w_linear = 0.5,
      Vox_print = FALSE,
      Woodpoints_print = TRUE,

      # DBH parameters (v3.0.4 uses fixed heights: 1.3, 1.8, 2.3)
      dbh_tolerance = 0.05,
      dbh_max_rmse = 5,
      dbh_min_radius = 0.025,
      dbh_max_radius = 0.8,

      # DAV crown-understory separation
      canopy_vox_dim = 0.15,  # v3.0.4: renamed from dav_voxel_size
      canopy_min_density = 100,
      dav_understory_max_start = 1.3,
      dav_height_factor = 0.8,
      dav_eps = 1,
      dav_minPts = 3,

      # CBH parameters (v3.0.4: GAB Voronoi with voxel coordinates)
      calculate_cbh = FALSE,
      cbh_hex_side = 0.15,
      cbh_min_branch_length = 2.0,
      cbh_save_points = FALSE,

      # Canopy analysis
      analyze_canopy = FALSE,
      canopy_bulk_vox = 0.30,

      # Output
      output_format = "xyz",
      output_path = temp_path,
      generate_reports = TRUE)
  )
  
  # Debug:
  cat("\nFile creati nella directory:", list.files(temp_path), "\n")

  # Verifica output files - v3.0.4 naming scheme
  wood_file <- file.path(temp_path, "test_forest_wood_valid.xyz")
  floor_file <- file.path(temp_path, "test_forest_floor.xyz")
  crown_file <- file.path(temp_path, "test_forest_crown.xyz")
  understory_file <- file.path(temp_path, "test_forest_understory.xyz")

  expect_true(file.exists(wood_file),
              paste("File di legno non trovato:", wood_file,
                    "\nFile presenti:", paste(list.files(temp_path), collapse=", ")))
  expect_true(file.exists(floor_file),
              paste("File forest floor non trovato:", floor_file,
                    "\nFile presenti:", paste(list.files(temp_path), collapse=", ")))
  expect_true(file.exists(crown_file),
              paste("File crown non trovato:", crown_file,
                    "\nFile presenti:", paste(list.files(temp_path), collapse=", ")))
  expect_true(file.exists(understory_file),
              paste("File understory non trovato:", understory_file,
                    "\nFile presenti:", paste(list.files(temp_path), collapse=", ")))

  # Verifica contenuto files
  wood <- fread(wood_file)
  floor_out <- fread(floor_file)
  crown <- fread(crown_file)
  understory <- fread(understory_file)

  expect_gt(nrow(wood), 100, "Numero di punti del tronco insufficiente")
  expect_gt(nrow(floor_out), 1000, "Numero di punti del pavimento insufficiente")
  expect_gt(nrow(crown), 100, "Numero di punti della chioma insufficiente")
  # Floor extraction is based on DTM + tolerance (0.4m)
  # Test data has rough terrain + noise, so allow up to 2m
  expect_lt(mean(floor_out$V3), 1.0, "Media floor dovrebbe essere sotto 1m")
  expect_gt(max(wood$V3), 3, "I tronchi dovrebbero essere alti almeno 3m")
})