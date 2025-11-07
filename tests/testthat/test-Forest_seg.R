library(testthat)
library(data.table)
library(PiC)
library(withr)

# Funzioni di utilità per generare dati di test
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
  
  # Test esecuzione
  expect_no_error(
    Forest_seg(
      forest_data,
      filename = "test_forest",
      dimVox = 3,
      th = 1,
      eps = 2,
      mpts = 6,
      h_tree = 1,
      N = 500,
      output_path = temp_path
    )
  )
  
  # Verifica output files
  wood_file <- file.path(temp_path, "test_forest_dim3_th1_Wood_eps2_mpts6.txt")
  floor_file <- file.path(temp_path, "test_forest_dim3_th1_Forest_floor_soil_dim_0.1.txt")
  agb_no_wood_file <- file.path(temp_path, "test_forest_dim3_th1_AGBnoWOOD_eps2_mpts6.txt")
  
  expect_true(file.exists(wood_file), "File Wood non trovato")
  expect_true(file.exists(floor_file), "File Forest Floor non trovato")
  expect_true(file.exists(agb_no_wood_file), "File AGB no wood non trovato")
  
  # Verifica contenuto files
  wood <- fread(wood_file)
  floor_out <- fread(floor_file)
  agb_no_wood <- fread(agb_no_wood_file)
  
  expect_gt(nrow(wood), 1000, "Numero di punti del tronco insufficiente")
  expect_gt(nrow(floor_out), 1000, "Numero di punti del pavimento insufficiente")
  expect_true(all(floor_out$z < 1.05), "Il pavimento dovrebbe essere sotto 1m")
  expect_gt(max(wood$z), 5, "I tronchi dovrebbero essere alti almeno 5m")
})