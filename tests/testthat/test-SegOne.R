library(testthat)
library(data.table)
library(PiC)
library(withr)

# Funzione per generare un tronco d'albero verticale con rumore
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

# Funzione per generare chioma dell'albero
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

# Funzione per generare il terreno forestale con variazione di altezza
generate_forest_floor <- function(x_range, y_range, resolution = 0.1, roughness = 0.05) {
  x_coords <- seq(x_range[1], x_range[2], by = resolution)
  y_coords <- seq(y_range[1], y_range[2], by = resolution)
  
  floor_points <- expand.grid(x = x_coords, y = y_coords)
  
  floor_points$z <- sin(floor_points$x/2) * 0.1 +
    cos(floor_points$y/2) * 0.1 +
    rnorm(nrow(floor_points), 0, roughness)
  
  floor_points
}

# Test principale
test_that("SegOne funziona correttamente con una scena forestale realistica", {

  # SegOne writes a classified LAS file, which requires the optional 'lidR' package
  skip_if_not_installed("lidR")

  # Usa withr per gestione temporanea
  temp_path <- withr::local_tempdir()
  
  # Dimensioni della scena
  x_range <- c(0, 20)
  y_range <- c(0, 20)
  
  # Genera forest floor
  floor <- generate_forest_floor(x_range, y_range)
  
  # Genera un solo albero per velocizzare il test
  set.seed(123)  # Per riproducibilità
  trunk_x <- 10
  trunk_y <- 10
  height <- 8
  radius <- 0.3
  
  trunk <- generate_trunk(trunk_x, trunk_y, height, radius)
  crown <- generate_crown(trunk_x, trunk_y, height * 0.7, height / 3)
  
  # Combina tutti i punti
  forest_data <- rbind(
    floor,
    trunk,
    crown
  )
  
  # Aggiungi rumore casuale
  forest_data$x <- forest_data$x + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$y <- forest_data$y + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$z <- forest_data$z + rnorm(nrow(forest_data), 0, 0.01)
  
  # Test della funzione
  result <- NULL
  expect_no_error({
    result <- SegOne(
      a = forest_data,
      filename = "test_tree",
      dimVox = 2,
      th = 1,
      eps = 2,
      mpts = 6,
      N = 200,
      R = 30,
      output_path = temp_path,
      calculate_metrics = FALSE  # Disabilita metriche per velocizzare
    )
  })
  
  # SegOne v4.2 output: single classified LAS file
  # Filename: <filename>_dim<dimVox>_th<th>_eps<eps>_mpts<mpts>.las
  las_file <- file.path(temp_path, "test_tree_dim2_th1_eps2_mpts6.las")

  expect_true(file.exists(las_file),
              paste("File LAS non trovato:", las_file,
                    "\nFile presenti:", paste(list.files(temp_path), collapse = ", ")))

  # Verifica contenuto del file LAS solo se esiste
  if (file.exists(las_file)) {
    las <- lidR::readLAS(las_file)
    wood <- las@data[Classification == 4L]
    leaf <- las@data[Classification == 5L]

    expect_gt(nrow(wood), 100, "Numero insufficiente di punti del tronco.")
    expect_gt(nrow(leaf), 100, "Numero insufficiente di punti della chioma.")
    expect_gt(max(wood$Z, na.rm = TRUE), 2,
              "Il tronco dovrebbe essere alto almeno 2m.")
  }
})
