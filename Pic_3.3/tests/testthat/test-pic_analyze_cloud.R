library(testthat)
library(data.table)
library(PiC)
library(withr)

# Funzione per generare il terreno forestale
generate_forest_floor <- function(x_range, y_range, resolution = 0.1, roughness = 0.05) {
  x_coords <- seq(x_range[1], x_range[2], by = resolution)
  y_coords <- seq(y_range[1], y_range[2], by = resolution)

  floor_points <- expand.grid(x = x_coords, y = y_coords)

  floor_points$z <- (0.4 + sin(floor_points$x/2) * 0.1 +
                       cos(floor_points$y/2) * 0.1 +
                       rnorm(nrow(floor_points), 0, roughness))/2

  floor_points
}

# Funzione per generare un tronco d'albero
generate_trunk <- function(base_x, base_y, height, radius, points_per_meter = 200) {
  z_points <- seq(0, height, length.out = height * points_per_meter)

  trunk_points <- lapply(z_points, function(z) {
    angles <- seq(0, 2 * pi, length.out = 120)
    x <- base_x + radius * cos(angles) + rnorm(length(angles), 0, 0.01)
    y <- base_y + radius * sin(angles) + rnorm(length(angles), 0, 0.01)
    data.frame(x = x, y = y, z = rep(z, length(angles)))
  })

  do.call(rbind, trunk_points)
}

# Funzione per generare la chioma di un albero
generate_crown <- function(trunk_x, trunk_y, trunk_height, crown_radius, points_per_volume = 1000) {
  n_points <- points_per_volume * crown_radius^3

  theta <- runif(n_points, 0, 2 * pi)
  phi <- runif(n_points, 0, pi)
  r <- crown_radius * runif(n_points)^(1 / 3)

  x <- trunk_x + r * sin(phi) * cos(theta)
  y <- trunk_y + r * sin(phi) * sin(theta)
  z <- trunk_height + r * cos(phi)

  data.frame(x = x, y = y, z = z)
}

# Test principale
test_that("pic_analyze_cloud funziona correttamente con dati forestali", {

  temp_path <- withr::local_tempdir()

  # Dimensioni della scena
  x_range <- c(0, 10)
  y_range <- c(0, 10)

  # Genera il terreno forestale
  floor <- generate_forest_floor(x_range, y_range, resolution = 0.1, roughness = 0.02)

  # Genera alberi
  trees <- list()
  crowns <- list()
  set.seed(42)

  n_trees <- 3
  for (i in 1:n_trees) {
    trunk_x <- runif(1, x_range[1] + 1, x_range[2] - 1)
    trunk_y <- runif(1, y_range[1] + 1, y_range[2] - 1)
    height <- runif(1, 5, 10)
    radius <- runif(1, 0.2, 0.3)

    trees[[i]] <- generate_trunk(trunk_x, trunk_y, height, radius)
    crowns[[i]] <- generate_crown(trunk_x, trunk_y, height * 0.7, height / 3)
  }

  forest_data <- rbind(
    floor,
    do.call(rbind, trees),
    do.call(rbind, crowns)
  )

  # Aggiungi rumore casuale
  forest_data$x <- forest_data$x + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$y <- forest_data$y + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$z <- forest_data$z + rnorm(nrow(forest_data), 0, 0.01)

  # Esegui pic_analyze_cloud con generazione PDF
  result <- NULL
  expect_no_error({
    result <- pic_analyze_cloud(
      points = forest_data,
      voxel_sizes = c(0.1, 0.5),
      generate_pdf = TRUE,
      output_path = temp_path
    )
  })

  # Verifica struttura del risultato
  expect_type(result, "list")
  expect_true("summary" %in% names(result))
  expect_true("density_analysis" %in% names(result))
  expect_true("voxel_analysis" %in% names(result))
  expect_true("plots" %in% names(result))
  expect_true("text_output" %in% names(result))
  expect_true("pdf_file" %in% names(result))

  # Verifica summary
  expect_equal(result$summary$n_points, nrow(forest_data))
  expect_gt(result$summary$area_xy, 0)
  expect_s3_class(result$summary$extent, "data.frame")
  expect_true(all(c("x_min", "x_max", "y_min", "y_max", "z_min", "z_max") %in%
                    names(result$summary$extent)))

  # Verifica density_analysis
  expect_gt(result$density_analysis$density_median, 0)
  expect_gt(result$density_analysis$density_mean, 0)
  expect_s4_class(result$density_analysis$raster, "SpatRaster")
  expect_gt(length(result$density_analysis$points_per_cell), 0)

  # Verifica voxel_analysis per entrambe le risoluzioni
  expect_true("0.1" %in% names(result$voxel_analysis))
  expect_true("0.5" %in% names(result$voxel_analysis))

  for (vsize in c("0.1", "0.5")) {
    va <- result$voxel_analysis[[vsize]]
    expect_true("stats" %in% names(va))
    expect_gt(va$stats$n_voxels, 0)
    expect_gt(va$stats$dens_mean, 0)
    expect_gt(va$stats$dens_median, 0)
  }

  # Verifica plots
  expect_true("density_raster" %in% names(result$plots))
  expect_true("density_curve" %in% names(result$plots))
  expect_s3_class(result$plots[["density_raster"]], "ggplot")
  expect_s3_class(result$plots[["density_curve"]], "ggplot")

  # Verifica text_output
  expect_gt(length(result$text_output), 0)

  # Verifica PDF generato
  expect_true(!is.null(result$pdf_file))
  expect_true(file.exists(result$pdf_file))
  expect_gt(file.size(result$pdf_file), 0)
})

# Test senza generazione PDF
test_that("pic_analyze_cloud funziona senza generazione PDF", {

  temp_path <- withr::local_tempdir()

  # Genera una nuvola semplice (solo terreno + 1 albero)
  x_range <- c(0, 5)
  y_range <- c(0, 5)

  floor <- generate_forest_floor(x_range, y_range, resolution = 0.2, roughness = 0.02)

  set.seed(99)
  trunk <- generate_trunk(2.5, 2.5, 6, 0.25)
  crown <- generate_crown(2.5, 2.5, 6 * 0.7, 2)

  forest_data <- rbind(floor, trunk, crown)

  result <- NULL
  expect_no_error({
    result <- pic_analyze_cloud(
      points = forest_data,
      voxel_sizes = 0.1,
      generate_pdf = FALSE,
      output_path = temp_path
    )
  })

  # Il PDF non deve essere generato
  expect_null(result$pdf_file)

  # Ma i plots devono essere comunque presenti
  expect_true("density_raster" %in% names(result$plots))
  expect_true("density_curve" %in% names(result$plots))
  expect_s3_class(result$plots[["density_raster"]], "ggplot")
  expect_s3_class(result$plots[["density_curve"]], "ggplot")

  # Le statistiche devono essere valide
  expect_gt(result$summary$n_points, 0)
  expect_gt(result$density_analysis$density_mean, 0)
})

# Test con input data.table
test_that("pic_analyze_cloud accetta input data.table", {

  temp_path <- withr::local_tempdir()

  set.seed(77)
  dt <- data.table(
    x = runif(5000, 0, 10),
    y = runif(5000, 0, 10),
    z = c(rnorm(4000, 0.5, 0.1), runif(1000, 1, 8))
  )

  result <- NULL
  expect_no_error({
    result <- pic_analyze_cloud(
      points = dt,
      voxel_sizes = 0.1,
      generate_pdf = FALSE,
      output_path = temp_path
    )
  })

  expect_equal(result$summary$n_points, 5000)
  expect_gt(result$summary$area_xy, 0)
})
