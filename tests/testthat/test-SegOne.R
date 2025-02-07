library(testthat)
library(data.table)
library(PiC)
library(withr)

# Funzione per generare un tronco d'albero verticale con rumore
generate_trunk <- function(base_x, base_y, height, radius, points_per_meter = 200) {
  # Punti lungo l'altezza
  z_points <- seq(0, height, length.out = height * points_per_meter)

  # Per ogni altezza, genero punti in circonferenza
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

  # Genera punti in una sfera
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

  # Aggiungi variazione di altezza realistica
  floor_points$z <- sin(floor_points$x/2) * 0.1 +
    cos(floor_points$y/2) * 0.1 +
    rnorm(nrow(floor_points), 0, roughness)

  floor_points
}


# Genera dataset di test
test_that("Forest_seg7 with realistic forest scene", {
  
  # Percorso temporaneo
  temp_path <- file.path(tempdir(), "test_SegOne")
  
  # Crea la directory temporanea
  if (!dir.exists(temp_path)) {
    dir.create(temp_path, recursive = TRUE)
  }
  
  # Dimensioni della scena
  x_range <- c(0, 20)
  y_range <- c(0, 20)

  
  # Genera forest floor
  floor <- generate_forest_floor(x_range, y_range)

  # Genera un albero
  trees <- list()
  crowns <- list()
  for(i in 1:1) {
    trunk_x <- runif(1, x_range[1], x_range[2])
    trunk_y <- runif(1, y_range[1], y_range[2])
    height <- runif(1, 5, 15)  # altezze tra 5 e 15 metri
    radius <- runif(1, 0.2, 0.4)  # raggi tra 20 e 40 cm

    trees[[i]] <- generate_trunk(trunk_x, trunk_y, height, radius)
    crowns[[i]] <- generate_crown(trunk_x, trunk_y, height*0.7, height/3)
  }

  # Combina tutti i punti
  forest_data <- rbind(
    floor,
    do.call(rbind, trees),
    do.call(rbind, crowns)
  )

  # Aggiungi rumore casuale
  forest_data$x <- forest_data$x + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$y <- forest_data$y + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$z <- forest_data$z + rnorm(nrow(forest_data), 0, 0.01)

  # Test della funzione
  expect_no_error(
    SegOne(
      forest_data,
      filename = "Elab_single_tree",
      dimVox = 2,
      th = 1,
      eps = 2,
      mpts = 6,
      N = 200,
      R = 30,
      output_path = temp_path)
    )

  # Verifica l'esistenza dei file di output
  wood_file <- file.path(temp_path, "Elab_single_tree_DBSCAN_wood.txt")
  leaf_file <- file.path(temp_path, "Elab_single_tree_DBSCAN_leaf.txt")
  
  expect_true(file.exists(wood_file), "File di legno non trovato.")
  expect_true(file.exists(leaf_file), "File di foglie non trovato.")
  
  # Verifica contenuto dei file
  wood <- fread(wood_file)
  leaf <- fread(leaf_file)
  
  expect_true(nrow(wood) > 500, "Numero insufficiente di punti del tronco.")
  expect_true(nrow(leaf) > 400, "Numero insufficiente di punti della chioma.")
  expect_true(max(wood$z) > 3, "Il tronco dovrebbe essere alto almeno 3m.")})

