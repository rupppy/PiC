library(testthat)
library(data.table)
library(PiC)  

# Funzione per generare il terreno forestale
generate_forest_floor <- function(x_range, y_range, resolution = 0.1, roughness = 0.05) {
  x_coords <- seq(x_range[1], x_range[2], by = resolution)
  y_coords <- seq(y_range[1], y_range[2], by = resolution)
  
  floor_points <- expand.grid(x = x_coords, y = y_coords)
  
  # Aggiungi variazioni di altezza
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
test_that("Floseg funziona correttamente con dati forestali", {
  # Dimensioni della scena
  x_range <- c(0, 10)
  y_range <- c(0, 10)
  
  # Genera il terreno forestale
  floor <- generate_forest_floor(x_range, y_range, resolution = 0.1, roughness = 0.02)
  
  # Genera alberi
  trees <- list()
  crowns <- list()
  set.seed(123)  # Per riproducibilità
  
  n_trees <- 5
  for (i in 1:n_trees) {
    trunk_x <- runif(1, x_range[1] + 1, x_range[2] - 1)
    trunk_y <- runif(1, y_range[1] + 1, y_range[2] - 1)
    height <- runif(1, 3, 5)
    radius <- 0.2
    
    trunk_points <- generate_trunk(trunk_x, trunk_y, height, radius, points_per_meter = 1000)
    trunk_points$z <- trunk_points$z + 0.5
    trees[[i]] <- trunk_points
    
    crown_points <- generate_crown(trunk_x, trunk_y, height, height / 2, points_per_volume = 10000)
    crown_points$z <- crown_points$z + height / 2
    crowns[[i]] <- crown_points
  }
  
  all_trees <- do.call(rbind, trees)
  all_crowns <- do.call(rbind, crowns)
  
  forest_data <- rbind(
    transform(floor, type = "floor"),
    transform(all_trees, type = "trunk"),
    transform(all_crowns, type = "crown")
  )
  
  # Percorso temporaneo
  temp_path <- file.path(tempdir(), "test_forest")
  
  # Crea la directory temporanea
  if (!dir.exists(temp_path)) {
    dir.create(temp_path, recursive = TRUE)
  }
  
  # Esegui Floseg
  result <- Floseg(forest_data, filename = "test_forest",
                   soil_dim = 0.3, 
                   th = 5,        
                   N = 5,
                   output_path = temp_path)
  
  # Verifica che i file siano stati creati
  forest_floor_file <- file.path(temp_path, "Forest_floor.txt")
  agb_file <- file.path(temp_path, "AGB.txt")
  
  cat("Percorso del file Forest_floor:", forest_floor_file, "\n")
  cat("Percorso del file AGB:", agb_file, "\n")
  
  expect_true(file.exists(forest_floor_file), "File Forest_floor non creato")
  expect_true(file.exists(agb_file), "File AGB non creato")
  
  # Leggi i file e verifica il contenuto
  forest_floor <- fread(forest_floor_file)
  agb <- fread(agb_file)
  
  expect_gt(nrow(forest_floor), 0, "Nessun punto nel forest floor")
  expect_gt(nrow(agb), 0, "Nessun punto nell'AGB")
  
  # Verifica logica: il terreno dovrebbe essere più basso degli alberi
  if (nrow(forest_floor) > 0 && nrow(agb) > 0) {
    max_floor_height <- max(forest_floor$z, na.rm = TRUE)
    min_agb_height <- min(agb$z, na.rm = TRUE)
    
    expect_lt(max_floor_height, min_agb_height,
              "Il forest floor dovrebbe essere più basso dell'AGB")
  }
  
  # Rimuovi la directory temporanea
  unlink(temp_path, recursive = TRUE)
})