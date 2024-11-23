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


# Crea il test fixture
defer({
  files_to_remove <- list.files(pattern = "test_forest", full.names = TRUE)
  unlink(files_to_remove)
})

test_that("Forest_floor3 funziona correttamente con dati forestali", {
  # Dimensioni della scena
  x_range <- c(0, 10)
  y_range <- c(0, 10)

  # Genera forest floor
  floor <- generate_forest_floor(x_range, y_range, resolution = 0.1, roughness = 0.02)

  # Debug floor
  cat("\nPunti nel floor:", nrow(floor), "\n")
  cat("Range z nel floor:", range(floor$z), "\n")

  # Genera alberi con parametri più estremi per facilitare la segmentazione
  trees <- list()
  crowns <- list()
  set.seed(123)

  n_trees <- 5
  for(i in 1:n_trees) {
    trunk_x <- runif(1, x_range[1] + 1, x_range[2] - 1)
    trunk_y <- runif(1, y_range[1] + 1, y_range[2] - 1)
    height <- runif(1, 3, 5)  # Alberi più bassi
    radius <- 0.2  # Raggio fisso per maggiore stabilità

    # Genera tronco con più punti
    trunk_points <- generate_trunk(trunk_x, trunk_y, height, radius, points_per_meter = 1000)
    # Forza l'altezza minima del tronco a essere significativamente sopra il suolo
    trunk_points$z <- trunk_points$z + 0.5
    trees[[i]] <- trunk_points

    # Genera chioma con più punti e più alta del tronco
    crown_points <- generate_crown(trunk_x, trunk_y, height, height/2, points_per_volume = 10000)
    # Forza l'altezza minima della chioma a essere sopra il tronco
    crown_points$z <- crown_points$z + height/2
    crowns[[i]] <- crown_points
  }

  # Combina i punti
  all_trees <- do.call(rbind, trees)
  all_crowns <- do.call(rbind, crowns)

  # Debug trees e crowns
  cat("Punti nei tronchi:", nrow(all_trees), "\n")
  cat("Range z nei tronchi:", range(all_trees$z), "\n")
  cat("Punti nelle chiome:", nrow(all_crowns), "\n")
  cat("Range z nelle chiome:", range(all_crowns$z), "\n")

  # Combina tutti i punti assicurandosi che ci sia una chiara separazione in altezza
  forest_data <- rbind(
    transform(floor, type = "floor"),
    transform(all_trees, type = "trunk"),
    transform(all_crowns, type = "crown")
  )

  # Debug dataset finale
  cat("\nDataset finale:\n")
  cat("Totale punti:", nrow(forest_data), "\n")
  cat("Range z totale:", range(forest_data$z), "\n")
  cat("Distribuzione punti per tipo:\n")
  print(table(forest_data$type))

  # Parametri Floseg modificati
  result <- Floseg(forest_data, "test_forest",
                   Soil_dim = 30,  # Valore più piccolo per maggiore dettaglio
                   th = 5,        # Soglia più bassa
                   N = 5)           # Numero minimo di punti molto basso

  # Verifica esistenza file
  if(!file.exists("test_forest_Forest_floor.txt")) {
    stop("File Forest_floor non creato")
  }
  if(!file.exists("test_forest_AGB.txt")) {
    stop("File AGB non creato")
  }

  # Leggi i file con controlli
  forest_floor <- fread("test_forest_Forest_floor.txt")
  agb <- fread("test_forest_AGB.txt")

  # Debug output
  cat("\nOutput Floseg:\n")
  cat("Punti nel forest floor:", nrow(forest_floor), "\n")
  if(nrow(forest_floor) > 0) {
    cat("Range z nel forest floor:", range(forest_floor$z), "\n")
  }
  cat("Punti nell'AGB:", nrow(agb), "\n")
  if(nrow(agb) > 0) {
    cat("Range z nell'AGB:", range(agb$AGB2.z), "\n")
  }

  # Test base
  expect_gt(nrow(forest_floor), 0, "Nessun punto nel forest floor")
  expect_gt(nrow(agb), 0, "Nessun punto nell'AGB")

  # Test sulle altezze solo se entrambi i dataset hanno punti
  if(nrow(forest_floor) > 0 && nrow(agb) > 0) {
    max_floor_height <- max(forest_floor$z)
    min_agb_height <- min(agb$AGB2.z)

    cat("\nVerifica separazione altezze:\n")
    cat("Massima altezza forest floor:", max_floor_height, "\n")
    cat("Minima altezza AGB:", if(nrow(agb) > 0 && !all(is.na(agb$z))) min(agb$z, na.rm = TRUE) else "NA", "\n")

    expect_lt(max_floor_height, min_agb_height,
              "Il forest floor dovrebbe essere più basso dell'AGB")
  }
})
