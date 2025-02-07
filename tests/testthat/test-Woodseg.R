library(testthat)
library(data.table)
library(collapse)
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

# Crea il test fixture
defer({
  files_to_remove <- list.files(pattern = "test_forest", full.names = TRUE)
  unlink(files_to_remove)
})

# Genera dataset di test
test_that("Wood_seg funziona correttamente con dati forestali", {
  
  # Specifica una directory temporanea
  temp_path <- withr::local_tempdir()
  
  # Dimensioni della scena
  x_range <- c(0, 20)
  y_range <- c(0, 20)

  # Genera solo alberi (tronchi e chiome)
  trees <- list()
  crowns <- list()

  # Genera 4 alberi come nel test precedente
  for(i in 1:4) {
    trunk_x <- runif(1, x_range[1], x_range[2])
    trunk_y <- runif(1, y_range[1], y_range[2])
    height <- runif(1, 5, 15)  # altezze tra 5 e 15 metri
    radius <- runif(1, 0.2, 0.4)  # raggi tra 20 e 40 cm

    trees[[i]] <- generate_trunk(trunk_x, trunk_y, height, radius)
    crowns[[i]] <- generate_crown(trunk_x, trunk_y, height*0.7, height/3)
  }

  # Combina i punti solo di tronchi e chiome
  forest_data <- rbind(
    do.call(rbind, trees),
    do.call(rbind, crowns)
  )

  # Aggiungi rumore casuale
  forest_data$x <- forest_data$x + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$y <- forest_data$y + rnorm(nrow(forest_data), 0, 0.01)
  forest_data$z <- forest_data$z + rnorm(nrow(forest_data), 0, 0.01)

  # crea la tabella un voxel per ogni punto (con doppioni)
  dim<-0.03
  AAvox <- data.frame(as.integer(forest_data$x / dim) + 1, as.integer(forest_data$y / dim) + 1, as.integer(forest_data$z / dim) + 1)
  colnames(AAvox) <- c("u", "v", "w")

  # crea la nuvola di voxel, con valori univoci e quarta colonna col numero di punti per voxel
  AAvox1 <- data.frame(AAvox %>% fcount(u, v, w))

  forest_data_vox<- data.frame(AAvox1$u, AAvox1$v, AAvox1$w)

  # Test della funzione
  expect_no_error(
    Woodseg(
      forest_data_vox,
      filename = "test_wood",
      eps = 2,
      mpts = 6,
      N = 500,
      R = 30,
      output_path = temp_path
    )
  )

  # Verifica l'esistenza del file di output
  output_file <- file.path(temp_path, "test_wood_WoodVox_eps2_mpts6.txt")
  expect_true(file.exists(output_file), "File di output non trovato.")
  
  # Verifica il contenuto del file di output
  wood_out <- fread(output_file)
  
  expect_true(nrow(wood_out) > 0, "Nessun punto identificato come legno.")
  expect_true(all(colnames(wood_out) == c("u", "v", "w", "cls", "r", "pop_cls")),
              "Colonne nel file di output non corrette.")
  expect_true(max(wood_out$w) > 5, "L'altezza massima del legno è insufficiente.")
  
})
