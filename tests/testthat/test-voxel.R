library(testthat)
library(data.table)
library(PiC)

# Funzioni di generazione dati
generate_test_data <- function() {
  # Genera terreno base (20m x 20m)
  n_ground <- 10000
  ground <- data.frame(
    x = round(runif(n_ground, 0, 20), 3),
    y = round(runif(n_ground, 0, 20), 3),
    z = round(rnorm(n_ground, 0, 0.1), 3)  # terreno leggermente irregolare
  )

  # Genera 3 alberi
  trees <- list()
  for(i in 1:3) {
    # Tronco
    tree_x <- runif(1, 5, 15)
    tree_y <- runif(1, 5, 15)
    height <- runif(1, 8, 12)

    # Punti del tronco
    z_points <- seq(0, height, by = 0.05)
    trunk_points <- lapply(z_points, function(z) {
      angles <- seq(0, 2*pi, length.out = 8)
      radius <- 0.3  # raggio tronco 30cm
      x <- round(tree_x + radius * cos(angles), 3)
      y <- round(tree_y + radius * sin(angles), 3)
      data.frame(x = x, y = y, z = rep(z, length(angles)))
    })
    trunk <- do.call(rbind, trunk_points)

    # Chioma
    crown_height <- height * 0.7
    crown_points <- 5000  # punti della chioma
    crown <- data.frame(
      x = round(rnorm(crown_points, tree_x, 1.5), 3),
      y = round(rnorm(crown_points, tree_y, 1.5), 3),
      z = round(runif(crown_points, crown_height, height), 3)
    )

    trees[[i]] <- rbind(trunk, crown)
  }

  # Combina tutti i punti
  all_points <- rbind(
    ground,
    do.call(rbind, trees)
  )

  return(all_points)
}


# Test per Voxels
test_that("Voxels funziona correttamente con dati forestali", {
  # Genera dati di test
  test_data <- generate_test_data()

  # Specifica la directory temporanea
  temp_path <- withr::local_tempdir()
  
  # Test con diversi parametri
  test_cases <- list(
    list(dimVox = 2, th = 1),   # caso base
    list(dimVox = 5, th = 2),   # voxel più grandi
    list(dimVox = 10, th = 3)    # voxel più piccoli
  )

  for(case in test_cases) {
    # Esegui Voxels
    voxel_result <- Voxels(test_data,
                           filename = "test_voxels",
                           dimVox = case$dimVox,
                           th = case$th,
                           output_path = temp_path)

    # Verifica l'esistenza dei file di output
    voxel_file <- file.path(temp_path, paste0("test_voxels_dim", case$dimVox, "_th", case$th, "_vox.txt"))
    voxel_raw_file <- file.path(temp_path, paste0("test_voxels_dim", case$dimVox, "_th", case$th, "_vox_raw.txt"))
    
    expect_true(file.exists(voxel_file), paste("File voxel non trovato:", voxel_file))
    expect_true(file.exists(voxel_raw_file), paste("File voxel RAW non trovato:", voxel_raw_file))
    
    # Leggi il file di output
    voxel_output <- fread(voxel_file)
    
    # Verifiche base
    expect_true(ncol(voxel_output) == 4, "Il file voxel dovrebbe avere 4 colonne: u, v, w, N.")
    expect_true(all(voxel_output$N >= case$th), "Il numero di punti per voxel non rispetta la soglia.")
    expect_true(all(voxel_output$u >= 1), "I valori u dei voxel dovrebbero partire da 1.")
    expect_true(all(voxel_output$v >= 1), "I valori v dei voxel dovrebbero partire da 1.")
    expect_true(all(voxel_output$w >= 1), "I valori w dei voxel dovrebbero partire da 1.")
  }


})

