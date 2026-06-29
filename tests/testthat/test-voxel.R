library(testthat)
library(data.table)
library(PiC)
library(withr)

# Funzioni di generazione dati
generate_test_data <- function() {
  # Genera terreno base (ridotto a 10m x 10m per velocità)
  n_ground <- 5000
  ground <- data.frame(
    x = round(runif(n_ground, 0, 10), 3),
    y = round(runif(n_ground, 0, 10), 3),
    z = round(abs(rnorm(n_ground, 0.2, 0.1)), 3)  # Assicura z positivo
  )
  
  # Genera 2 alberi (ridotto da 3 per velocità)
  trees <- list()
  set.seed(123)
  
  for(i in 1:2) {
    # Tronco
    tree_x <- runif(1, 3, 7)
    tree_y <- runif(1, 3, 7)
    height <- runif(1, 6, 8)
    
    # Punti del tronco
    z_points <- seq(0.2, height, by = 0.1)
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
    crown_points <- 2000  # ridotto da 5000
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
  
  # Assicura che tutti i punti abbiano z >= 0
  all_points$z <- pmax(all_points$z, 0.01)
  
  return(all_points)
}

# Test per Voxels
test_that("Voxels funziona correttamente con dati forestali", {
  
  # Genera dati di test
  test_data <- generate_test_data()
  
  # Usa withr per gestione temporanea
  temp_path <- withr::local_tempdir()
  
  # Test con parametri base (solo uno per velocità)
  expect_no_error({
    voxel_result <- Voxels(
      a = test_data,
      filename = "test_voxels",
      dimVox = 5,
      th = 2,
      output_path = temp_path,
      coordinate_precision = "mm"  # Test coordinate precision parameter
    )
  })
  
  # Verifica l'esistenza del file di output
  voxel_file <- file.path(temp_path, "test_voxels_dim5_th2_vox.txt")
  
  expect_true(file.exists(voxel_file), 
              paste("File voxel non trovato:", voxel_file))
  
  # Leggi il file di output
  if (file.exists(voxel_file)) {
    voxel_output <- fread(voxel_file)
    
    # Verifiche base
    expect_equal(ncol(voxel_output), 4, 
                 info = "Il file voxel dovrebbe avere 4 colonne: u, v, w, N.")
    expect_true(all(voxel_output$N >= 2), 
                "Il numero di punti per voxel non rispetta la soglia.")
    expect_true(all(voxel_output$u >= 1), 
                "I valori u dei voxel dovrebbero partire da 1.")
    expect_true(all(voxel_output$v >= 1), 
                "I valori v dei voxel dovrebbero partire da 1.")
    expect_true(all(voxel_output$w >= 1), 
                "I valori w dei voxel dovrebbero partire da 1.")
    expect_gt(nrow(voxel_output), 0,
              "Dovrebbero esserci voxel generati")
  }
})

# Test coordinate_precision parameter
test_that("Voxels gestisce correttamente il parametro coordinate_precision", {

  # Genera dati di test più densi per evitare voxel vuoti
  set.seed(456)
  test_data <- data.frame(
    x = rep(seq(0, 10, by = 0.1), 10),
    y = rep(seq(0, 10, by = 0.1), each = 10),
    z = runif(1010, 0, 5)
  )

  temp_path <- withr::local_tempdir()

  # Test con precision "mm" e th=1 per dati sparsi
  expect_no_error({
    Voxels(
      a = test_data,
      filename = "test_mm",
      dimVox = 10,  # voxel più grandi per catturare più punti
      th = 1,       # soglia più bassa
      output_path = temp_path,
      coordinate_precision = "mm"
    )
  })

  # Test con precision "cm"
  expect_no_error({
    Voxels(
      a = test_data,
      filename = "test_cm",
      dimVox = 10,
      th = 1,
      output_path = temp_path,
      coordinate_precision = "cm"
    )
  })

  # Verifica che i file siano stati creati
  expect_true(file.exists(file.path(temp_path, "test_mm_dim10_th1_vox.txt")))
  expect_true(file.exists(file.path(temp_path, "test_cm_dim10_th1_vox.txt")))
})
