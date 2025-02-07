#' Voxelize point cloud
#' @name Voxels
#' @description Transform pointcloud in voxel
#' @usage Voxels(a, filename = "XXX", dimVox = 2, th = 2, output_path = tempdir())
#' @param a - input file
#' @param filename - file output prefix
#' @param dimVox - voxel dimension in cm - Default = 2
#' @param th - Minimum number of point to generate a voxel - Default = 1

#' @return Voxelized pointcloud
#'
#' @param th Minimum number of point to generate a voxel (Default = 1)
#' Is a parameter that should be used with caution;
#' it generates a lightened cloud with fewer points.
#' To be evaluated in relation with the dimVox parameter,
#' for high point densities it is efficae to remove noise (outliers)
#' @param output_path Directory in cui scrivere i file di output. Default = tempdir()

#' @importFrom tictoc tic toc
#' @importFrom data.table fwrite fread data.table setDT setkey
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join anti_join semi_join
#' @importFrom collapse fcount na_omit
#' @importFrom utils read.csv
#' @export


utils::globalVariables(c("u", "v", "w", "cls"))

Voxels <- function(a, filename = "XXX", dimVox = 2, th = 2, output_path = tempdir()) {

  tic('Voxelizing time')

  ########

  u<-NULL
  v<-NULL
  w<-NULL

  dim <- dimVox / 100
  plot <- paste0(filename, "_dim", dimVox, "_th", th)

  a <- data.frame(a)
  colnames(a) <- c("x", "y", "z")
  xmin <- min(a$x)
  ymin <- min(a$y)
  zmin <- min(a$z)
  # rendere positive le coordinate
  if (xmin < 0) {
    AA <- data.frame(a["x"] + abs(xmin), a["y"], a["z"])
  } else {
    AA <- data.frame(a["x"] - abs(xmin), a["y"], a["z"])
  }
  if (ymin < 0) {
    BB <- data.frame(AA["x"], AA["y"] + abs(ymin), AA["z"])
  } else {
    BB <- data.frame(AA["x"], AA["y"] - abs(ymin), AA["z"])
  }
  if (zmin < 0) {
    CC <- data.frame(BB["x"], BB["y"], BB["z"] + abs(zmin))
  } else {
    CC <- data.frame(BB["x"], BB["y"], BB["z"] - abs(zmin))
  }

  # crea la tabella un voxel per ogni punto (con doppioni)
  AAvox <- data.frame(as.integer(CC$x / dim) + 1, as.integer(CC$y / dim) + 1, as.integer(CC$z / dim) + 1)
  colnames(AAvox) <- c("u", "v", "w")

  # crea una tabella di corrispondenza voxel/punto
  AAvoxRAW<-data.frame(CC$x,CC$y,CC$z, AAvox$u,AAvox$v,AAvox$w)
  fwrite(AAvoxRAW, file.path(output_path, paste0(plot,'_vox_raw.txt')), row.names = FALSE)

  # crea la nuvola di voxel, con valori univoci e quarta colonna col numero di punti per voxel
  AAvox1 <- data.frame(AAvox %>% fcount(u, v, w))

  # crea una tabella con i soli voxel che contengono un numeri di punti superiore a th
  AAvoxels <- (AAvox1[AAvox1["N"] >= th, ])
  if(is.null(AAvoxels))
  { stop('No wood cluster generated, might deacrese th value')}

  fwrite(AAvoxels, file.path(output_path, paste0(plot, "_vox.txt")), row.names = FALSE)
  toc()
}


