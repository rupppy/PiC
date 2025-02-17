#' @name SegOne
#' @title Single Tree wood leaf segmentation
#' @description Wood - leaf segmentation of single tree
#' @usage SegOne(a, filename = "Elab_single_tree", dimVox = 2, th = 2, 
#' eps = 1, mpts = 4, N = 1000, R = 30, output_path = tempdir())
#' @param a - input file
#' @param filename - file output prefix
#' @param dimVox - voxel dimension in cm - Default = 2
#' @param th - Minimum number of points to generate a voxel - Default = 2
#' @param filename - Output file prefix
#' @param a - AGB voxelized input file
#' @param eps - size (radius) of the epsilon neighborhood - Default = 1
#' @param mpts - number of minimum points required in the eps neighborhood for core points (including the point itself) - Default = 4
#' @param N - Minimum number of voxel in a wood cluster - Default = 1000
#' @param R - R = Standard deviation * Proportion of Variance - Default = 30
#' @param output_path Directory in cui scrivere i file di output. Default = tempdir()
#' @return Two file (.txt) in output - Wood points and non wood points
#'
#' @importFrom tictoc tic toc
#' @importFrom dbscan dbscan
#' @importFrom data.table fwrite fread data.table setDT setkey
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join anti_join semi_join
#' @importFrom foreach foreach %dopar%  %do%
#' @importFrom collapse fcount na_omit
#' @importFrom stats prcomp
#' @importFrom utils read.csv
#'
#' @export


utils::globalVariables(c("u", "v", "w", "cls"))

SegOne <- function(a, filename = "Elab_single_tree", dimVox = 2,
                   th = 2, eps = 1, mpts = 4, N = 1000, R = 30,
                   output_path = tempdir()) {

  tic('Total time')

  ########

  u<-NULL
  v<-NULL
  w<-NULL
  cluster0<-NULL

  # Controlla se la directory esiste, altrimenti creala
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  dim <- dimVox / 100
  plot <- paste0(filename, "_dim", dimVox, "_th", th)

  a <- data.frame(a)
  colnames(a) <- c("x", "y", "z")
  xmin <- min(a$x)
  ymin <- min(a$y)
  zmin <- min(a$z)
  # rendere positive le coordinate
  if (xmin < 0) {
    AA <- data.frame(a["x"] + abs(xmin), a['x'], a["y"], a["z"])
    colnames(AA)<-c('xP','x','y','z')
  } else {
    AA <- data.frame(a["x"] - abs(xmin), a['x'], a["y"], a["z"])
    colnames(AA)<-c('xP','x','y','z')
  }
  if (ymin < 0) {
    BB <- data.frame(AA['xP'], AA["x"], AA["y"] + abs(ymin), AA['y'], AA["z"])
    colnames(BB)<-c('xP','x','yP','y','z')
  } else {
    BB <- data.frame(AA['xP'], AA["x"], AA["y"] - abs(ymin), AA['y'], AA["z"])
    colnames(BB)<-c('xP','x','yP','y','z')
  }
  if (zmin < 0) {
    CC <- data.frame(BB["xP"], BB["x"], BB["yP"], BB["y"], BB["z"] + abs(zmin), BB["z"])
    colnames(CC)<-c('xP','x','yP','y','zP', 'z')
  } else {
    CC <- data.frame(BB["xP"], BB["x"], BB["yP"], BB["y"], BB["z"] - abs(zmin), BB["z"])
    colnames(CC)<-c('xP','x','yP','y','zP', 'z')
  }

  # crea la tabella un voxel per ogni punto (con doppioni)
  AAvox <- data.frame(as.integer(CC$x / dim) + 1, as.integer(CC$y / dim) + 1, as.integer(CC$z / dim) + 1)
  colnames(AAvox) <- c("u", "v", "w")

  # crea una tabella di corrispondenza voxel/punto

  AAvoxRAW<-data.frame(CC$x,CC$y,CC$z, AAvox$u,AAvox$v,AAvox$w)
  colnames(AAvoxRAW) <- c('x','y', 'z','u', 'v', 'w')
  #fwrite(AAvoxRAW, file.path(tempdir(),file = paste0(plot,'_vox_raw.txt')), row.names = FALSE)

  # crea la nuvola di voxel, con valori univoci e quarta colonna col numero di punti per voxel
  AAvox1 <- data.frame(AAvox %>% fcount(u, v, w))

  # crea una tabella con i soli voxel che contengono un numeri di punti superiore a th
  AAvoxels <- (AAvox1[AAvox1["N"] >= th, ])
  if(is.null(AAvoxels)) {message ('No wood cluster generated, might change values of dimVox and th')}





# Wood voxels segmentation


  ###############
  # Wood segmentation
  #colnames(a) <- c("u", "v", "w")
  wood <- data.frame(AAvoxels$u, AAvoxels$v, AAvoxels$w)
  #colnames(wood) <- c("u", "v", "w")

  b <- dbscan(wood, eps = eps, minPts = mpts)
  y1 <- cbind(wood, b$cluster)
  y <- data.frame(y1)
  colnames(y) <- c("u", "v", "w", "cls")
  freq_cls <- data.frame(table(y$cls))
  colnames(freq_cls) <- c("cls", "num")
  minCls <- min(y["cls"])
  maxCls <- max(y["cls"])
  good_cluster <- data.frame(freq_cls[freq_cls["num"] > N, ])
  for (CLS in good_cluster$cls) {
    if (CLS == 0) {
      next
    }
    pop_cls <- freq_cls[freq_cls["cls"] == CLS, 2]
    valCls <- data.frame(y[y["cls"] == CLS, ])
    d <- data.frame(valCls["u"], valCls["v"], valCls["w"])
    h <- prcomp(d)
    sdev <- h$sdev
    p <- sdev[1]
    s <- summary(h)
    w <- s$importance
    q <- w[2, 1]
    r <- p * q
    if (r < R) {
      next
    }
    cluster <- data.frame(valCls["u"], valCls["v"], valCls["w"]) #, valCls["cls"], r, pop_cls)
    cluster0<-rbind(cluster, cluster0)
  }


  if(is.null(cluster0))
   stop ('No wood cluster generated, might change values of eps and mpts')



  woodPoints0<-suppressMessages(inner_join(AAvoxRAW, cluster0))



  leafPoints0<-suppressMessages(anti_join(AAvoxRAW, cluster0))



  a<-data.frame(leafPoints0['x'], leafPoints0['y'], leafPoints0['z'])

  leaf<-NULL
  leaf1<-NULL
  nr<-1
  z1<-min(a$z)
  zmax<-max(a$z)
  #slice by 0.1 m
  sliceLeaf<-data.frame(a$x, a$y, a$z, as.integer(a$z*10))
  colnames(sliceLeaf) <- c("x", "y", "z", "nr")

  clusterquote<- data.frame(sliceLeaf %>% fcount(nr))
  cutoff<-clusterquote[clusterquote['N']>200,]
  leafon<-suppressMessages(inner_join(sliceLeaf,cutoff))



  woodPoints<-data.frame(woodPoints0$x, woodPoints0$y, woodPoints0$z)
  colnames(woodPoints)<-c("x", "y", "z")
  leafPoints<-data.frame(leafon$x, leafon$y, leafon$z)
  colnames(leafPoints)<-c("x", "y", "z")
  
  fwrite(woodPoints, file.path(output_path, paste0(filename,'_DBSCAN_wood.txt')))
  fwrite(leafPoints, file.path(output_path, paste0(filename,'_DBSCAN_leaf.txt')))
  
  message("File di legno scritto in:", file.path(output_path, paste0(filename, '_DBSCAN_wood.txt')), "\n")
  message("File di foglie scritto in:", file.path(output_path, paste0(filename, '_DBSCAN_leaf.txt')), "\n")
  

  toc()
}


