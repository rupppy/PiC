#' Whole pointcloud segmentation process
#' @name Forest_seg
#' @title Forest component segmentation
#'
#' @description Segments the input .xyz pointcloud file into different forestry layers.
#' @usage Forest_seg (a, filename="XXX", dimVox = 2, th = 2,
#' eps = 2, mpts = 6, h_tree = 1, soil_dim= 0.3,
#' N = 500, R = 30, Vox_print = FALSE, WoodVox_print = FALSE, output_path = tempdir())
#'
#' @param a - Input file (.xyz)
#' @param filename - Output file prefix
#' @param dimVox - Voxel dimension (cm) - Default = 2
#' @param th - Minimum number of point to generate a voxel. Default = 2
#' @param eps - size (radius) of the epsilon neighborhood - Default = 1
#' @param mpts - number of minimum points required in the eps neighborhood for core points (including the point itself) - Default = 4
#' @param h_tree - minumum trunk lenght (m)
#' @param soil_dim - Voxel dimension (m) for forest floor segmentation - Default = 0.30
#' @param N - Minimum number of voxel in a wood cluster - Default = 1000
#' @param R - R = Standard deviation * Proportion of Variance - Default = 30
#' @param Vox_print - Print point cloud voxelization. Default FALSE
#' @param WoodVox_print - Print wood voxelization
#' @param output_path Directory in cui scrivere i file di output. Default = tempdir()
#'
#' @return 6 files (.txt) output. 1. Voxelized pointcloud. 2. Forest floor (vox). 3. AGB (vox) 4. DTM. 5. Wood (vox) 6. AGB no wood
#'
#' @importFrom tictoc tic toc
#' @importFrom dbscan dbscan
#' @importFrom data.table fwrite fread setDT setkey data.table
#' @importFrom data.table fdroplevels
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join anti_join
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom collapse fcount na_omit
#' @importFrom stats prcomp na.omit
#' @importFrom utils read.csv
#' @export



utils::globalVariables(c("u", "v", "w", "cls"))


Forest_seg <- function(a, filename="XXX", dimVox = 2, th = 2,
                        eps = 2, mpts = 6, h_tree = 1, soil_dim= 0.3,
                        N = 500, R = 30, Vox_print = FALSE, WoodVox_print = FALSE,
                       output_path = tempdir()) {
  ########

  tic('Total time')

  ###########
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }



  colnames(a)<-c('x', 'y', 'z')

  # voxelizzo, tabella di corrispomdenza punto/voxel con passo dim, un record per ciascun punto

  AAvox <- data.frame(a$x, a$y, a$z, as.integer(a$x / soil_dim) + 1, as.integer(a$y / soil_dim) + 1, as.integer(a$z / soil_dim) + 1)
  colnames(AAvox) <- c('x', 'y', 'z', 'u', 'v', 'w')


  # aggrego i punti per voxel

  AAvox1 <- data.frame(AAvox %>% fcount(u, v, w))


  # seleziono i voxel con un minimo di punti th

  AAvoxels <- (AAvox1[AAvox1["N"] >= th, ])


  # seleziono i voxel minimi di ciascuna coppia di x e y

  voxel_minimi <- aggregate(w ~ u + v, AAvoxels, min)
  colnames(voxel_minimi) <- c("u", "v", "wmin")


  # porto in un piano w=0 tutte le colonne xy
  #a ciascuna coppia xy della tabella completa associo la w del voxel più basso della stessa coppia

  p0<-inner_join(AAvoxels, voxel_minimi)
  p1 <- data.frame(p0['u'], p0['v'], p0['w'], p0['w'] - p0['wmin'])
  colnames(p1) <- c('u', 'v', 'w', 'w0')

  # separo i primi soil_dim m di strato basale, considerato come forest floor, dal resto della nuvola considerata AGB
  Forest_floor0<-p1[p1['w0'] <= 1.05, ]
  Forest_floor0 <- na.omit(Forest_floor0)  # Rimuovi eventuali valori NA
  

  Forest_floor00<-data.frame(Forest_floor0$u, Forest_floor0$v, Forest_floor0$w)
  bf <- dbscan(Forest_floor00, eps = 1, minPts = 3)
  y1f <- cbind(Forest_floor00, bf$cluster)
  colnames(y1f)<-c('u', 'v', 'w', 'cls')
  nc<- data.frame(y1f %>% fcount(cls))
  good_cluster<-nc[nc['N']> 400 & nc['cls']!=0,]
  y2f<-inner_join(y1f,good_cluster)
  colnames(y2f)<-c('u', 'v', 'w', 'cls', 'N')



  Forest_floor1<-inner_join(AAvox,y2f)



  Forest_floor<-data.frame(Forest_floor1$x, Forest_floor1$y, Forest_floor1$z)
  colnames(Forest_floor)<-c('x','y','z')
  fwrite(Forest_floor, file.path(output_path, paste0(filename, '_Forest_floor_eps', eps, '_mpts', mpts, '.txt')))
  
  message("File scritto in:", file.path(output_path, paste0(filename, '_Forest_floor_eps', eps, '_mpts', mpts, '.txt')), "\n")



  AGB0<-p1[p1['w0'] >1,]
  ant<-anti_join(Forest_floor0,y2f)

  #genera voxel AGB
  AGB1<-rbind(AGB0, ant)

  #genera punti AGB
  AGB2<-inner_join(AAvox,AGB1)
  AGB<-data.frame(AGB2['x'], AGB2['y'], AGB2['z'])
  AGB2<-NULL


  ############
  ############


  u<-NULL
  v<-NULL
  w<-NULL
  xr<-NULL


  dim <- dimVox / 100
  plot <- paste0(filename, "_dim", dimVox, "_th", th) # nome del lavoro completo

  xmin <- min(AGB$x)
  ymin <- min(AGB$y)
  zmin <- min(AGB$z)

  # crea la tabella un voxel per ogni punto (con doppioni)
  AAvox <- data.frame(AGB$x, AGB$y, AGB$z, as.integer(AGB$x / dim) + 1, as.integer(AGB$y / dim) + 1, as.integer(AGB$z / dim) + 1)
  colnames(AAvox) <- c('x', 'y', 'z', 'u', 'v', 'w')

  # create a voxel cloud, con valori univoci e quarta colonna col numero di punti per voxel

  AAvox1<-data.frame(AAvox['u'], AAvox['v'], AAvox['w'])
  AAvox2 <- data.frame(AAvox1 %>% fcount(u, v, w))

  # crea una tabella con i soli voxel che contengono un numeri di punti superiore a th
  AAvoxels <- (AAvox2[AAvox2["N"] >= th, ])
  AAvox1<-NULL
  AAvox2<-NULL

  if (Vox_print) {
    fwrite(AAvoxels, file.path(output_path, paste0(plot, "_vox.txt")), row.names = FALSE)
    
    message("File scritto in:", file.path(output_path, paste0(filename, '_vox.txt')), "\n")
  }



  ###############
  ###############
  ###############
  # Wood segmentation



  agbw <- data.frame(AAvoxels$u, AAvoxels$v, AAvoxels$w)
  colnames(agbw) <- c("u", "v", "w")
  agbw<-na.omit(agbw)
  b <- dbscan(agbw, eps = eps, minPts = mpts)
  y1 <- cbind(agbw, b$cluster)
  agbw<-NULL
  cluster<-NULL
  y <- data.frame(y1)
  y1<-NULL
  colnames(y) <- c("x", "y", "z", "cls")
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
    d <- data.frame(valCls["x"], valCls["y"], valCls["z"])
    if (max(valCls["z"])-min(valCls["z"])<(h_tree/dim)) {
      next
    }
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
    cluster0 <- data.frame(valCls["x"], valCls["y"], valCls["z"], valCls["cls"], r, pop_cls)
    cluster<-rbind(cluster,cluster0)
  }
  if(is.null(cluster))
  { stop('No wood cluster generated, might change values of eps and mpts')}

  colnames(cluster)<-c('u','v','w', 'cls', 'r', 'pop_cls')
  if(WoodVox_print) {
    fwrite(cluster, file.path(output_path, paste0(plot,'_WoodVox_eps', eps, '_mpts', mpts, '.txt')))
    
    message("File scritto in:", file.path(output_path, paste0(filename, '_WoodVox_eps', eps, '_mpts', mpts, '.txt')), "\n")
  }

  woodpoint0<-inner_join(AAvox, cluster)
  woodpoint<-data.frame(woodpoint0['x'], woodpoint0['y'], woodpoint0['z'], woodpoint0['cls'])
  woodpoint0=NULL
  fwrite(woodpoint, file.path(output_path, paste0(plot, "_Wood_eps", eps, "_mpts", mpts, ".txt")), row.names = FALSE)
  
  message("File scritto in:", file.path(output_path, paste0(filename, "_Wood_eps", eps, "_mpts", mpts, ".txt")), "\n")


  ###########
  ## Generating AGB without wood

  # Parametri
  voxel_size <- 0.2  #  metri


  # Funzione per creare voxel più grandi

  # crea la tabella un voxel per ogni punto legno (con doppioni)
  AAvoxL <- data.frame( as.integer(woodpoint$x / voxel_size) + 1, as.integer(woodpoint$y / voxel_size) + 1, as.integer(woodpoint$z / voxel_size) + 1)
  colnames(AAvoxL) <- c('u', 'v', 'w')

  # create a voxel cloud, con valori univoci e quarta colonna col numero di punti per voxel

  AAvox1L<-data.frame(AAvoxL['u'], AAvoxL['v'], AAvoxL['w'])
  AAvox2L <- data.frame(AAvox1L %>% fcount(u, v, w))

  # crea la tabella un voxel per ogni punto AGB (con doppioni)
  AAvoxA <- data.frame(AGB$x, AGB$y, AGB$z, as.integer(AGB$x / voxel_size) + 1, as.integer(AGB$y / voxel_size) + 1, as.integer(AGB$z / voxel_size) + 1)
  colnames(AAvoxA) <- c('x', 'y', 'z', 'u', 'v', 'w')

  # create a voxel cloud, con valori univoci e quarta colonna col numero di punti per voxel

  AAvox2A <- data.frame(AAvoxA %>% fcount(u, v, w))

  # Assicuriamoci che entrambe le tabelle siano data.table
  setDT(AAvox2L)
  setDT(AAvox2A)

  # Impostiamo le chiavi per entrambe le tabelle
  setkey(AAvox2L, u, v, w)
  setkey(AAvox2A, u, v, w)

  # Sottraiamo AAvox2L da AAvox2A
  AAvoxD <- AAvox2A[!AAvox2L]

  AGB_def0<-inner_join(AAvoxD,AAvoxA)
  AGB_def<-data.table(AGB_def0$x, AGB_def0$y, AGB_def0$z)

  colnames(AGB_def)<-c('x','y','z')

  fwrite(AGB_def, file.path(output_path, paste0(plot, "_AGBnoWOOD_eps", eps, "_mpts", mpts, ".txt")), row.names = FALSE)
  
  message("File scritto in:", file.path(output_path, paste0(filename, "_AGBnoWOOD_eps", eps, "_mpts", mpts, ".txt")), "\n")


  toc()
}

