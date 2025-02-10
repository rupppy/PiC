
#' @name Floseg
#' @title Forest floor segmentation
#'
#' @description Segments the input .xyz pointcloud file into different forestry layers: forest floor and above ground biomass.
#'
#' @usage Floseg(a, filename="XXX", soil_dim = 0.3, th = 20, N=500, output_path = tempdir())
#'
#' @param a - Input file (.xyz)
#' @param filename - Output file prefix
#' @param soil_dim - Voxel dimension (m) for forest floor segmentation - Default = 0.30
#' @param th - Minimum number of point to generate a voxel. Default = 20
#' @param N - Minimum number of voxel to generate a cluster. Default = 500
#' @param output_path Directory in cui scrivere i file di output. Default = tempdir()

#' @return 2 files (.txt) output. 1. Forest floor pointcolud; 2. AGB pointcloud

#' @importFrom tictoc tic toc
#' @importFrom dbscan dbscan
#' @importFrom data.table fwrite fread setDT setkey data.table
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join anti_join
#' @importFrom collapse fcount na_omit
#' @importFrom stats aggregate na.omit


#' @export

utils::globalVariables(c("u", "v", "w", "cls"))

Floseg <- function(a, filename="XXX", soil_dim = 0.3, th = 20, N = 500, output_path = tempdir()) {

tic('Forest Floor segmentation')

 ## Controlla se la directory esiste
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  


#input_file<- nuvola di punti

colnames(a)<-c('x', 'y', 'z')

# voxelizzo, tabella di corrispondenza punto/voxel con passo dim, un record per ciascun punto
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
p1<-data.frame(p0['u'], p0['v'], p0['w'], p0['w']-p0['wmin'])
colnames(p1) <- c('u', 'v', 'w', 'w0')

# separo i primi dim m di strato basale, considerato come forest floor, dal resto della nuvola considerata AGB
Forest_floor0<-p1[p1['w0'] <= 1,]

Forest_floor00<-data.frame(Forest_floor0$u, Forest_floor0$v, Forest_floor0$w)
b <- dbscan(Forest_floor00, eps = 1, minPts = 3)
y1 <- cbind(Forest_floor00, b$cluster)
colnames(y1)<-c('u', 'v', 'w', 'cls')
nc<- data.frame(y1 %>% fcount(cls))
good_cluster<-nc[nc['N']> N & nc['cls']!=0,]
y2<-inner_join(y1,good_cluster)
colnames(y2)<-c('u', 'v', 'w', 'cls', 'N')

Forest_floor1<-inner_join(AAvox,y2)

Forest_floor<-data.frame(Forest_floor1$x, Forest_floor1$y, Forest_floor1$z)
colnames(Forest_floor)<-c('x','y','z')

AGB0<-p1[p1['w0'] >1,]
ant<-anti_join(Forest_floor0,y2)
AGB1<-rbind(AGB0, ant)
AGB2<-inner_join(AAvox,AGB1)
AGB<-data.frame(AGB2$x, AGB2$y, AGB2$z)

# Scrivi i file nella directory specificata
fwrite(Forest_floor, file.path(output_path, 'Forest_floor.txt'))
fwrite(AGB, file.path(output_path, 'AGB.txt'))

message("File Forest_floor scritto in:", file.path(output_path, 'Forest_floor.txt'), "\n")
message("File AGB scritto in:", file.path(output_path, 'AGB.txt'), "\n")


toc()
}
