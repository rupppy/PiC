
#' @name Floseg
#' @title Forest floor segmentation
#'
#' @description Segments the input .xyz pointcloud file into different forestry layers: forest floor and above ground biomass.
#'
#' @usage Floseg(a, filename="XXX", soil_dim = 0.3, th = 20, N=500)
#'
#' @param a - Input file (.xyz)
#' @param filename - Output file prefix
#' @param soil_dim - Voxel dimension (m) for forest floor segmentation - Default = 0.30
#' @param th - Minimum number of point to generate a voxel. Default = 20
#' @param N - Minimum number of voxel to generate a cluster. Default = 500

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

Floseg <- function(a, filename="XXX", soil_dim = 0.3, th = 20, N = 500) {
  ########

#sink("Execution time.txt")
#{
tic('Forest Floor segmentation')



#input_file<- nuvola di punti

colnames(a)<-c('x', 'y', 'z')

# voxelizzo, tabella di corrispondenza punto/voxel con passo dim, un record per ciascun punto
tic('AAVox')
AAvox <- data.frame(a$x, a$y, a$z, as.integer(a$x / soil_dim) + 1, as.integer(a$y / soil_dim) + 1, as.integer(a$z / soil_dim) + 1)
colnames(AAvox) <- c('x', 'y', 'z', 'u', 'v', 'w')
toc()

# aggrego i punti per voxel
tic('AAvox1')
AAvox1 <- data.frame(AAvox %>% fcount(u, v, w))
toc()

# seleziono i voxel con un minimo di punti th
tic('AAvoxels')
AAvoxels <- (AAvox1[AAvox1["N"] >= th, ])
toc()

# seleziono i voxel minimi di ciascuna coppia di x e y
tic('voxel_minimi')
voxel_minimi <- aggregate(w ~ u + v, AAvoxels, min)
colnames(voxel_minimi) <- c("u", "v", "wmin")
toc()

# porto in un piano w=0 tutte le colonne xy
#a ciascuna coppia xy della tabella completa associo la w del voxel più basso della stessa coppia
tic('p0')
p0<-inner_join(AAvoxels, voxel_minimi)
toc()

tic('p1')
p1<-data.frame(p0['u'], p0['v'], p0['w'], p0['w']-p0['wmin'])
colnames(p1) <- c('u', 'v', 'w', 'w0')
toc()

# separo i primi dim m di strato basale, considerato come forest floor, dal resto della nuvola considerata AGB
tic('Forest_floor0')
Forest_floor0<-p1[p1['w0'] <= 1,]
toc()

tic('dbscan')
Forest_floor00<-data.frame(Forest_floor0$u, Forest_floor0$v, Forest_floor0$w)
b <- dbscan(Forest_floor00, eps = 1, minPts = 3)
y1 <- cbind(Forest_floor00, b$cluster)
colnames(y1)<-c('u', 'v', 'w', 'cls')
nc<- data.frame(y1 %>% fcount(cls))
good_cluster<-nc[nc['N']> N & nc['cls']!=0,]
y2<-inner_join(y1,good_cluster)
colnames(y2)<-c('u', 'v', 'w', 'cls', 'N')
toc()

tic('Forest_floor1')
Forest_floor1<-inner_join(AAvox,y2)
toc()

tic('Forest_foor')
Forest_floor<-data.frame(Forest_floor1$x, Forest_floor1$y, Forest_floor1$z)
colnames(Forest_floor)<-c('x','y','z')
toc()

tic('AGB0')
AGB0<-p1[p1['w0'] >1,]
ant<-anti_join(Forest_floor0,y2)
AGB1<-rbind(AGB0, ant)
AGB2<-inner_join(AAvox,AGB1)
AGB<-data.frame(AGB2$x, AGB2$y, AGB2$z)
toc()

tic('write')
fwrite(Forest_floor, file = paste0(filename,'_Forest_floor.txt'))
fwrite(AGB, file = paste0(filename,'_AGB.txt'))
toc()

toc()
}
