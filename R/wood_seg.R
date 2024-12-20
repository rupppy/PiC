#' Wood voxels segmentation
#'
#'
#' @name Wood_seg
#'
#' @description Point cloud segmentation to identify wood voxels
#' @param filename - Output file prefix
#' @param a - AGB voxelized input file
#' @param eps - size (radius) of the epsilon neighborhood - Default = 1
#' @param mpts - number of minimum points required in the eps neighborhood for core points (including the point itself) - Default = 4
#' @param N - Minimum number of voxel in a wood cluster - Default = 1000
#' @param R - R = Standard deviation * Proportion of Variance - Default = 30

#' @return One file (.txt) output - Wood voxels (vox)

#' @importFrom tictoc tic toc
#' @importFrom dbscan dbscan
#' @importFrom data.table fwrite fread setDT setkey data.table
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join anti_join
#' @importFrom foreach foreach %dopar%  %do%
#' @importFrom collapse fcount na_omit
#' @importFrom stats prcomp na.omit
#' @importFrom utils read.csv

#' @export

utils::globalVariables(c("u", "v", "w", "cls"))

Wood_seg <- function(a, filename = "XXX", eps = 1, mpts = 4, N = 1000, R = 30) {

  tic('Wood segmentation time')
  ###############
  # Wood segmentation
  colnames(a) <- c("u", "v", "w")
  wood <- data.frame(a$u, a$v, a$w)
  colnames(wood) <- c("u", "v", "w")

  b <- dbscan(wood, eps = eps, minPts = mpts)
  y1 <- cbind(wood, b$cluster)
  y <- data.frame(y1)
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
    cluster <- data.frame(valCls["x"], valCls["y"], valCls["z"], valCls["cls"], r, pop_cls)
    fwrite(cluster, file = paste0(filename, "_WoodVox_eps", eps, "_mpts", mpts, ".txt"), append = TRUE, row.names = FALSE)
  }
  toc()
}
