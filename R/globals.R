#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table .BY
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom stats median quantile sd weighted.mean
#' @importFrom utils head write.csv
NULL

# Declare global variables used in data.table NSE operations
utils::globalVariables(c(
  # data.table special symbols
  ".", ".SD", ".BY", ".I", ".N", ":=",
  
  # Point cloud coordinate columns
  "x", "y", "z",
  
  # Tree metrics columns
  "u", "v", "w", "cls",
  "Tree_n", "X", "Y", "Z_min",
  "Height", "Height_m", "max_z", "min_z",
  "DBH", "DBH_cm", "DBH_valido",
  "Crown_Base_m",
  
  # Other computed columns
  "join_key", "n_points", "num",
  "w_normalized", "coverage_degree", "N",
  "floor_w", "height_bin",
  
  # Voxel columns
  "xP", "yP", "zP", "wmin", "w0"
))
