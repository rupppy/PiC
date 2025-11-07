# PiC Package Documentation
# File: R/PiC-package.R
# Place this file in your R/ directory

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' PiC: Pointcloud Interactive Computation for Forest Structure Analysis
#'
#' @description
#' The PiC package provides advanced algorithms for analyzing forest point cloud 
#' data acquired through terrestrial laser scanning (TLS). Key features include 
#' fast voxelization of large datasets, automatic segmentation into forest components 
#' (ground, wood, foliage), extraction of individual tree metrics, and canopy 
#' structure analysis.
#'
#' @section Main Functions:
#' The main functions of the package are:
#' 
#' \describe{
#'   \item{\code{\link{Forest_seg}}}{Complete forest stand analysis: performs 
#'     segmentation into forest floor, wood and foliage, calculates individual 
#'     tree metrics (DBH, height, crown base) and canopy metrics (coverage, 
#'     volume, vertical profile).}
#'   
#'   \item{\code{\link{SegOne}}}{Single tree segmentation: separates wood from 
#'     foliage using voxelization, DBSCAN clustering and PCA analysis. Calculates 
#'     height, DBH and crown base for the analyzed tree.}
#'   
#'   \item{\code{\link{Floseg}}}{Forest floor segmentation: identifies and separates 
#'     ground points from above-ground biomass using voxelization and clustering. 
#'     Useful for terrain normalization and understory analysis.}
#'   
#'   \item{\code{\link{Voxels}}}{Basic voxelization: converts point cloud into 
#'     voxel representation with configurable resolution. Useful for data reduction 
#'     and pre-processing.}
#'   
#'   \item{\code{\link{run_PiC}}}{Launches the interactive Shiny interface for 
#'     visual analysis and parameter selection. Allows file upload, parameter 
#'     configuration, analysis execution and 3D visualization of results.}
#' }
#'
#' @section Typical Workflow:
#' A typical workflow with PiC includes:
#' 
#' \enumerate{
#'   \item Load the point cloud (format .xyz, .txt, or .csv)
#'   \item Execute segmentation with \code{\link{Forest_seg}} or specific functions
#'   \item Analyze resulting metrics (tree-level and plot-level)
#'   \item Visualize results with the Shiny interface (\code{\link{run_PiC}})
#' }
#'
#' @section Algorithm Details:
#' 
#' \subsection{Voxelization}{
#'   Voxelization transforms the continuous point cloud into a discrete 3D grid. 
#'   Each voxel contains the number of laser points falling within it. Voxel size 
#'   influences analysis resolution:
#'   \itemize{
#'     \item Small voxels (1-2 cm): Greater detail, longer computation time
#'     \item Medium voxels (2-3 cm): Optimal balance for most cases
#'     \item Large voxels (3-5 cm): Faster analysis, less detail
#'   }
#' }
#' 
#' \subsection{DBSCAN Clustering}{
#'   The DBSCAN (Density-Based Spatial Clustering of Applications with Noise) 
#'   algorithm identifies clusters of densely connected voxels. Main parameters are:
#'   \itemize{
#'     \item \code{eps}: Neighborhood search radius (in voxel units)
#'     \item \code{mpts}: Minimum number of points to form a cluster
#'   }
#'   Clusters with elongated shape and high principal variance are classified 
#'   as wood (trunks and branches).
#' }
#' 
#' \subsection{Wood-Leaf Separation}{
#'   Wood-leaf separation is based on PCA (Principal Component Analysis) of 
#'   identified clusters. Clusters with:
#'   \itemize{
#'     \item High principal length (PC1)
#'     \item High variance explained by PC1
#'     \item Cylindrical/elongated shape
#'   }
#'   are classified as wood. The \code{R} parameter (PCA threshold) controls 
#'   this classification.
#' }
#'
#' @section Data Requirements:
#' 
#' \subsection{Input Format}{
#'   Point clouds must be provided as data frames with at least 3 columns:
#'   \itemize{
#'     \item \code{x}: X coordinate (meters)
#'     \item \code{y}: Y coordinate (meters)
#'     \item \code{z}: Z coordinate or height (meters)
#'   }
#'   Supported formats: .xyz, .txt, .csv, .asc
#' }
#' 
#' \subsection{Data Quality}{
#'   For optimal results:
#'   \itemize{
#'     \item Minimum density: ~1000 points/m²
#'     \item Multiple scans (360°) to reduce occlusions
#'     \item Pre-processing: noise removal, scan registration
#'     \item Georeferencing (optional)
#'   }
#' }
#'
#' @section Output Files:
#' 
#' Segmentation functions produce several output files:
#' 
#' \subsection{Tree-Level Metrics}{
#'   File: \code{*_tree_report.csv}
#'   \itemize{
#'     \item \code{Tree_n}: Tree identification number
#'     \item \code{X_tree, Y_tree}: Tree base coordinates
#'     \item \code{Z_min}: Minimum height (ground level)
#'     \item \code{Height_m}: Total tree height (m)
#'     \item \code{DBH_cm}: Diameter at breast height (cm, measured at 1.3m)
#'     \item \code{Crown_Base_m}: Crown base height (m)
#'   }
#' }
#' 
#' \subsection{Plot-Level Metrics}{
#'   File: \code{*_plot_report.csv}
#'   \itemize{
#'     \item \code{tree_count}: Number of detected trees
#'     \item \code{valid_tree_count}: Trees with valid DBH
#'     \item \code{mean_dbh_cm, median_dbh_cm}: DBH statistics
#'     \item \code{mean_cbh_m, median_cbh_m}: Crown base statistics
#'     \item \code{coverage_area_m2}: Canopy coverage area
#'     \item \code{coverage_percentage}: Coverage percentage
#'     \item \code{canopy_volume_m3}: Canopy volume
#'     \item \code{trees_per_hectare}: Tree density (n/ha)
#'     \item \code{basal_area_m2_ha}: Basal area (m²/ha)
#'   }
#' }
#' 
#' \subsection{Point Cloud Files}{
#'   \itemize{
#'     \item \code{*_Forest_floor*.txt}: Ground points
#'     \item \code{*_Wood*.txt}: Wood points (trunks and branches)
#'     \item \code{*_AGBnoWOOD*.txt}: Foliage points
#'     \item \code{*_vox.txt}: Voxelized cloud (if requested)
#'   }
#' }
#'
#' @section Parameter Selection Guide:
#' 
#' \subsection{Forest Type Recommendations}{
#'   \strong{Mediterranean Pine Forest:}
#'   \preformatted{
#'     dimVox = 2, th = 2, eps = 2, mpts = 9, 
#'     soil_dim = 0.1, N = 500, R = 30
#'   }
#'   
#'   \strong{Dense Deciduous Forest:}
#'   \preformatted{
#'     dimVox = 2, th = 3, eps = 2.5, mpts = 12, 
#'     soil_dim = 0.15, N = 800, R = 35
#'   }
#'   
#'   \strong{Open Boreal Forest:}
#'   \preformatted{
#'     dimVox = 3, th = 2, eps = 3, mpts = 9, 
#'     soil_dim = 0.2, N = 400, R = 25
#'   }
#'   
#'   \strong{Young Plantation:}
#'   \preformatted{
#'     dimVox = 1, th = 2, eps = 1.5, mpts = 6, 
#'     soil_dim = 0.1, N = 300, R = 20
#'   }
#' }
#' 
#' \subsection{Adaptive Parameter Selection}{
#'   If results are not satisfactory:
#'   \itemize{
#'     \item \strong{Too many small clusters}: Increase \code{N} (min voxels)
#'     \item \strong{Trees not separated}: Reduce \code{eps}
#'     \item \strong{Fragmented trunks}: Increase \code{eps}
#'     \item \strong{Wood classified as leaves}: Reduce \code{R}
#'     \item \strong{Leaves classified as wood}: Increase \code{R}
#'   }
#' }
#'
#' @section Performance Considerations:
#' 
#' \subsection{Memory Requirements}{
#'   Approximate memory required:
#'   \itemize{
#'     \item 1M points: ~50 MB RAM
#'     \item 10M points: ~500 MB RAM
#'     \item 100M points: ~5 GB RAM
#'   }
#'   For very large datasets (>50M points), consider:
#'   \itemize{
#'     \item Preliminary voxelization with \code{\link{Voxels}}
#'     \item Subdivision into smaller plots
#'     \item Increase voxel size
#'   }
#' }
#' 
#' \subsection{Processing Time}{
#'   Approximate times on standard laptop (Intel i7, 16GB RAM):
#'   \itemize{
#'     \item 1M points: 30-60 seconds
#'     \item 10M points: 5-10 minutes
#'     \item 100M points: 30-60 minutes
#'   }
#'   Canopy analysis adds ~20-30\% to total time.
#' }
#'
#' @section Shiny Interface:
#' 
#' The interactive Shiny interface (\code{\link{run_PiC}}) offers:
#' 
#' \itemize{
#'   \item \strong{File Upload}: Supports .txt, .xyz, .csv up to 100GB
#'   \item \strong{Parameter Configuration}: Sliders and inputs for all parameters
#'   \item \strong{Output Directory Selection}: Choose save directory
#'   \item \strong{Real-time Progress}: Progress bar during analysis
#'   \item \strong{3D Visualization}: Interactive Plotly to inspect results
#'   \item \strong{Auto File Management}: Automatic refresh of output file list
#'   \item \strong{Cross-platform}: Windows, macOS, Linux
#' }
#' 
#' To launch the interface:
#' \preformatted{
#'   library(PiC)
#'   run_PiC()
#' }
#'
#' @section Limitations:
#' 
#' \itemize{
#'   \item \strong{Occlusions}: TLS scans suffer from occlusions. Use multiple 
#'     scans for complete coverage.
#'   
#'   \item \strong{Dense Understory}: In presence of very dense understory, 
#'     forest floor segmentation may be less accurate.
#'   
#'   \item \strong{Adjacent Trees}: Trees with overlapping crowns may not be 
#'     completely separated. The \code{eps} parameter influences this separation.
#'   
#'   \item \strong{DBH in Extreme Conditions}: DBH estimation requires at least 
#'     5 points in the 1.25-1.35m range. In highly inclined trees or with aerial 
#'     roots, estimation may be imprecise.
#'   
#'   \item \strong{Non-Arboreal Species}: The algorithm is optimized for trees 
#'     with defined trunk. Shrubs or multi-stem plants require specific 
#'     parameterization.
#' }
#'
#' @section Best Practices:
#' 
#' \enumerate{
#'   \item \strong{Pre-Processing}: 
#'     \itemize{
#'       \item Remove noise and outliers before analysis
#'       \item Accurately register multiple scans
#'       \item Verify coordinates are in meters
#'     }
#'   
#'   \item \strong{Parameter Tuning}: 
#'     \itemize{
#'       \item Start with default parameters
#'       \item Test on small representative subset
#'       \item Adjust iteratively based on visual results
#'     }
#'   
#'   \item \strong{Quality Control}: 
#'     \itemize{
#'       \item Visually inspect results with Shiny interface
#'       \item Compare with field measurements for validation
#'       \item Verify plausibility of extracted metrics
#'     }
#'   
#'   \item \strong{Reporting}: 
#'     \itemize{
#'       \item Document used parameters for reproducibility
#'       \item Save processing logs
#'       \item Maintain versioning of input and output data
#'     }
#' }
#'
#' @references
#' Ferrara R, Di Stefano V, Barone S, Foli L, Puletti N, Chianucci F, 
#' Corona P, Raparelli E, Turco R (2018). 
#' "An automated approach for wood-leaf separation from terrestrial LIDAR 
#' point clouds using the density based clustering algorithm DBSCAN."
#' \emph{Agricultural and Forest Meteorology}, \strong{262}, 434-444.
#' \doi{10.1016/j.agrformet.2018.04.008}
#' 
#' Ferrara R, Arrizza S (2025).
#' "PiC: Pointcloud Interactive Computation for Forest Structure Analysis."
#' Technical Report CNR-IBE.
#' \url{https://hdl.handle.net/20.500.14243/533471}
#'
#' @examples
#' \dontrun{
#' # Example 1: Complete forest analysis
#' library(PiC)
#' 
#' # Load point cloud
#' pointcloud <- read.table("forest_plot.xyz", header = FALSE)
#' colnames(pointcloud) <- c("x", "y", "z")
#' 
#' # Run complete analysis
#' results <- Forest_seg(
#'   a = pointcloud,
#'   filename = "mediterranean_pine",
#'   dimVox = 2,
#'   th = 2,
#'   eps = 2,
#'   mpts = 9,
#'   analyze_canopy = TRUE,
#'   output_path = "results/plot_001/"
#' )
#' 
#' # Read tree metrics
#' tree_data <- read.csv(
#'   "results/plot_001/mediterranean_pine_dim2_th2_tree_report.csv",
#'   sep = ";"
#' )
#' head(tree_data)
#' 
#' # Read plot summary
#' plot_data <- read.csv(
#'   "results/plot_001/mediterranean_pine_dim2_th2_plot_report.csv"
#' )
#' print(plot_data)
#' 
#' # Example 2: Single tree analysis
#' single_tree <- read.table("individual_tree.xyz")
#' 
#' SegOne(
#'   a = single_tree,
#'   filename = "tree_001",
#'   dimVox = 2,
#'   eps = 1,
#'   mpts = 4,
#'   output_path = "results/single_trees/"
#' )
#' 
#' # Example 3: Interactive interface
#' run_PiC()
#' }
#'
#' @author 
#' Roberto Ferrara \email{roberto.ferrara@@cnr.it} (\href{https://orcid.org/0009-0000-3627-6867}{ORCID})
#' 
#' Stefano Arrizza
#'
#' @seealso
#' \itemize{
#'   \item GitHub repository: \url{https://github.com/rupppy/PiC}
#'   \item Package website: \url{https://rupppy.github.io/PiC/}
#'   \item Bug reports: \url{https://github.com/rupppy/PiC/issues}
#' }
#'
#' @md