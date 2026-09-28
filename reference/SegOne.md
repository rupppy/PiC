# Single Tree Wood-Leaf Segmentation and Comprehensive Metrics Calculation

Performs wood-leaf segmentation and calculates comprehensive structural
metrics for individual trees from terrestrial laser scanning (TLS) point
cloud data.

Version 4.2 uses shared_utils.R functions for code reuse and consistency
with the Forest_seg pipeline.

The analysis follows a four-stage processing pipeline:

1.  Voxelization and wood component identification using DBSCAN
    clustering

2.  Foliage separation through voxel-based subtraction

3.  Tree structural metrics calculation (height, DBH, crown base)

4.  Canopy volume quantification using density-weighted voxel analysis

## Usage

``` r
SegOne(a, filename = "Elab_single_tree", dimVox = 2, th = 2, 
       eps = 2, mpts = 6, N = 1000, R = 30, output_path = tempdir(),
       calculate_metrics = TRUE, voxel_size_canopy = 0.1, 
       coverage_method = "linear")
```

## Arguments

- a:

  Input point cloud data. Can be either: (1) a data frame with x, y, z
  coordinates, or (2) a file path to a .txt or .xyz file containing
  point cloud data

- filename:

  Character string specifying the output file prefix (default:
  "Elab_single_tree")

- dimVox:

  Numeric value specifying voxel dimension in centimeters for wood
  segmentation. Typical range: 1-5 cm. Smaller values increase
  computational cost but improve spatial resolution (default: 2)

- th:

  Integer specifying minimum number of points required to generate a
  voxel. Used to filter noise and low-density regions (default: 2)

- eps:

  Numeric value specifying the epsilon neighborhood radius for DBSCAN
  clustering in voxel units. Determines spatial connectivity of wood
  clusters (default: 2)

- mpts:

  Integer specifying minimum number of points required in epsilon
  neighborhood for a voxel to be considered a core point in DBSCAN
  algorithm (default: 6)

- N:

  Integer specifying minimum number of voxels required to form a valid
  wood cluster. Filters small non-wood clusters (default: 1000)

- R:

  Numeric threshold for cluster shape parameter (standard deviation
  proportion of variance from PCA). Used to identify cylindrical/linear
  wood structures. Higher values are more restrictive (default: 30)

- output_path:

  Character string specifying directory path for output files. If
  directory does not exist, it will be created (default: tempdir())

- calculate_metrics:

  Logical flag indicating whether to calculate comprehensive tree
  metrics. If FALSE, only wood-leaf segmentation is performed (default:
  TRUE)

- voxel_size_canopy:

  Numeric value specifying voxel size in meters for canopy volume
  calculation. Typical range: 0.05-0.2 m (default: 0.1)

- coverage_method:

  Character string specifying method for calculating coverage degree in
  canopy volume analysis. Options: "linear", "mean_normalized",
  "exponential", "threshold", "mediterranean" (default: "linear")

## Value

Invisibly returns a named list containing:

- las_file:

  Character string with full path to classified LAS file (class 4 =
  wood, class 5 = foliage)

- metrics_file:

  Character string with full path to metrics CSV file (NULL if
  calculate_metrics = FALSE)

- metrics:

  data.table containing calculated tree structural metrics (NULL if
  calculate_metrics = FALSE)

## Details

\## Processing Pipeline

\*\*Stage 1: Wood Segmentation\*\*

Wood components are identified through a multi-step process:

1.  Point cloud voxelization at resolution specified by `dimVox`

2.  DBSCAN clustering applied to voxel centroids using `eps` and `mpts`

3.  Principal Component Analysis (PCA) filtering to identify cylindrical
    structures

4.  Retention of clusters with shape parameter R exceeding threshold
    (cylindrical wood)

The PCA-based filtering exploits the geometric properties of tree stems
and branches, which exhibit high first principal component values due to
their elongated structure.

\*\*Stage 2: Foliage Separation\*\*

Foliage points are extracted using voxel-based subtraction:

1.  Both wood and total point cloud are voxelized at 0.2 m resolution

2.  Wood-occupied voxels are identified

3.  Non-wood voxels are retained and mapped back to original points

This approach ensures complete spatial separation between wood and
foliage components.

\*\*Stage 3: Structural Metrics Calculation\*\*

When `calculate_metrics = TRUE`, the following metrics are computed:

- \*\*Tree Base Location (X, Y, Z_min)\*\*: Coordinates of lowest wood
  point

- \*\*Tree Height\*\*: Vertical distance from base to highest point
  within 1 m buffer

- \*\*DBH (Diameter at Breast Height)\*\*: Calculated at 1.3 m using
  Pratt circle fitting algorithm with +/- 5 cm tolerance. Valid range:
  5-300 cm. DBH value is always reported in CSV output. DBH_RMSE_cm
  column provides fit quality (max 5 cm for validation). DBH_valido flag
  indicates whether measurement meets quality standards.

- \*\*Crown Base Height\*\*: Detected using vertical density analysis to
  filter noise, followed by gap analysis. Uses 0.5 m bins with minimum 3
  points per bin. Valid range: 0.5 m to 90

\*\*Stage 4: Canopy Volume Quantification\*\*

Canopy volume metrics are calculated using density-weighted voxel
analysis:

1.  Foliage points voxelized at resolution `voxel_size_canopy`

2.  Point density calculated per voxel

3.  Coverage degree computed using specified `coverage_method`

4.  Two volume metrics calculated:

    - \*\*Canopy Volume\*\*: Total occupied voxel volume (m^3)

    - \*\*Occupied Volume\*\*: Density-weighted volume accounting for
      point distribution

5.  Coverage area computed from ground projection of occupied voxels
    (mq)

\## Improvements in Version 2.0

\*\*Enhanced Crown Base Calculation:\*\*

- Vertical density analysis filters noise points (isolated points near
  trunk)

- Uses 0.5 m vertical bins with minimum 3 points per bin threshold

- Combines density filtering with gap analysis for robust detection

- Validates results with multiple criteria

\*\*Improved DBH Validation:\*\*

- Updated maximum diameter to 3.0 m (300 cm) for large/monumental trees

- RMSE quality check (max 5 cm) used to flag poor circle fits

- DBH value always reported (even if RMSE threshold exceeded)

- DBH_RMSE_cm column provides fit quality indicator

- DBH_valido flag indicates whether measurement meets all quality
  standards

- Enhanced diagnostic messages showing fit quality

\## Coverage Degree Methods

The `coverage_method` parameter determines how point density is
translated to coverage degree:

- \*\*linear\*\*: Linear normalization by column maximum: \\CD = N /
  N\_{max}\\

- \*\*mean_normalized\*\*: Normalization by mean density: \\CD = N /
  \bar{N}\\

- \*\*exponential\*\*: Exponential saturation: \\CD = 1 - exp(-N /
  \bar{N})\\

- \*\*threshold\*\*: Binary classification at 50th percentile

- \*\*mediterranean\*\*: Power-law scaling optimized for sparse
  Mediterranean canopies: \\CD = (N / N\_{max})^{0.7}\\

For single trees, "linear" is recommended as it provides intuitive
interpretation of point density relative to maximum observed density.

\## Quality Assurance

The function implements several validation checks:

- DBH validation: radius 2.5-150 cm, minimum 5 points, RMSE reported (\<
  5 cm for valid flag)

- Crown base validation: density filtering, minimum 0.5 m, maximum 90

- Point count validation: sufficient points in measurement zones

- Comprehensive error handling with diagnostic messages

\## Output Files

Two files are generated in `output_path`:

1.  \*\*Classified LAS\*\*: `<filename>_eps<eps>_mpts<mpts>.las`

    - Class 4 = wood points, Class 5 = foliage points

    - Single file replaces the two legacy .txt outputs

2.  \*\*Metrics\*\*: `<filename>_metrics.csv` (if calculate_metrics =
    TRUE)

    - Contains all calculated structural metrics

    - DBH_cm: Always populated when calculation succeeds

    - DBH_RMSE_cm: Fit quality indicator (lower is better, \<5 cm is
      valid)

    - Semicolon-delimited format

## Note

Version 2.0 addresses two critical issues identified in field testing:

1.  Crown base calculation now robust against noise points near trunk

2.  DBH validation extended to 3 m diameter with quality checks

This implementation is fully consistent with the Forest_seg approach,
ensuring methodological coherence across the PiC package.

## Parameter Selection Guidelines

\*\*Voxel Size (dimVox)\*\*:

- Small trees or fine branches: 1-2 cm

- Medium trees: 2-3 cm

- Large trees: 3-5 cm

\*\*DBSCAN Parameters (eps, mpts)\*\*:

- Densely scanned trees: eps = 1-2, mpts = 4-6

- Sparsely scanned trees: eps = 2-3, mpts = 3-4

- Complex branch structures: lower eps, higher mpts

\*\*Cluster Size (N)\*\*:

- Small trees: N = 500-1000

- Medium trees: N = 1000-2000

- Large trees: N = 2000-5000

## References

Ferrara, R., Virdis, S.G.P., Ventura, A., Ghisu, T., Duce, P., &
Pellizzaro, G. (2018). An automated approach for wood-leaf separation
from terrestrial LIDAR point clouds using the density based clustering
algorithm DBSCAN. Agricultural and Forest Meteorology, 262, 434-444.
[doi:10.1016/j.agrformet.2018.04.008](https://doi.org/10.1016/j.agrformet.2018.04.008)

Pratt, V. (1987). Direct least-squares fitting of algebraic surfaces.
ACM SIGGRAPH Computer Graphics, 21(4), 145-152.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with default parameters
result <- SegOne(
  a = "single_tree.xyz",
  filename = "tree_analysis",
  output_path = "~/results"
)

# View calculated metrics
print(result$metrics)

# Advanced usage for large tree
result <- SegOne(
  a = my_point_cloud,
  filename = "large_oak",
  dimVox = 3,           # Larger voxels for large tree
  eps = 2,              # Increased connectivity
  mpts = 6,             # More stringent clustering
  N = 2000,             # Larger minimum cluster size
  R = 35,               # Stricter cylindrical filter
  output_path = "~/tree_metrics",
  voxel_size_canopy = 0.15,
  coverage_method = "linear"
)
} # }
```
