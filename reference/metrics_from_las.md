# Calculate tree and plot metrics from a classified LAS file

Recalculates tree-level and plot-level metrics starting from a
classified LAS file. Intended for use after manual correction of an
automatic classification produced by
[`Forest_seg`](https://rupppy.github.io/PiC/reference/Forest_seg.md).

Wood points (class 4) are re-clustered into individual trees using
DBSCAN with fixed, permissive parameters (5 cm voxels, eps = 2, minPts =
5). These parameters are intentionally loose because the classification
is already trusted; DBSCAN here only separates spatially distinct
trunks.

Tree height is computed as the difference between the tree base
elevation (minimum z of the wood cluster) and the maximum z of any point
in the full cloud within a 0.5 m XY buffer around the tree base,
consistent with the
[`Forest_seg`](https://rupppy.github.io/PiC/reference/Forest_seg.md)
approach.

Crown Base Height (CBH) is calculated with the GAB (Geometrical
Aggregation of Biomass) Voronoi method, identical to
[`Forest_seg`](https://rupppy.github.io/PiC/reference/Forest_seg.md).

Output CSV files use the same column layout as `Forest_seg` reports.

## Usage

``` r
metrics_from_las(
  las_file,
  output_path = NULL,
  filename = NULL,
  dbh_tolerance = 0.05,
  dbh_max_rmse = 5,
  dbh_min_radius = 0.025,
  dbh_max_radius = 0.8,
  calculate_cbh = TRUE,
  cbh_hex_side = 0.15,
  cbh_min_branch_length = 2,
  canopy_vox_dim = 0.15,
  canopy_min_density = 100,
  generate_reports = TRUE
)
```

## Arguments

- las_file:

  Character. Path to the classified LAS file.

- output_path:

  Character. Directory for output files. Default: same directory as
  `las_file`.

- filename:

  Character. Prefix for output file names. Default: derived from
  `las_file` (stripping `_classified`).

- dbh_tolerance:

  Vertical half-width of the DBH slice in metres (default = 0.05).

- dbh_max_rmse:

  Maximum acceptable RMSE for circle fit in cm (default = 5).

- dbh_min_radius:

  Minimum valid stem radius in metres (default = 0.025).

- dbh_max_radius:

  Maximum valid stem radius in metres (default = 0.8).

- calculate_cbh:

  Logical. If TRUE, calculate Crown Base Height using the GAB method
  (default = TRUE). Requires crown points (class 5).

- cbh_hex_side:

  Hexagonal cell side length in metres for GAB (default = 0.15).

- cbh_min_branch_length:

  Minimum horizontal foliage extent in metres to consider a branch valid
  for CBH (default = 2.0).

- canopy_vox_dim:

  Voxel size in metres for crown and wood voxelisation used in GAB CBH
  (default = 0.15).

- canopy_min_density:

  Minimum point density in pts/m\\^3\\ used to derive the minimum points
  per hex cell for GAB (default = 100).

- generate_reports:

  Logical. If TRUE (default), saves `<filename>_tree_report.csv` and
  `<filename>_plot_report.csv`.

## Value

Invisibly returns a named list:

- tree_metrics:

  data.table with one row per detected tree.

- plot_stats:

  data.frame with plot-level aggregate metrics.

- tree_report:

  Path to the tree-level CSV, or NULL.

- plot_report:

  Path to the plot-level CSV, or NULL.

## LAS classification codes expected

- 2 = Ground / forest floor (convex hull for plot area)

- 3 = Understory

- 4 = Wood (re-clustered into individual trees)

- 5 = Crown / foliage (used for height buffer and CBH)

- 6 = Non-valid trees (ignored)

- 7 = Noise (ignored)

## See also

[`Forest_seg`](https://rupppy.github.io/PiC/reference/Forest_seg.md),
[`SegOne`](https://rupppy.github.io/PiC/reference/SegOne.md)
