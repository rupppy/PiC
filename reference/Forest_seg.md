# Forest component segmentation, plot and tree metrics

`Forest_seg()` is the main end-to-end routine of the package. It takes a
raw terrestrial or mobile laser scanning (TLS/MLS) point cloud and runs
a complete eight-stage pipeline that reconstructs the forest floor,
recognises the woody structures, measures individual trees, separates
crown from understory, and writes fully classified point clouds together
with per-tree and per-plot reports and a reproducibility log.

All processing is performed on native float coordinates. A single global
shift is applied at the start for numerical precision and reversed once
at the end, so that output coordinates are returned in the original
reference system. The wood and crown analysis relies on three dedicated
algorithms, **LOR** (Ligneous Object Recognition), **DAV** (Directional
Anisotropy of Vegetation) and **GAB** (Geometrical Aggregation of
Biomass), described in the Details section.

## Usage

``` r
Forest_seg(
  a,
  filename = "XXX",
  integer_precision = "mm",
  dtm_coarse_res = 0.5,
  tolerance = 0.4,
  dimVox = 2,
  th = 2,
  eps = 2,
  mpts = 9,
  h_trunk = 3,
  N = 3000,
  w_linear = 0.5,
  Vox_print = FALSE,
  Woodpoints_print = TRUE,
  h_rescue_min = 1,
  h_rescue_max = 5,
  dbh_tolerance = 0.05,
  dbh_max_rmse = 5,
  dbh_min_radius = 0.025,
  dbh_max_radius = 0.5,
  canopy_vox_dim = 0.15,
  canopy_min_density = 500,
  dav_understory_max_start = 1.3,
  dav_height_factor = 0.8,
  dav_median_height_factor = 0.5,
  dav_eps = 2,
  dav_minPts = 4,
  calculate_cbh = TRUE,
  cbh_hex_side = 0.15,
  cbh_min_branch_length = 2,
  cbh_save_points = TRUE,
  output_format = "las",
  output_path = tempdir(),
  generate_reports = TRUE,
  ...
)
```

## Arguments

- a:

  Input point cloud data frame (.xyz format) or file path

- filename:

  Output file prefix (default = "XXX")

- integer_precision:

  Coordinate decimal precision: "mm" (3 decimals) or "cm" (2 decimals)

- dtm_coarse_res:

  Coarse DTM/DSM resolution in m (default = 0.5). Fine resolution is
  automatically set to half. DSM uses same resolution for height
  calculation.

- tolerance:

  Vertical tolerance for floor extraction in m (default = 0.4)

- dimVox:

  Voxel dimension in cm for LOR wood segmentation (default = 2)

- th:

  Minimum points per voxel (default = 2)

- eps:

  DBSCAN epsilon radius in voxel units (default = 2)

- mpts:

  DBSCAN minimum points (default = 9)

- h_trunk:

  Minimum trunk length in m (default = 3)

- N:

  Minimum voxels in wood cluster (default = 3000)

- w_linear:

  Wood cluster quality filter (default = 0.5)

- Vox_print:

  Save voxelization? (default = FALSE)

- Woodpoints_print:

  Save wood points? (default = TRUE)

- h_rescue_min:

  Minimum normalized height (m above DTM) for RESCUE PASS wood recovery
  (default = 1)

- h_rescue_max:

  Maximum normalized height (m above DTM) for RESCUE PASS wood recovery
  (default = 5)

- dbh_tolerance:

  Vertical tolerance for DBH slice in m (default = 0.05)

- dbh_max_rmse:

  Maximum RMSE for DBH fit in cm (default = 5)

- dbh_min_radius:

  Minimum valid DBH radius in m (default = 0.025, i.e. 5 cm DBH)

- dbh_max_radius:

  Maximum valid DBH radius in m (default = 0.5, i.e. 100 cm DBH)

- canopy_vox_dim:

  Voxel size for DAV (Directional Anisotropy of Vegetation) analysis in
  m (default = 0.15)

- canopy_min_density:

  Minimum canopy density in pts/m^3 (default = 500)

- dav_understory_max_start:

  Maximum start height for understory in m (default = 1.3)

- dav_height_factor:

  Internal DAV parameter. Upper fraction of median tree height for
  understory ceiling (default = 0.8, stable value).

- dav_median_height_factor:

  Internal DAV parameter. Fraction of median tree height for mass
  distribution threshold (default = 0.50, stable value).

- dav_eps:

  DBSCAN epsilon for DAV (default = 2)

- dav_minPts:

  DBSCAN minPts for DAV (default = 4)

- calculate_cbh:

  Calculate crown base height? (default = TRUE)

- cbh_hex_side:

  Hexagon edge length in m (default = 0.15)

- cbh_min_branch_length:

  Minimum horizontal extent of foliage in m (default = 2.0)

- cbh_save_points:

  Save CBH points? (default = TRUE)

- output_format:

  Output format: "las" (single classified LAS file, default) or "xyz"
  (separate files per class). Any value other than "xyz" falls back to
  "las".

- output_path:

  Output directory (default = tempdir())

- generate_reports:

  Generate CSV reports? (default = TRUE)

- ...:

  Currently unused; reserved for future extensions

## Value

A named list with the in-memory `tree_metrics` table, the paths of the
files written (`tree_report_file`, `plot_report_file`, `las_file` or, in
XYZ mode, the per-class `crown_file`, `understory_file`, `wood_file`,
`noiseL_file`, `noiseH_file`, `non_valid_trees_file`), the
`parameters_log` path, and `shift_info` (the applied coordinate shift).

## Details

**Processing pipeline (8 stages)**

1.  *Load and shift* - the input (data frame or `.xyz` / `.txt` / `.las`
    / `.laz` file) is coerced to an `x, y, z` table and a global
    coordinate shift is applied.

2.  *Forest floor extraction* - a coarse/fine ground model (DTM)
    separates the forest floor from low vegetation and above-ground
    biomass (AGB).

3.  *LOR (Ligneous Object Recognition)* - woody points (trunks and
    branches) are identified.

4.  *Tree detection and metrics* - tree positions and heights are
    derived from the woody clusters.

5.  *DBH* - diameter at breast height is fitted at multiple heights.

6.  *DAV (Directional Anisotropy of Vegetation)* - foliage is split into
    tree crown and understory.

7.  *GAB (Geometrical Aggregation of Biomass)* - crown base height (CBH)
    is estimated.

8.  *Output* - the classified point cloud, the reports and the log are
    written.

**Point classification scheme**

Every point is assigned an integer class, consistent with the codes
stored in the classified LAS file:

- `2` Forest floor (ground / vegetal soil)

- `3` Understory (low vegetation and shrubs below the crown)

- `4` Wood (trunks and branches)

- `5` Crown / foliage

- `6` Non-valid trees (clusters that failed DBH validation)

- `7` Noise (isolated or non-classifiable points)

**LOR - Ligneous Object Recognition (stage 3)**

AGB points are voxelised (`dimVox`, `th`) and grouped with a
density-based clustering (DBSCAN, parameters `eps` / `mpts`). Clusters
are kept as woody structures when they exceed a minimum size (`N`),
satisfy a linearity/quality filter (`w_linear`) and reach a minimum
trunk length (`h_trunk`). A dedicated rescue pass
(`h_rescue_min`-`h_rescue_max`) recovers woody points in the height band
where trunks and low branches are most often missed.

**DAV - Directional Anisotropy of Vegetation (stage 6)**

The remaining foliage is voxelised at `canopy_vox_dim` and filtered by a
minimum density (`canopy_min_density`). Voxels are clustered (`dav_eps`
/ `dav_minPts`) and split into crown and understory using an adaptive
height threshold derived from the median tree height
(`dav_height_factor`, `dav_median_height_factor`, capped by
`dav_understory_max_start`). Low vegetation from the forest floor stage
is assigned directly to the understory. The voxel classification is then
mapped back to the original points, and crown and understory volumes are
accumulated for the plot report.

**GAB - Geometrical Aggregation of Biomass / CBH (stage 7)**

When `calculate_cbh = TRUE`, crown and wood voxels are merged for
spatial continuity and projected onto a hexagonal tessellation
(`cbh_hex_side`). Each foliage cell is assigned to the nearest tree by
Voronoi partitioning on voxel coordinates, and a breadth-first
connectivity trace (trunk to branches to foliage) identifies the lowest
continuously foliated branch, whose height is reported as the crown base
height. The step runs in parallel across the available cores. A minimum
foliage extent (`cbh_min_branch_length`) guards against spurious
detections.

**DBH measurement (stage 5)**

Diameter is estimated by circle fitting (Pratt/Landau) on horizontal
slices at 1.3 m, with fallbacks at 1.8 m and 2.3 m. A fit is accepted
only within the radius range (`dbh_min_radius`-`dbh_max_radius`) and
below the maximum RMSE (`dbh_max_rmse`); trees failing all heights are
flagged as non-valid (class 6).

**Outputs**

- *Classified point cloud* - by default a single classified LAS file
  (`output_format = "las"`) carrying the class codes above; with
  `output_format = "xyz"` one plain-text file per class is written
  instead.

- *Tree report* (`<filename>_tree_report.csv`) - one row per tree:
  identifier, class, position (X, Y, Z), height, DBH and its RMSE,
  validity flag, and CBH when computed.

- *Plot report* (`<filename>_plot_report.csv`) - stand-level statistics
  including tree density, basal area, crown and understory volume, and
  canopy coverage.

- *CBH diagnostics* - per-tree CBH details, written when
  `calculate_cbh = TRUE`.

- *Parameters log* - a complete record of every parameter (exposed and
  internal), the input summary, the coordinate shift, the
  software/system versions and the timing, for full reproducibility.

Report and log writing is controlled by `generate_reports`.

**Fixed internal parameters (not exposed in the interface)**

- `dtm_fine_res`: automatically set to `dtm_coarse_res / 2`

- `dbh_heights`: fixed at `c(1.3, 1.8, 2.3)` metres

- `dbh_min_points`: fixed at 8 (works well for both Pratt and Landau
  fitting)
