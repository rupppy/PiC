# PiC: Pointcloud Interactive Computation for Forest Structure Analysis

[![Version](https://img.shields.io/badge/version-3.3.1-blue.svg)](https://github.com/rupppy/PiC)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.3-blue.svg)](https://www.r-project.org/)

## Overview

**PiC** (Pointcloud Interactive Computation) is an R package for the processing, segmentation, and analysis of **terrestrial and mobile laser scanning (TLS and MLS)** forest point-cloud data. It provides fast voxel-based processing, classification of the point cloud into forest floor, understory, wood and canopy components, and algorithms for single-tree analysis and stand-level structural characterization.

The methods are designed to handle large and dense point clouds efficiently, supporting applications in forest structure assessment, connectivity analysis, and fire-risk evaluation. In particular, `Forest_seg()` was built for very large datasets: it can process clouds of up to ~600 million points (over 10 GB) on a workstation such as a MacBook Pro M3 with 36 GB of RAM, in reasonable times — a throughput of roughly one million points per second. Input data are provided as `.xyz`, `.txt`, `.las`, or `.laz` point-cloud files.

### Key Features

- **Fast, memory-efficient voxelization** of large point clouds, on native float coordinates with a single global coordinate shift for numerical precision.
- **Full forest segmentation** into forest floor, understory, wood, crown, non-valid trees, and noise.
- **Individual-tree metrics**: position, height, DBH (multi-height, Pratt/Landau), and crown base height (CBH).
- **Plot-level metrics**: tree density, basal area, crown and understory volume, canopy coverage.
- **Dedicated algorithms**: LOR (Ligneous Object Recognition), DAV (Directional Anisotropy of Vegetation), and GAB (Geometrical Aggregation of Biomass).
- **Interactive Shiny interface** for parameter tuning and 3D visualization.
- **Cross-platform** (Windows, macOS, Linux); optional LAS/LAZ support.

## Installation

### From CRAN
```r
install.packages("PiC")
```

### Development version
```r
# install.packages("devtools")
devtools::install_github("rupppy/PiC")
```

**System requirements**

- R ≥ 4.3
- RAM: 8 GB minimum, 16 GB recommended for large clouds (> 50M points)
- Storage: roughly 2–3× the input file size for the outputs

**Optional dependency.** Reading and writing `.las`/`.laz` files relies on the
[`lidR`](https://r-lidar.github.io/lidRbook/) package, listed under `Suggests`.
If `lidR` is not installed, PiC still runs on `.xyz`/`.txt` data and the LAS/LAZ
routines stop with an informative message. Install it with
`install.packages("lidR")`.

## Quick Start

### Forest segmentation
```r
library(PiC)

# Load a point cloud (x, y, z)
data <- data.table::fread("forest_plot.xyz", header = FALSE)
colnames(data)[1:3] <- c("x", "y", "z")

# Full segmentation + tree/plot metrics
results <- Forest_seg(
  a             = data,
  filename      = "plot_001",
  dimVox        = 2,        # voxel size (cm) for LOR wood segmentation
  eps           = 2,        # DBSCAN epsilon (voxel units)
  mpts          = 9,        # DBSCAN minimum points
  calculate_cbh = TRUE,     # estimate crown base height (GAB)
  output_format = "las",    # single classified LAS (default) or "xyz"
  output_path   = "./results"
)

results$tree_metrics      # per-tree table
results$plot_report_file  # path to the plot-level report
results$las_file          # path to the classified LAS file
```

### Single-tree analysis
```r
tree_results <- SegOne(
  a                 = data,
  filename          = "MyTree",
  dimVox            = 2,
  eps               = 1,
  mpts              = 4,
  calculate_metrics = TRUE,
  output_path       = "./output"
)

print(tree_results$metrics)
```

### Metrics from an already-classified cloud
```r
# Recompute tree- and plot-level metrics from a classified LAS
# (classes: ground = 2, understory = 3, wood = 4, crown = 5)
m <- metrics_from_las(
  las_file    = "plot_001_classified.las",
  output_path = "./output"
)
```

### Point-cloud diagnostics (pre-processing)
```r
# Assess density and structure before segmentation
diag <- pic_analyze_cloud(a = data, output_path = "./output")
```

### Interactive Shiny application
```r
run_PiC()
```

## Main Functions

| Function | Purpose |
| --- | --- |
| `Forest_seg()` | End-to-end plot segmentation and tree/plot metrics (8-stage pipeline). |
| `SegOne()` | Single-tree wood–leaf segmentation and metrics; writes a classified LAS. |
| `metrics_from_las()` | Recomputes tree/plot metrics from an already-classified LAS, without re-segmenting. |
| `pic_analyze_cloud()` | Pre-processing diagnostics (point/voxel density, structure), with optional PDF report. |
| `Voxels()` | Standalone voxelization of a point cloud. |
| `Floseg()` | Forest-floor (ground) segmentation. |
| `run_PiC()` | Launches the interactive Shiny interface. |

## Processing Pipeline (`Forest_seg`, 8 stages)

1. **Load & shift** — input is coerced to an `x, y, z` table; a global coordinate shift is applied for numerical precision (reversed for the outputs).
2. **Forest floor extraction** — a coarse/fine ground model (DTM) separates the forest floor from low vegetation and above-ground biomass (AGB).
3. **LOR (Ligneous Object Recognition)** — woody points (trunks and branches) are identified.
4. **Tree detection & metrics** — tree positions and heights are derived from the woody clusters.
5. **DBH** — diameter at breast height fitted at multiple heights.
6. **DAV (Directional Anisotropy of Vegetation)** — foliage is split into crown and understory.
7. **GAB (Geometrical Aggregation of Biomass)** — crown base height (CBH) is estimated.
8. **Output** — classified point cloud, reports, and parameters log are written.

### Point classification scheme

Each point is assigned an integer class, consistent with the codes stored in the classified LAS file:

| Code | Component |
| --- | --- |
| `2` | Forest floor (ground / vegetal soil) |
| `3` | Understory (low vegetation and shrubs below the crown) |
| `4` | Wood (trunks and branches) |
| `5` | Crown / foliage |
| `6` | Non-valid trees (clusters that failed DBH validation) |
| `7` | Noise (isolated or non-classifiable points) |

## Algorithm Details

### LOR — Ligneous Object Recognition

AGB points are voxelized (`dimVox`, `th`) and grouped with density-based clustering (DBSCAN, parameters `eps`/`mpts`). Clusters are retained as woody structures when they exceed a minimum size (`N`), satisfy a linearity/quality filter (`w_linear`), and reach a minimum trunk length (`h_trunk`). A dedicated **rescue pass** (`h_rescue_min`–`h_rescue_max`) recovers woody points in the height band where trunks and low branches are most often missed.

### DAV — Directional Anisotropy of Vegetation

The remaining foliage is voxelized at `canopy_vox_dim` and filtered by a minimum density (`canopy_min_density`). Voxels are clustered (`dav_eps`/`dav_minPts`) and split into crown and understory using an adaptive height threshold derived from the median tree height (`dav_height_factor`, `dav_median_height_factor`, capped by `dav_understory_max_start`). Low vegetation from the forest-floor stage is assigned directly to the understory.

### GAB — Geometrical Aggregation of Biomass (CBH)

When `calculate_cbh = TRUE`, crown and wood voxels are merged for spatial continuity and projected onto a hexagonal tessellation (`cbh_hex_side`). Each foliage cell is assigned to the nearest tree by Voronoi partitioning on voxel coordinates, and a breadth-first connectivity trace (trunk → branches → foliage) identifies the lowest continuously foliated branch, whose height is reported as the crown base height. The step runs in parallel across the available cores. A minimum foliage extent (`cbh_min_branch_length`) guards against spurious detections.

### DBH calculation (multi-height, dual method)

At each priority height (1.3 m, with fallbacks at 1.8 m and 2.3 m) both **Pratt** and **Landau** circle-fitting methods are computed; when both are valid, the fit with the lower RMSE is kept. This handles occluded trunks, irregular sections and scan artefacts. A fit is accepted only within the radius range (`dbh_min_radius`–`dbh_max_radius`, i.e. ≈ 5–100 cm diameter) and below the maximum RMSE (`dbh_max_rmse`, default 5 cm). Trees failing all heights are flagged as non-valid (class 6).

## Parameters

Parameters are grouped by the forest component they affect. Defaults are those of `Forest_seg()` 3.3.1.

### 1. Input / Output
- **`a`** — input point cloud: a file path (`.xyz`/`.txt`/`.las`/`.laz`) or a data.frame/data.table with at least 3 columns (x, y, z). *Required.*
- **`filename`** — output file prefix (default `"XXX"`); e.g. `"plot_001"` → `plot_001_tree_report.csv`.
- **`integer_precision`** — coordinate rounding precision, `"mm"` (3 decimals, high-resolution TLS) or `"cm"` (2 decimals, lighter for MLS/UAV). Default `"mm"`.
- **`output_format`** — `"las"` (single classified LAS file, default) or `"xyz"` (one plain-text file per class).
- **`output_path`** — output directory (default `tempdir()`); created if missing.
- **`generate_reports`** — write the CSV reports and the parameters log (default `TRUE`).
- **`Vox_print`** / **`Woodpoints_print`** — export the voxel grid (default `FALSE`) / the wood points (default `TRUE`).

### 2. Forest floor (DTM-based extraction)
- **`dtm_coarse_res`** — coarse DTM grid size in m (default `0.5`); the fine resolution is set internally to half of this.
- **`tolerance`** — vertical threshold above the DTM in m (default `0.4`); points within this band are classified as forest floor.

### 3. Wood detection (LOR)
- **`dimVox`** — voxel size in cm (default `2`). TLS: 2 cm; sparser data: larger.
- **`th`** — minimum points per voxel (default `2`).
- **`eps`** — DBSCAN radius in voxel units (default `2`); metric distance ≈ `eps × dimVox`.
- **`mpts`** — DBSCAN minimum neighbours (default `9`).
- **`h_trunk`** — minimum trunk length in m (default `3`); shorter clusters are discarded (removes shrubs).
- **`N`** — minimum voxels per wood cluster (default `3000`).
- **`w_linear`** — linearity/quality filter, 0–1 (default `0.5`); higher = stricter cylindrical shape.
- **`h_rescue_min`** / **`h_rescue_max`** — normalized height band (m above DTM) for the rescue pass (defaults `1` / `5`).

### 4. DBH (multi-height diameter)
- **`dbh_tolerance`** — vertical slice half-thickness in m (default `0.05`, i.e. ±5 cm).
- **`dbh_max_rmse`** — maximum fitting RMSE in cm (default `5`).
- **`dbh_min_radius`** / **`dbh_max_radius`** — valid radius range in m (defaults `0.025` / `0.5`, i.e. ≈ 5–100 cm DBH).
- *Internal (not exposed):* `dbh_heights = c(1.3, 1.8, 2.3)` m, `dbh_min_points = 8`.

### 5. Crown–understory separation (DAV)
- **`canopy_vox_dim`** — DAV voxel size in m (default `0.15`).
- **`canopy_min_density`** — minimum canopy density in pts/m³ (default `500`; shared with the CBH step).
- **`dav_understory_max_start`** — maximum start height for the understory in m (default `1.3`).
- **`dav_height_factor`** / **`dav_median_height_factor`** — internal DAV thresholds based on the median tree height (defaults `0.8` / `0.50`).
- **`dav_eps`** / **`dav_minPts`** — DBSCAN parameters for DAV (defaults `2` / `4`).

### 6. Crown base height (GAB / CBH)
- **`calculate_cbh`** — enable CBH estimation (default `TRUE`).
- **`cbh_hex_side`** — hexagon edge length in m (default `0.15`).
- **`cbh_min_branch_length`** — minimum horizontal foliage extent in m (default `2.0`).
- **`cbh_save_points`** — save CBH points (default `TRUE`).
- *Internal (auto-calculated):* minimum cluster cells and minimum points per cell (from `cbh_hex_side`, `canopy_vox_dim`, `canopy_min_density`); vertical gap tolerance and core count.

## Output Files

### Classified point cloud
- With `output_format = "las"` (default): a single `*_classified.las` carrying the class codes above.
- With `output_format = "xyz"`: one plain-text file per class — forest floor, wood, crown, understory, low/high noise, and non-valid trees.

### Reports (when `generate_reports = TRUE`)

**`*_tree_report.csv`** — one row per tree:

| Column | Units | Description |
| --- | --- | --- |
| Tree_n | – | Tree identifier |
| cls | – | Internal cluster id |
| X, Y, Z | m | Position (original coordinates) |
| Height | m | Total height above the DTM |
| DBH (cm) | cm | Diameter at the selected height |
| RMSE (cm) | cm | Circle-fit quality |
| valid_tree | – | `TRUE`/`FALSE` (DBH validation) |
| CBH | m | Crown base height (when computed) |

**`*_plot_report.csv`** — stand-level summary in long format (`metric`; `value`), including: `area_of_interest_m2`, `coverage_area_m2`, `coverage_percentage`, `crown_volume_m3`, `understory_volume_m3`, `min/max/mean/median/sd_height_m`, `mean/median_dbh_cm`, `mean/median_cbh_m`, `tree_count`, `valid_tree_count`, `trees_per_hectare`, `basal_area_m2_ha`.

```r
plot_summary <- data.table::fread(results$plot_report_file, sep = ";")
plot_summary[metric == "basal_area_m2_ha", value]
```

### Other outputs
- **CBH diagnostics** — per-tree CBH details (when `calculate_cbh = TRUE`). A CBH of `-999` flags a failed estimate.
- **Parameters log** — complete record of every parameter (exposed and internal), input summary, coordinate shift, software/system versions and timing, for reproducibility.

## Usage Examples

### Example 1 — Standard forest inventory
```r
library(PiC)

data <- data.table::fread("mature_stand.xyz")
colnames(data)[1:3] <- c("x", "y", "z")

results <- Forest_seg(
  a                 = data,
  filename          = "inventory_2025",
  output_path       = "./results",
  integer_precision = "mm",
  dimVox            = 2, th = 2, eps = 2, mpts = 9,
  calculate_cbh     = TRUE,
  output_format     = "las"
)

trees        <- results$tree_metrics
plot_summary <- data.table::fread(results$plot_report_file, sep = ";")

cat(sprintf("Trees: %d\n", nrow(trees)))
cat(sprintf("Mean DBH: %.1f cm\n", mean(trees[valid_tree == TRUE, `DBH (cm)`], na.rm = TRUE)))
cat(sprintf("Basal area: %.2f m2/ha\n", plot_summary[metric == "basal_area_m2_ha", value]))
```

### Example 2 — Fire-risk assessment (ladder fuels)
```r
results <- Forest_seg(
  a                     = "aleppo_pine.xyz",
  filename              = "fire_risk",
  calculate_cbh         = TRUE,    # critical for ladder-fuel detection
  cbh_min_branch_length = 1.2,     # conservative foliage extent
  cbh_save_points       = TRUE,
  output_format         = "las"
)

trees     <- results$tree_metrics
valid_cbh <- trees[CBH > 0 & CBH < 900, CBH]

cat(sprintf("Mean CBH: %.2f m\n", mean(valid_cbh)))
cat(sprintf("Trees with CBH < 2 m: %d  [ladder-fuel risk]\n", sum(valid_cbh < 2)))
```

### Example 3 — Large dataset / speed
```r
results <- Forest_seg(
  a                 = "large_plot.xyz",
  filename          = "optimized",
  integer_precision = "cm",   # coarser coordinate rounding
  dimVox            = 3,       # larger wood voxels
  canopy_vox_dim    = 0.20,    # coarser DAV resolution
  calculate_cbh     = FALSE    # skip the most intensive stage
)
```

## Parameter Tuning Guidelines

### By scanner type

| Scanner | `integer_precision` | `dimVox` | `dav_eps` |
| --- | --- | --- | --- |
| High-res TLS | mm | 2 | 2 |
| Standard TLS | mm | 2–3 | 2 |
| Mobile LS (MLS) | cm | 3–4 | 3 |
| UAV-LiDAR | cm | 4–5 | 3–4 |

### By forest type

| Type | `N` (voxels) | `h_trunk` (m) | `w_linear` |
| --- | --- | --- | --- |
| Young regeneration | 2000 | 1.5 | 0.4 |
| Mature conifers | 3000–5000 | 3.0 | 0.5 |
| Broadleaf | 3000 | 2.5 | 0.45 |
| Sparse / degraded | 2000 | 2.0 | 0.4 |

### By analysis goal

**Fast inventory** (DBH + height only):
```r
calculate_cbh = FALSE
generate_reports = TRUE
```

**Fire-risk assessment**:
```r
calculate_cbh   = TRUE       # ladder-fuel detection
cbh_save_points = TRUE       # export crown/understory layers
output_format   = "las"
```

**Research (full detail)**:
```r
calculate_cbh    = TRUE
cbh_save_points  = TRUE
generate_reports = TRUE
Vox_print        = TRUE      # diagnostic voxel grid
```

## Troubleshooting

### No trees detected (`tree_count = 0`)
```r
# Relax wood segmentation
dimVox   = 2     # finer voxels
eps      = 3     # larger clustering radius
N        = 2000  # lower cluster-size threshold
w_linear = 0.4   # accept less linear clusters
```

### Many invalid DBH (`valid_tree = FALSE`)
```r
# Inspect the fit-quality distribution
hist(results$tree_metrics[["RMSE (cm)"]])

# If systematically high, relax (cautiously):
dbh_max_rmse = 7
# If clusters are noisy, tighten the wood filters (N, h_trunk, w_linear).
```

### Many CBH failures (`CBH = -999`)
```r
cbh_min_branch_length = 1.0   # accept shorter foliated branches
canopy_min_density    = 300   # lower density requirement
# Verify the DAV crown is well populated (output_format = "xyz" exports it).
```

### Memory errors
```r
integer_precision = "cm"      # coarser coordinate rounding
dimVox            = 3
canopy_vox_dim    = 0.20
# Or subsample very large inputs:
data_sub <- data[sample(.N, 50e6)]
```

## Coordinate Precision & Performance

PiC processes the cloud on native float coordinates and applies a single global shift for numerical stability. The `integer_precision` argument controls the rounding granularity used throughout the pipeline: `"mm"` keeps 3 decimals (recommended for high-resolution TLS), while `"cm"` keeps 2 decimals, which lightens memory use and voxel bookkeeping on large or coarser MLS/UAV clouds. The CBH stage (GAB) runs in parallel across the available cores.

## What's New in 3.3.1

- `lidR` moved from `Imports` to `Suggests`: LAS/LAZ reading and writing are now **optional** and guarded; the package installs and works on `.xyz`/`.txt` data without `lidR`.
- The maintainer repository for `lidR` is declared via `Additional_repositories`.
- Substantially expanded `Forest_seg()` documentation (8-stage pipeline, classification scheme, LOR/DAV/GAB, DBH, outputs, return value).

See `NEWS.md` for the full history, including the 3.3 features (`metrics_from_las()`, `pic_analyze_cloud()`, crown/understory volumes, the revised GAB/CBH connectivity and the LOR rescue pass).

## Citation

**Package**
```
Ferrara, R., & Arrizza, S. (2025). PiC: Pointcloud Interactive Computation
for Forest Structure Analysis. R package version 3.3.1.
https://hdl.handle.net/20.500.14243/533471
```

**Methodological reference**
```
Ferrara, R., Virdis, S.G.P., Ventura, A., Ghisu, T., Duce, P., & Pellizzaro, G. (2018).
An automated approach for wood-leaf separation from terrestrial LIDAR point clouds
using the density based clustering algorithm DBSCAN.
Agricultural and Forest Meteorology, 262, 434-444.
https://doi.org/10.1016/j.agrformet.2018.04.008
```

**Circle-fitting algorithm**
```
Pratt, V. (1987). Direct least-squares fitting of algebraic surfaces.
ACM SIGGRAPH Computer Graphics, 21(4), 145-152.
```

## Authors

**Roberto Ferrara** (Maintainer) — *CNR-IBE (National Research Council of Italy – Institute for BioEconomy)*
Email: roberto.ferrara@cnr.it
ORCID: [0009-0000-3627-6867](https://orcid.org/0009-0000-3627-6867)

**Stefano Arrizza** (Contributor) — *CNR-IBE*
Email: stefano.arrizza@cnr.it
ORCID: [0009-0009-2290-3650](https://orcid.org/0009-0009-2290-3650)

## License

Licensed under the GNU General Public License v3.0 or later.
See [the GPL-3 license](https://www.gnu.org/licenses/gpl-3.0.en.html) for details.

## Links

- **GitHub repository**: https://github.com/rupppy/PiC
- **Issue tracker**: https://github.com/rupppy/PiC/issues
- **CNR-IBE**: https://www.ibe.cnr.it
- **Documentation**: run `?PiC` (or `?Forest_seg`) in the R console after installation

## Acknowledgments

Development supported by CNR-IBE research on Mediterranean forest structure and fire-risk assessment using terrestrial and mobile laser scanning.
