# PiC: Pointcloud Interactive Computation for Forest Structure Analysis

[![Version](https://img.shields.io/badge/version-3.3-blue.svg)](https://github.com/rupppy/PiC)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.3-blue.svg)](https://www.r-project.org/)

## Overview

**PiC** (Pointcloud Interactive Computation) is an R package for automated forest structure analysis from ground-based LiDAR point clouds. It processes 3D laser scanning data to extract individual tree metrics and stand-level characteristics using a comprehensive 8-stage pipeline.

### Core Features

- **Memory-efficient processing**: Integer coordinates with mm/cm precision (~50% RAM reduction)
- **Complete segmentation**: Terrain, wood, foliage, crown, understory layers
- **Individual tree metrics**: Position, height, DBH with multi-height comparison, crown base
- **Plot-level statistics**: Density, basal area, canopy volume, coverage
- **Interactive interface**: Shiny GUI for parameter tuning and 3D visualization
- **Cross-platform**: Windows, macOS, Linux support

---
  
  ## Installation
  ```r
# Install dependencies
install.packages(c("data.table", "dbscan", "terra", "conicfit", "tictoc"))

# Install PiC
devtools::install_github("rupppy/PiC")
```

**System Requirements**:
  - R ≥ 4.3
- RAM: 8 GB minimum, 16 GB recommended (>50M points)
- Storage: ~2-3× point cloud file size for outputs

---
  
  ## Quick Start
  
  ### Basic Analysis
  ```r
library(PiC)

# Load point cloud
data <- data.table::fread("forest_plot.xyz", header = FALSE)
colnames(data)[1:3] <- c("x", "y", "z")

# Run analysis
results <- Forest_seg(
  a = data,
  filename = "plot_001",
  output_path = "./results"
)

# View results
print(results$tree_metrics)     # Individual trees
print(results$canopy_metrics)   # Plot-level stats
```

### Interactive Interface
```r
# Launch Shiny GUI
run_PiC()
```

---
  
  ## Processing Pipeline
  
  Forest_seg implements an 8-stage workflow:
  ```
1. DTM Floor Extraction    → Separate ground from vegetation
2. Wood Segmentation        → Detect trunks and branches  
3. Tree Position           → Identify tree locations
4. Heights & DBH           → Measure dimensions
5. Foliage Separation      → Remove wood from vegetation
6. DAV Classification      → Separate crown from understory
7. CBH Detection           → Find crown base height
8. Canopy Analysis         → Calculate volume and coverage
```

Each stage processes integer coordinates for memory efficiency, with final outputs converted back to metric units (meters, centimeters).

---
  
  ## Parameters Guide
  
  Parameters are organized by the forest component they affect:
  
  ### 1. Input/Output
  
  **`a`** - Input point cloud  
- Type: File path (string) or data.frame/data.table
- Format: XYZ coordinates (3+ columns)
- Required: Yes

**`filename`** - Output file prefix  
- Type: String
- Default: `"XXX"`
- Example: `"plot_001"` → generates `plot_001_tree_report.csv`, etc.

**`integer_precision`** - Coordinate precision  
- Type: `"mm"` or `"cm"`
- Default: `"mm"`
- **mm**: 1 millimeter precision, best for high-res TLS
- **cm**: 1 centimeter precision, efficient for ALS/UAV-LiDAR
- Impact: 50% memory reduction vs float coordinates

**`output_path`** - Output directory  
- Type: String
- Default: `tempdir()`
- Creates directory if it doesn't exist

---

### 2. Terrain (DTM-based floor extraction)

Purpose: Extract ground surface for height normalization

**`dtm_coarse_res`** - Coarse DTM grid size (meters)  
- Default: `0.5`
- Range: 0.1 - 5.0
- Larger = faster processing, suitable for flat terrain

**`dtm_fine_res`** - Fine DTM interpolation (meters)  
- Default: `0.1`
- Range: 0.01 - 1.0
- Finer = more accurate floor detection on slopes

**`tolerance`** - Vertical threshold above DTM (meters)  
- Default: `0.4`
- Range: 0.1 - 1.0
- Points within this height above DTM classified as floor

**`outlier_k`** - Outlier removal strength (standard deviations)  
- Default: `1`
- Range: 1 - 5
- Removes DTM cells >k×σ from local 5×5 neighborhood median
- 1σ = conservative (remove only extreme outliers)

---

### 3. Wood Detection

Purpose: Segment trunk and branch structures

**`dimVox`** - Voxel size for wood analysis (centimeters)  
- Default: `2`
- Range: 1 - 20
- Spatial resolution of 3D grid
- Smaller = finer detail but slower
- **TLS**: 2cm | **ALS**: 5-10cm

**`th`** - Minimum points per voxel  
- Default: `2`
- Range: 1 - 50
- Occupancy threshold for density filtering
- Voxels with fewer points discarded as noise

**`eps`** - DBSCAN search radius (voxel units)  
- Default: `2`
- Range: 0.1 - 10.0
- Distance to connect voxels into clusters
- Metric distance = eps × dimVox (e.g., 2 voxels × 2cm = 4cm)

**`mpts`** - DBSCAN minimum neighbors  
- Default: `9`
- Range: 1 - 50
- Core point threshold for clustering
- Higher = denser wood structures only

**`h_trunk`** - Minimum trunk length (meters)  
- Default: `3`
- Range: 1 - 20
- Vertical extent filter
- Clusters shorter than this discarded (removes shrubs)

**`N`** - Minimum voxels per cluster  
- Default: `10000`
- Range: 1000 - 1,000,000
- Size filter for tree detection
- Lower for small trees, higher to exclude artifacts

**`w_linear`** - PCA linearity threshold  
- Default: `0.50`
- Range: 0.1 - 1.0
- Geometric descriptor of cluster shape
- Linearity = (λ₁ - λ₂) / λ₁ from eigenvalues
- High (>0.7) = strict cylindrical shape (trunks only)
- Low (<0.3) = includes planar/spherical structures

---

### 4. DBHx - Multi-Height Diameter Measurement

Purpose: Robust DBH calculation with priority heights and dual methods

**NEW in v1.8.3**: At each height, both **Pratt** and **Landau** circle-fitting methods are computed. If both valid, the method with minimum RMSE is selected. This handles occluded trunks, irregular sections, and scan artifacts.

**`dbh_heights`** - Priority heights for measurement (meters)  
- Default: `c(1.3, 1.8, 2.3)`
- Heights tested in order: standard (1.3m) → alternative (1.8m) → fallback (2.3m)
- First valid result at any height is used

**`dbh_tolerance`** - Vertical slice thickness (meters)  
- Default: `0.05`
- Range: 0.01 - 0.2
- Points within ±tolerance of target height used for fitting
- ±5cm = standard dendrometric practice

**`dbh_max_rmse`** - Maximum fitting error (centimeters)  
- Default: `5`
- Range: 0.5 - 50
- Root mean square error threshold for quality validation
- Fits with higher RMSE marked as invalid

**`dbh_min_radius`** - Minimum valid radius (meters)  
- Default: `0.025` (5cm DBH)
- Range: 0.01 - 0.1
- Lower bound for trunk size
- 0.025m suitable for saplings

**`dbh_max_radius`** - Maximum valid radius (meters)  
- Default: `1.5` (3m DBH)
- Range: 0.5 - 5.0
- Upper bound for trunk size
- Filters out aberrant fits

**`dbh_min_points`** - Minimum points for fitting  
- Default: `5`
- Range: 3 - 50
- Data sufficiency threshold
- Fewer points = unreliable fit

**Output Columns** in `tree_report.csv`:
- `DBH_cm`: Selected diameter
- `DBH_height_m`: Measurement height used (1.3, 1.8, or 2.3)
- `DBH_method`: Circle-fitting method ("Pratt" or "Landau")
- `DBH_RMSE_cm`: Fit quality indicator
- `DBH_valid`: Quality flag (TRUE/FALSE)

---

### 5. Crown-Understory Separation (DAV)

Purpose: Vertical stratification using density-based clustering

**`canopy_vox_dim`** - DAV voxel size (meters)  
- Default: `0.10`
- Range: 0.05 - 0.5
- Fine resolution for crown detection
- Smaller = more detail, slower

**`dav_min_points`** - Minimum points per voxel  
- Default: `10`
- Range: 1 - 100
- Density threshold for valid voxels
- Filters noise and sparse points

**`dav_min_crown_height`** - Crown thickness estimation parameter (meters)  
- Default: `1.0`
- Range: 0.5 - 5.0
- Used to calculate adaptive understory threshold
- Threshold = min_tree_height - (0.35 × min_tree_height)
- Constrained between 2m and 8m crown thickness

**`dav_min_crown_base`** - Minimum crown attachment (meters)  
- Default: `1.5`
- Range: 0.5 - 10.0
- Expected lowest crown base height
- Vegetation rooted below this may be classified as understory
- Safety constraint for adaptive threshold

**`dav_eps`** - DBSCAN radius (voxel units)  
- Default: `2`
- Range: 0.5 - 10.0
- Horizontal clustering distance
- Larger = merge adjacent crowns

**`dav_minPts`** - DBSCAN minimum neighbors  
- Default: `6`
- Range: 3 - 50
- Core point threshold
- Lower = detect smaller crowns

**Output Files** (if `cbh_save_points = TRUE`):
- `*_crown.xyz`: Crown points (x, y, z, z_norm, cluster_id)
- `*_understory.xyz`: Understory points
- `*_noise.xyz`: DBSCAN noise points (NEW v1.8.3)

---

### 6. Crown Base Height (CBH) - GAB Method

Purpose: Identify lowest live crown attachment point

**`calculate_cbh`** - Enable CBH calculation  
- Type: Boolean
- Default: `TRUE`

**`cbh_layer_height`** - Vertical slice thickness (meters)  
- Default: `0.30`
- Range: 0.05 - 1.0
- Height of horizontal layers for connectivity analysis

**`cbh_band_width`** - Radial shell width (meters)  
- Default: `0.20`
- Range: 0.05 - 1.0
- Width of cylindrical shells around trunk

**`cbh_n_sectors`** - Angular divisions  
- Default: `8`
- Range: 4 - 36
- Azimuthal sectors (8 = cardinal + intercardinal directions)

**`cbh_min_branch_length`** - Minimum branch projection (meters)  
- Default: `1.00`
- Range: 0.2 - 5.0
- Minimum vertical extent of connected crown
- Validates branch structure

**`cbh_n_min_points`** - Minimum points per cell  
- Default: `250`
- Range: 10 - 5000
- Data sufficiency in layer-band-sector cell

**`cbh_save_points`** - Export classified point clouds  
- Type: Boolean
- Default: `TRUE`
- Saves crown/understory/noise XYZ files

**Output Values**:
- **Valid**: 0.5m ≤ CBH ≤ 0.90×tree_height
- **-999**: Failed (too low, understory interference)
- **999**: Failed (too high, anomalous)

---

### 7. Canopy Structure

Purpose: Stand-level canopy metrics (volume, coverage)

**`analyze_canopy`** - Enable canopy analysis  
- Type: Boolean
- Default: `TRUE`

**`min_canopy_height`** - Lower boundary (meters)  
- Default: `1.5`
- Range: 0 - 10
- Only voxels above this height included
- Removes understory and surface fuels

**Output Metrics** (added to `plot_report.csv`):
- `canopy_volume_m3`: Total canopy volume
- `coverage_area_m2`: Horizontal coverage
- `canopy_mean_density_pts_m3`: Average point density

---

### 8. Workflow Control

**`generate_reports`** - Generate CSV metrics  
- Type: Boolean
- Default: `TRUE`
- Creates `tree_report.csv` and `plot_report.csv`
- Disabling = segmentation files only (no metrics)

**`Vox_print`** - Export voxelized point cloud  
- Type: Boolean
- Default: `FALSE`
- Saves intermediate voxel grid (diagnostics)

---

## Output Files

### Always Generated

**Point Clouds**:
- `{prefix}_floor.xyz`: Ground points (x, y, z)
- `{prefix}_wood.xyz`: Wood points (x, y, z, cluster_id, Tree_n)

**Reports** (if `generate_reports = TRUE`):
- `{prefix}_tree_report.csv`: Per-tree metrics

  | Column | Units | Description |
  |--------|-------|-------------|
  | Tree_n | - | Sequential tree ID |
  | X, Y, Z | m | Position (original coordinates) |
  | Height | m | Total height from DTM |
  | DBH_cm | cm | Diameter at selected height |
  | DBH_height_m | m | Measurement height (1.3, 1.8, or 2.3) |
  | DBH_method | - | "Pratt" or "Landau" |
  | DBH_RMSE_cm | cm | Fit quality |
  | DBH_valid | - | TRUE/FALSE |
  | CBH | m | Crown base height (if calculated) |

- `{prefix}_plot_report.csv`: Plot-level summary

  | Metric | Units | Description |
  |--------|-------|-------------|
  | tree_count | n | Total trees detected |
  | validated_tree_count | n | Trees with valid DBH |
  | mean_height_m | m | Average height |
  | mean_dbh_cm | cm | Average DBH |
  | plot_area_m2 | m² | Estimated area (convex hull) |
  | basal_area_m2_ha | m²/ha | Basal area per hectare |
  | trees_per_hectare | n/ha | Stand density |
  | canopy_volume_m3 | m³ | Total canopy volume |
  | coverage_area_m2 | m² | Canopy horizontal coverage |

### Conditional Outputs

**DAV Classification** (if `cbh_save_points = TRUE`):
- `{prefix}_crown.xyz`: Crown layer
- `{prefix}_understory.xyz`: Understory layer
- `{prefix}_noise.xyz`: DBSCAN noise points

**Parameter Log** (if `generate_reports = TRUE`):
- `{prefix}_parameters_{timestamp}.txt`: Complete parameter record

---

## Usage Examples

### Example 1: Standard Forest Inventory
```r
library(PiC)

# Load TLS data
data <- data.table::fread("mature_stand.xyz")

# Run with defaults
results <- Forest_seg(
  a = data,
  filename = "inventory_2025",
  output_path = "./results",
  
  # Standard settings
  integer_precision = "mm",
  dimVox = 2,
  th = 2,
  eps = 2,
  mpts = 9,
  
  # Full metrics
  calculate_cbh = TRUE,
  analyze_canopy = TRUE
)

# Extract metrics
trees <- results$tree_metrics
plot_summary <- fread(results$plot_report_file, sep = ";")

# Key inventory parameters
cat(sprintf("Stand Inventory:\n"))
cat(sprintf("  Trees: %d\n", nrow(trees)))
cat(sprintf("  Mean DBH: %.1f cm\n", mean(trees[DBH_valid==TRUE, DBH_cm])))
cat(sprintf("  Mean Height: %.1f m\n", mean(trees$Height)))
cat(sprintf("  Basal Area: %.2f m²/ha\n", 
    plot_summary[metric=="basal_area_m2_ha", value]))
```

### Example 2: Fire Risk Assessment
```r
# Mediterranean pine forest - fire vulnerability

results <- Forest_seg(
  a = "aleppo_pine.xyz",
  filename = "fire_risk",
  
  # Fire-specific parameters
  dav_min_crown_base = 2.0,        # Elevated crowns typical
  calculate_cbh = TRUE,             # Critical for ladder fuels
  cbh_min_branch_length = 1.2,     # Conservative detection
  
  # Canopy characterization
  analyze_canopy = TRUE,
  min_canopy_height = 2.0          # Surface fuel cutoff
)

# Fire risk indicators
trees <- results$tree_metrics
valid_cbh <- trees[CBH > 0 & CBH < 900, CBH]

cat(sprintf("Fire Risk Metrics:\n"))
cat(sprintf("  Mean CBH: %.2f m\n", mean(valid_cbh)))
cat(sprintf("  Min CBH: %.2f m [vulnerability]\n", min(valid_cbh)))
cat(sprintf("  Trees with CBH < 2m: %d [ladder fuel risk]\n", 
    sum(valid_cbh < 2.0)))
```

### Example 3: DBHx Multi-Height Analysis
```r
# Complex stand with trunk occlusions

results <- Forest_seg(
  a = "dense_forest.xyz",
  filename = "dbhx_test",
  
  # DBHx configuration
  dbh_heights = c(1.3, 1.8, 2.3),  # Priority heights
  dbh_tolerance = 0.05,             # ±5cm slice
  dbh_max_rmse = 5,                 # 5cm quality threshold
  dbh_min_radius = 0.025,           # Min 5cm DBH
  dbh_max_radius = 1.5              # Max 3m DBH
)

# Analyze DBHx performance
trees <- results$tree_metrics

# Distribution by height
table(trees[DBH_valid==TRUE, .(DBH_height_m, DBH_method)])
#       DBH_method
# DBH_height_m Landau Pratt
#          1.3      5    42
#          1.8      3     8
#          2.3      1     3

# Quality assessment
summary(trees[DBH_valid==TRUE, DBH_RMSE_cm])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.82    1.95    2.73    2.81    3.54    4.98
```

### Example 4: Large Dataset Optimization
```r
# 200M points - memory-constrained processing

results <- Forest_seg(
  a = "large_plot.xyz",
  filename = "optimized",
  
  # Memory reduction
  integer_precision = "cm",        # 50% RAM reduction
  
  # Coarser voxels
  dimVox = 3,                       # Larger wood voxels
  canopy_vox_dim = 0.20,           # Coarser DAV resolution
  
  # Skip intensive analyses
  calculate_cbh = FALSE,            # Reduce processing time
  analyze_canopy = FALSE
)
```

---

## Parameter Tuning Guidelines

### By Scanner Type

| Scanner | `integer_precision` | `dimVox` | `dav_eps` |
|---------|---------------------|----------|-----------|
| High-res TLS | mm | 2 | 2 |
| Standard TLS | mm | 2-3 | 2 |
| Mobile LS | cm | 3-4 | 3 |
| UAV-LiDAR | cm | 4-5 | 3-4 |
| ALS | cm | 5-10 | 4-5 |

### By Forest Type

| Type | `N` (voxels) | `h_trunk` (m) | `w_linear` |
|------|--------------|---------------|------------|
| Young regeneration | 3000 | 1.5 | 0.4 |
| Mature conifers | 10000 | 3.0 | 0.5 |
| Broadleaf | 8000 | 2.5 | 0.45 |
| Sparse/degraded | 5000 | 2.0 | 0.4 |

### By Analysis Goal

**Fast inventory** (DBH + height only):
```r
generate_reports = TRUE
calculate_cbh = FALSE
analyze_canopy = FALSE
```

**Fire risk assessment**:
```r
calculate_cbh = TRUE              # Ladder fuel detection
cbh_save_points = TRUE            # Export fuel layers
analyze_canopy = TRUE             # Canopy bulk density
dav_min_crown_base = 2.0         # Elevated crowns
```

**Research (full detail)**:
```r
generate_reports = TRUE
calculate_cbh = TRUE
analyze_canopy = TRUE
cbh_save_points = TRUE
Vox_print = TRUE                  # Diagnostic voxels
```

---

## Performance

### Memory Usage

**Coordinate storage**:
- Float (double): 24 bytes/point
- Integer (mm/cm): 12 bytes/point → **50% reduction**

**Benchmarks** (Apple M3 Pro, 36 GB RAM):

| Points | Precision | RAM | Time |
|--------|-----------|-----|------|
| 50M | mm | 8.4 GB | 39 sec |
| 50M | cm | 4.8 GB | 39 sec |
| 200M | cm | 24 GB | 3.2 min |

### Optimization Tips

**Large datasets (>100M points)**:
- Use `integer_precision = "cm"`
- Increase `dimVox` to 3-4
- Coarsen `canopy_vox_dim` to 0.20
- Disable `Vox_print`

**High-precision needs**:
- Use `integer_precision = "mm"`
- `dimVox = 2` for fine detail
- Strict `dbh_max_rmse = 3`

**Speed priority**:
- Larger voxels (`dimVox = 4`)
- Higher `eps` (3-4)
- Skip `calculate_cbh`

---

## Troubleshooting

### No Trees Detected

**Symptoms**: `tree_count = 0` in plot_report

**Solutions**:
```r
# Relax wood segmentation
dimVox = 2           # Reduce voxel size
eps = 3              # Increase clustering radius
N = 3000             # Lower cluster size threshold
w_linear = 0.4       # Accept less linear shapes
```

### Many Invalid DBH

**Symptoms**: `DBH_valid = FALSE` for most trees

**Solutions**:
```r
# Check RMSE distribution
hist(results$tree_metrics$DBH_RMSE_cm)

# If systematically high:
dbh_max_rmse = 7     # Relax threshold (use cautiously)

# If radius issues:
# → Increase wood filters (N, h_trunk, w_linear)
```

### CBH Failures (many -999)

**Symptoms**: Most trees have `CBH = -999`

**Solutions**:
```r
# Relax GAB parameters
dav_min_crown_base = 1.0          # Lower threshold
cbh_min_branch_length = 0.8       # Shorter branches OK
cbh_n_min_points = 100            # Lower density requirement

# Check DAV classification
crown <- fread("*_crown.xyz")
nrow(crown)  # Should be >10000 points
```

### Memory Errors

**Symptoms**: `cannot allocate vector of size...`

**Solutions**:
```r
# Reduce precision
integer_precision = "cm"

# Coarsen analysis
dimVox = 3
canopy_vox_dim = 0.20

# Subsample input
data_sub <- data[sample(.N, 50e6)]  # 50M points
```

---

## Citation
```bibtex
@software{ferrara2025pic,
  author = {Ferrara, Roberto and Arrizza, Stefano},
  title = {{PiC: Pointcloud Interactive Computation}},
  year = {2025},
  version = {1.8.3},
  url = {https://github.com/rupppy/PiC}
}
```

**Methodological reference**:
```bibtex
@article{ferrara2018automated,
  title = {An automated approach for wood-leaf separation from terrestrial LIDAR point clouds using DBSCAN},
  author = {Ferrara, Roberto and others},
  journal = {Agricultural and Forest Meteorology},
  volume = {262},
  pages = {434--444},
  year = {2018},
  doi = {10.1016/j.agrformet.2018.04.008}
}
```

---

## Authors

**Roberto Ferrara** (Maintainer)  
CNR-IBE (National Research Council of Italy)  
📧 roberto.ferrara@cnr.it  
🔗 [ORCID: 0009-0000-3627-6867](https://orcid.org/0009-0000-3627-6867)

**Stefano Arrizza** (Contributor)  
CNR-IBE  
📧 stefano.arrizza@cnr.it  
🔗 [ORCID: 0009-0009-2290-3650](https://orcid.org/0009-0009-2290-3650)

---

## License

GNU General Public License v3.0  
See [LICENSE](https://www.gnu.org/licenses/gpl-3.0.en.html) for details

---

## Links

- **Repository**: https://github.com/rupppy/PiC
- **Issues**: https://github.com/rupppy/PiC/issues
- **CNR-IBE**: https://www.ibe.cnr.it

---

**Built with memory-optimized integer coordinates and DBHx multi-height comparison.**  
*Robust forest structure analysis from LiDAR point clouds.*