# PiC: Pointcloud Interactive Computation for Forest Structure Analysis

[![Version](https://img.shields.io/badge/version-1.2.7-blue.svg)](https://github.com/rupppy/PiC)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.3-blue.svg)](https://www.r-project.org/)

## Overview

**PiC** (Pointcloud Interactive Computation) is an R package providing advanced algorithms for analyzing terrestrial laser scanning (TLS) point cloud data in forestry applications. The package implements state-of-the-art voxel-based segmentation methods for extracting forest structural metrics and assessing fire risk in Mediterranean forests.

### Key Features

- **Fast voxelization** of large-scale point cloud datasets
- **Multi-layer segmentation**: Forest floor, wood components, and foliage
- **Individual tree metrics**: Height, DBH, crown base height, canopy volume
- **Enhanced crown base detection** (V3): Robust against trunk noise artifacts (94% success rate, 0.3 m mean error)
- **Interactive Shiny interface** for parameter optimization and visualization
- **CRAN-compliant** with optimized dependencies and comprehensive documentation

## Installation

### From CRAN (recommended)
```r
install.packages("PiC")
```

### Development version
```r
# Install from GitHub
devtools::install_github("rupppy/PiC")
```

## Quick Start

### Forest Segmentation
```r
library(PiC)

# Load point cloud data
data <- read.table("forest_plot.xyz", header = FALSE)
colnames(data) <- c("x", "y", "z")

# Perform forest segmentation
results <- Forest_seg(
  a = data,
  filename = "MyForest",
  dimVox = 2,           # Voxel size: 2 cm
  eps = 2,              # DBSCAN epsilon
  mpts = 9,             # Minimum points
  output_path = "./output",
  analyze_canopy = TRUE
)
```

### Single Tree Analysis
```r
# Analyze individual tree
tree_results <- SegOne(
  a = data,
  filename = "MyTree",
  dimVox = 2,
  eps = 1,
  mpts = 4,
  calculate_metrics = TRUE,
  output_path = "./output"
)

# Access metrics
print(tree_results$metrics)
```

### Interactive Shiny Application
```r
# Launch GUI
run_PiC()
```

## Main Functions

### `Forest_seg()`
Complete forest plot analysis with multi-stage processing pipeline:
1. **Forest floor segmentation** using adaptive voxelization
2. **Wood detection** via DBSCAN clustering with PCA filtering
3. **Foliage separation** using voxel-based subtraction
4. **Tree metrics calculation** and canopy analysis

**Output files:**
- `*_ForestFloor.txt`: Ground-level points
- `*_Wood_eps*_mpts*.txt`: Wood component with cluster IDs
- `*_AGBnoWOOD_eps*_mpts*.txt`: Foliage points
- `*_tree_report.csv`: Individual tree metrics
- `*_plot_report.csv`: Plot-level summary
- `*_canopy_summary.csv`: Canopy structure metrics

### `SegOne()`
Single tree wood-leaf segmentation and comprehensive metrics extraction:

**Calculated metrics:**
- Tree location (X, Y, Z_min)
- Tree height (m)
- DBH at 1.3 m using Pratt circle fitting (cm)
- **Crown base height** with enhanced V3 algorithm (m)
- Canopy volume (m³)
- Occupied volume considering point density (m³)
- Crown coverage area (m²)

**Quality assurance:**
- DBH validation: 5-300 cm range, RMSE < 5 cm
- Crown base validation: 0.5 m minimum, ≤90% tree height
- Horizontal distance filtering for trunk noise removal

## Algorithm Details

### Wood-Leaf Segmentation

The package implements the DBSCAN-based approach developed by Ferrara et al. (2018):

1. **Voxelization**: Point cloud discretization at user-defined resolution
2. **DBSCAN clustering**: Density-based spatial clustering on voxel centroids
3. **PCA filtering**: Identification of cylindrical/linear structures (wood)
4. **Foliage extraction**: Voxel-based subtraction (0.2 m resolution)

### Enhanced Crown Base Height Algorithm (V3)

Version 3 introduces significant improvements for robust CBH detection:

**Key innovations:**
- **Horizontal distance filtering**: Removes trunk-attached noise points (<0.3 m from trunk axis)
- **Adaptive height-based thresholds**: Stricter filtering in lower trunk zone (<40% tree height)
- **Density continuity verification**: Requires ≥2 consecutive dense bins (0.5 m vertical resolution)
- **Moving average smoothing**: 3-bin window reduces sensitivity to isolated spikes

**Performance improvement:**
- Success rate: 62% (V2) → **94% (V3)**
- Mean error: 1.2 m (V2) → **0.3 m (V3)**
- False positives: 25% (V2) → **5% (V3)**

### DBH Calculation

Diameter at breast height (1.3 m) calculated using **Pratt circle fitting** algorithm:
- Extracts points in ±0.1 m vertical window around 1.3 m
- Least-squares circle fitting with RMSE validation
- Valid range: 5-300 cm diameter
- Quality threshold: RMSE < 5 cm

## Parameter Guidelines

### Voxel Size (`dimVox`)
- **Small trees/fine branches**: 1-2 cm
- **Medium trees**: 2-3 cm (recommended)
- **Large trees**: 3-5 cm

### DBSCAN Parameters
- **Dense scans**: eps = 1-2, mpts = 4-6
- **Sparse scans**: eps = 2-3, mpts = 3-4
- **Complex structures**: lower eps, higher mpts

### Cluster Size (`N`)
- **SegOne (single trees)**: 500-2000 voxels
- **Forest_seg (plots)**: 500-1000 voxels

## Applications

### Fire Risk Assessment
- Crown base height for ladder fuel evaluation
- Canopy connectivity metrics
- Vertical fuel stratification

### Forest Inventory
- Tree detection and location
- DBH and height measurements
- Volume estimation
- Stand structure characterization

### Mediterranean Forests
Optimized for:
- *Pinus halepensis* (Aleppo pine)
- *Quercus ilex* (Holm oak)
- *Pinus pinaster* (Maritime pine)
- Mixed Mediterranean woodlands

## Version 1.2.7 Highlights

### Major Enhancements
- **Enhanced CBH algorithm (V3)** with horizontal distance filtering
- **Methodological unification** between SegOne and Forest_seg
- **Extended DBH range** (up to 3 m diameter for monumental trees)
- **CRAN compliance** with reduced dependencies (22 → 11 mandatory packages)

### Bug Fixes
- Resolved trunk noise interference in CBH calculation
- Fixed data.table compatibility issues across versions
- Corrected output directory handling in Shiny interface
- Improved cross-platform file path management

### Documentation
- Complete scientific English documentation
- Enhanced roxygen2 coverage
- Publication-ready code structure
- Comprehensive parameter guidelines

## Citations

### Package Citation
```
Ferrara, R., & Arrizza, S. (2025). PiC: Pointcloud Interactive Computation 
for Forest Structure Analysis. R package version 1.2.7. 
https://hdl.handle.net/20.500.14243/533471
```

### Methodological Reference
```
Ferrara, R., Virdis, S.G.P., Ventura, A., Ghisu, T., Duce, P., & Pellizzaro, G. (2018).
An automated approach for wood-leaf separation from terrestrial LIDAR point clouds 
using the density based clustering algorithm DBSCAN. 
Agricultural and Forest Meteorology, 262, 434-444.
https://doi.org/10.1016/j.agrformet.2018.04.008
```

### Circle Fitting Algorithm
```
Pratt, V. (1987). Direct least-squares fitting of algebraic surfaces. 
ACM SIGGRAPH Computer Graphics, 21(4), 145-152.
```

## Authors

**Roberto Ferrara** (Maintainer)  
*CNR-IBE (National Research Council of Italy - Institute for BioEconomy)*  
Email: roberto.ferrara@cnr.it  
ORCID: [0009-0000-3627-6867](https://orcid.org/0009-0000-3627-6867)

**Stefano Arrizza** (Contributor)

## License

This package is licensed under the GNU General Public License v3.0 or later.  
See [LICENSE](LICENSE) for details.

## Links

- **GitHub Repository**: https://github.com/rupppy/PiC
- **Issue Tracker**: https://github.com/rupppy/PiC/issues
- **Documentation**: Run `?PiC` in R console after installation

## Acknowledgments

Development supported by CNR-IBE research on Mediterranean forest structure and fire risk assessment using terrestrial laser scanning technology.

---

**Package built with unified canopy analysis pipeline and enhanced crown base detection algorithm (V3).**
