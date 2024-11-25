# PiC Documentation

Version 1.0.0

## Table of Contents
1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Core Functions](#core-functions)
4. [Workflows](#workflows)
5. [Advanced Topics](#advanced-topics) 
6. [Troubleshooting](#troubleshooting)

## Introduction
PiC (Point Cloud Interactive Computation) is an R package specialized in point cloud analysis for forestry applications. The package provides comprehensive tools for analyzing forest structure and individual trees through point cloud data.

### Key Features
- Point cloud voxelization
- Forest floor extraction
- Forest components segmentation (wood, above-ground biomass)
- Individual tree segmentation and analysis
- Wood segmentation from voxelized data

### Data Format Requirements
Input data must be organized as a matrix or data frame with at least 3 columns:
- x: X coordinate
- y: Y coordinate
- z: Z coordinate

## Installation
```R

# Install from CRAN
install.packages("PiC")

# Or install development version from GitHub
devtools::install_github("rupppy/PiC")


Core Functions
1. Voxels()

Voxels(a, filename = "output", dimVox = 2)
Converts the point cloud into a voxelized representation, creating a 3D grid where each voxel represents a portion of the space containing points.

Parameters:
a: matrix or data frame with coordinates (x,y,z)
filename: output file name
dimVox: voxel size

Example:
point_cloud <- read.csv("forest_data.csv")
voxelized_cloud <- Voxels(point_cloud, dimVox = 2)

2. Floseg()
Floseg(a, filename = "output", Soil_dim = 30)
Extracts the forest floor from the point cloud, identifying ground points and creating a digital terrain model.
Parameters:
a: matrix or data frame with coordinates (x,y,z)
filename: output file name
Soil_dim: voxel size for ground detection

3. Forest_seg()
Forest_seg(a, filename = "output", dimVox = 2)
Segments the point cloud into three main components:
Forest floor
Wood components
Above Ground Biomass (AGB) excluding wood
Parameters:
a: matrix or data frame with coordinates (x,y,z)
filename: output file name
dimVox: voxel size for segmentation

4. Wood_seg()
Wood_seg(a, filename = "output", dimVox = 2)
Segments wooden components from a pre-voxelized point cloud with the forest floor removed.
Parameters:
a: voxelized point cloud without forest floor
filename: output file name
dimVox: voxel size used in previous voxelization

5. SegOne()
SegOne(a, filename = "output", dimVox = 2)
Segments and analyzes individual trees from the point cloud.
Parameters:
a: matrix or data frame with coordinates (x,y,z) of a single tree
filename: output file name
dimVox: voxel size for tree segmentation
Output:
Returns a list containing:
Segmented components (trunk, branches, foliage)
Structural parameters


Workflows
Basic Forest Analysis
library(PiC)
# 1. Data Import
point_cloud <- read.csv("my_forest.csv")
# 2. Voxelization
vox_data <- Voxels(point_cloud, dimVox = 1)
# 3. Forest Floor Extraction
floor_points <- Floseg(point_cloud)
# 4. Forest Segmentation
forest_segments <- Forest_seg(point_cloud)
# 5. Wood Segmentation
wood_components <- Wood_seg(vox_data)

Single Tree Analysis
# 1. Import single tree data
tree_cloud <- read.csv("single_tree.csv")
# 2. Voxelize tree data
vox_tree <- Voxels(tree_cloud, dimVox = 0.5)
# 3. Perform single tree analysis
tree_segments <- SegOne(vox_tree)
# 4. Extract tree metrics
tree_metrics <- extract_metrics(tree_segments)

Complete Forest Analysis Pipeline
# Start with forest point cloud
forest_cloud <- read.csv("forest_data.csv")
# 1. Forest-level processing
vox_forest <- Voxels(forest_cloud, dimVox = 1)
floor <- Floseg(vox_forest)
forest_segments <- Forest_seg(vox_forest)
# 2. Extract individual trees
individual_trees <- extract_trees(forest_segments)
# 3. Analyze each tree
tree_results <- lapply(individual_trees, function(tree) {
    SegOne(tree, dimVox = 0.5)
})
# 4. Compile results
forest_analysis <- list(
    forest_level = forest_segments,
    individual_trees = tree_results
)

Advanced Topics

Parameter Optimization
Voxel Size Selection:
Smaller voxels (0.02-0.03m): detailed tree structure
Medium voxels (0.03-0.05m): general forest structure
Larger voxels (>0.05m): coarse analysis, better performance

Memory Management
Pre-process large datasets in chunks
Use appropriate data structures
Clean workspace regularly

Integration Tips
Visualization: compatible with rgl package
Data export: supports common formats
Statistical analysis: integrates with R statistical packages

Troubleshooting
Common Issues and Solutions

Memory Errors
# Split large datasets
chunks <- split(point_cloud, (1:nrow(point_cloud) %/% 1000))
results <- lapply(chunks, Forest_seg)
Data Format Verification
check_format <- function(data) {
  if(!all(c("x","y","z") %in% colnames(data))) {
    stop("Missing data: columns x, y, z required")
  }
}

Performance Tips

Optimize voxel size for your specific use case
Use appropriate hardware for large datasets
Consider parallel processing for batch operations

Support
For issues and feature requests, please use the GitHub issue tracker:
https://github.com/rupppy/PiC/issues

