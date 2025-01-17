
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PiC: Pointcloud Interactive Computation <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

## Overview

PiC provides advanced algorithms for analysing and segmenting 3D point clouds obtained from terrestrial LiDAR systems in forest contexts, both at the individual tree and plot level.

## Installation

From Github

``` r
# Install devtools if not already installed
install.packages("devtools")

# Install PiC
devtools::install_github("rupppy/PiC")
```
Via GUI in zip or tar.gz

``` r
# Installation with graphical user interface
install.packages(file.choose(),repos = NULL, type = "source")
# type = "win.binary" for .zip files on Windows

# Installation of the file .zip (Windows)
install.packages("C:/path/PiC..zip", repos = NULL, 
type = "win.binary")

# Installation of the file .tar.gz (Unix/Linux or source packages)
install.packages("C:/path/PiC.tar.gz",repos = NULL, type = "source")
```


## Main Functions

PiC's two main commands are 'Forest_seg', which applies to entire forest plots, and 'SegOne', which performs the same operations on the 3D cloud of a single tree. Other commands (Voxels, Floseg, Wood_seg) allow to perform the individual steps of voxelization, forest soil identification and separation from above ground biomass (AGB), and tree group identification.

### Forest Segmentation

High-level function which performs all segmentation and clustering operations on forest plot point clouds.

``` r
Forest_seg(inputfile, filename="output", dimVox = 2, th = 2, eps = 2, mpts = 6, h_tree = 1, Soil_dim = 30, N = 500, R = 30, Vox_print = FALSE/TRUE, WoddVox_print = FALSE/TRUE)
```
### Single Tree Segmentation

The function analyzes 3D clouds of individual trees by performing the entire segmentation process.

``` r
SegOne(inputfile, filename="output", dimVox = 2, th = 2, eps = 2, mpts = 6,  N = 500, R = 30)
```

### Voxelization

The function performs the voxelization of a point cloud.

``` r
Voxels(inputfile, filename="output", dimVox = 2, th = 2)
```

### Soil separation

The function analyzes a voxel point cloud and allows to identify and separate the forest floor from the rest of the overlying vegetation (AGB). 

``` r
Floseg(inputfile, filename="output", Soil_dim = 30, th = 20)
```
### Wood-leaf separation in AGB file without soil

Applied to a point cloud (txt or xyz) representing the AboveGroundBiomass of a forest formation without the ground layer, performs clustering of points and identification/separation of woody clusters from non-woody components.

``` r
Wood_seg(inputfile, filename="output", Soil_dim = 30, th = 20)
```

## Example
Suppose we want to perform segmentation on a point cloud representing a forest plot. The file is named "ForestPlot_example".
- Let's import the file into the working environment
- Let's run the "Forest_seg" command.
``` r
library(PiC)

ForestPlot_example <- read.table("path/ForestPlot_example.xyz", quote="\"", comment.char="")

Forest_seg(ForestPlot_example, filename="ForestPlot output name", dimVox = 2, th = 2, eps = 2, mpts = 6, h_tree = 1, Soil_dim = 30, N = 500, R = 30, Vox_print = FALSE/TRUE, WoddVox_print = FALSE/TRUE)

```

If we do not indicate the value of the parameters, by default they will be set to:
dimVox = 2
th = 2 
eps = 1 
mpts = 4 
h_tree = 1 
Soil_dim = 30 
N = 1000, 
R = 30, 
Vox_print = FALSE, 
WoddVox_print = TRUE

## Documentation

- [Getting Started with PiC](articles/pic-intro.html)
- [Function Reference](reference/index.html)

## Authors

- Roberto Ferrara (<roberto.ferrara@cnr.it>)
- Stefano Arrizza (<stefano.arrizza@cnr.it>)

## License

GPL (\>= 3)
