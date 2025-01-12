
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PiC: Pointcloud Interactive Computation <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

## Overview

PiC provides advanced algorithms for analyzing POINTCLOUD data in
forestry applications.

## Installation

``` r
# Install devtools if not already installed
install.packages("devtools")

# Install PiC
devtools::install_github("rupppy/PiC")
```

## Main Functions

### Forest Segmentation

``` r
Forest_seg(a, filename="output", dimVox = 2, th = 2, eps = 2, mpts = 6, h_tree = 1, Soil_dim = 30, N = 500, R = 30, Vox_print = FALSE/TRUE, WoddVox_print = FALSE/TRUE)
```
### Single Tree Segmentation

``` r
SegOne(a, filename="output", dimVox = 2, th = 2, eps = 2, mpts = 6,  N = 500, R = 30)
```

### Voxelization

``` r
Voxels(a, filename="output", dimVox = 2, th = 2)
```

### Soil separation

``` r
Floseg(a, filename="output", Soil_dim = 30, th = 20)
```
### Wood-leaf separation in AGB file without soil

``` r
Floseg(a, filename="output", Soil_dim = 30, th = 20)
```

## Example

``` r
library(PiC)
# Add example code here
```

## Documentation

- [Getting Started with PiC](articles/pic-intro.html)
- [Function Reference](reference/index.html)

## Authors

- Roberto Ferrara (<roberto.ferrara@cnr.it>)
- Stefano Arrizza (<stefano.arrizza@cnr.it>)

## License

GPL (\>= 3)
