
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
devtools::install_github("author/PiC")
```

## Main Functions

### Forest Segmentation

``` r
Forest_seg7(a, filename="output", dimVox = 2)
```

### Voxelization

``` r
Voxels(a, filename="output", dimVox = 2)
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
