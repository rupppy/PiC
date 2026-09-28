# Point cloud diagnostic analysis

Analyzes point cloud quality and structure with voxel density analysis.
Generates density maps and distribution curves. Results are always
returned as plots (for Shiny) and optionally exported as PDF.

## Usage

``` r
pic_analyze_cloud(
  points,
  voxel_sizes = 0.1,
  generate_pdf = TRUE,
  pdf_output = NULL,
  output_path = tempdir()
)
```

## Arguments

- points:

  Data frame with columns x, y, z (or will be coerced)

- voxel_sizes:

  Numeric vector of voxel sizes in meters for analysis

- generate_pdf:

  Logical, whether to generate PDF report (default = TRUE)

- pdf_output:

  Character, PDF filename (if NULL and generate_pdf=TRUE,
  auto-generated)

- output_path:

  Character, directory for PDF output (default = tempdir())

## Value

List containing: - summary: Basic statistics (n_points, extent, area) -
density_analysis: Density per m^2 statistics and raster -
voxel_analysis: Multi-resolution voxel statistics - plots: Named list of
ggplot2 objects (density_raster, density_curve) - text_output: Character
vector with formatted statistics - pdf_file: Path to PDF if generated,
NULL otherwise
