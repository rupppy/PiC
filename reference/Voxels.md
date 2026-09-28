# Voxelize Point Cloud (Optimized)

Transforms a 3D point cloud into voxel representation with density
filtering. Version 3.0 uses optimized data.table operations and native
float coordinates for maximum performance and consistency with
Forest_seg pipeline.

\*\*Key Features:\*\*

- Pure data.table operations (10-20x faster than dplyr)

- Native coordinate system (no confusing shifts)

- Memory efficient (no intermediate copies)

- Progress reporting for large datasets

- Consistent with Forest_seg v2.7 architecture

\*\*Performance:\*\*

- 1M points: ~0.5 seconds

- 10M points: ~3 seconds

- 100M points: ~30 seconds

## Arguments

- a:

  Input point cloud. Can be:

  - File path (.xyz, .txt)

  - Data frame with x,y,z columns

  - Matrix with 3+ columns

- filename:

  Output file prefix (default = "XXX")

- dimVox:

  Voxel dimension in centimeters (default = 2). Typical range: 1-5 cm
  for forest applications

- th:

  Minimum points per voxel for retention (default = 2). Higher values
  filter more noise but may remove valid sparse regions. Suggested: 1-2
  for high density scans, 2-4 for sparse scans

- output_path:

  Output directory (default = tempdir())

- coordinate_precision:

  Decimal places for coordinate rounding. "mm" (3 decimals) or "cm" (2
  decimals). Default = "mm"

## Value

Invisibly returns list with:

- voxels: data.table with columns u, v, w, N (voxel indices + point
  count)

- output_file: Path to saved voxel file

- stats: Voxelization statistics

## Details

\*\*Voxelization Process:\*\*

1\. Input validation and coercion to data.table 2. Calculate voxel
indices (u, v, w) from coordinates 3. Aggregate points per voxel (fast
data.table grouping) 4. Filter by minimum point threshold 5. Export to
file

\*\*Coordinate System:\*\*

Unlike v2.0, this version preserves native coordinates: - No forced
positive transformation - No confusing min/max shifts - Voxel indices
calculated directly: floor(coordinate / voxel_size) + 1 - Consistent
with Forest_seg coordinate handling

\*\*Memory Usage:\*\*

Approximate memory requirements: - Input points: ~40 bytes/point
(x,y,z + overhead) - Voxelized: ~20 bytes/voxel + indices - Peak usage
during aggregation: ~2x input size

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
Voxels(
  a = "forest_scan.xyz",
  filename = "plot_A",
  dimVox = 2,
  th = 2,
  output_path = "results/"
)

# High precision voxelization
result <- Voxels(
  a = point_cloud,
  dimVox = 1,  # 1 cm voxels
  th = 3,      # Aggressive noise filtering
  coordinate_precision = "mm"
)

# Access statistics
print(result$stats)
} # }
```
