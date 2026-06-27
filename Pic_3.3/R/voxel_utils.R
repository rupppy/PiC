# ==============================================================================
# VOXEL_UTILS - DEPRECATED
# ==============================================================================
#
# NOTE: This file is deprecated.
# All voxelization utilities have been moved to shared_utils.R
#
# Migration mapping:
# - .voxelize_fast() -> .voxelize_core() in shared_utils.R
# - .voxelize_with_stats() -> .voxelize_with_stats() in shared_utils.R
# - .voxel_centroids() -> removed (use direct calculation)
# - .filter_points_by_voxels() -> removed (use anti-join)
# - .calculate_pca_linearity() -> .calculate_linearity() in shared_utils.R
#
# For backward compatibility, these wrapper functions redirect to shared_utils.R
#
# Author: Roberto Ferrara, CNR-IBE
# ==============================================================================

# Backward compatibility wrapper
.voxelize_fast <- function(points, voxel_size, min_points = 1, return_points = FALSE) {
  .voxelize_core(points, voxel_size, min_points, return_points)
}

# Backward compatibility wrapper
.calculate_pca_linearity <- function(voxels, cluster_ids) {
  .calculate_linearity(voxels, cluster_ids)
}
