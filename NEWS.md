# PiC 3.3.1

## Changes

* Moved `lidR` from `Imports` to `Suggests`: LAS/LAZ reading and writing are now
  optional and guarded with `requireNamespace()`, so the package installs and
  works (with `.xyz`/`.txt` data) even when `lidR` is unavailable.
* Declared the maintainer repository for `lidR` via `Additional_repositories`.

## Documentation

* Substantially expanded the `Forest_seg()` documentation: full 8-stage
  pipeline, point classification scheme (forest floor, understory, wood, crown,
  non-valid trees, noise), the LOR, DAV and GAB algorithms, DBH measurement, the
  LAS/XYZ outputs, the tree/plot reports and the parameters log, and the return
  value. Also noted that `Forest_seg()` is designed for very large clouds (up to
  ~600M points / >10 GB on a 36 GB workstation, ~1M points/s).

# PiC 3.3

## New features

* New function `metrics_from_las()`: recomputes tree- and plot-level metrics
  (DBH, height, crown base height, plot statistics) from an **already
  classified** point cloud (LAS with ASPRS classes ground = 2, understory = 3,
  wood = 4, crown = 5), without repeating the full `Forest_seg()` segmentation.
* New function `pic_analyze_cloud()`: pre-processing diagnostic tool that
  assesses point-cloud quality and structure (point density per m^2, voxel
  density distribution), returns plots for interactive use and optionally
  exports a PDF report. The loaded cloud stays in memory for a subsequent
  `Forest_seg()` run.
* `Forest_seg()` plot report now includes `crown_volume_m3` and
  `understory_volume_m3`, computed from the final classified points (count of
  occupied voxels at `canopy_vox_dim` resolution times the voxel volume) and
  rounded to whole cubic metres.

## Changes

* Crown base height (GAB): branch connectivity now uses **direct vertical
  adjacency only** (no gap-layer bridging). The BFS keeps the six lateral
  hexagonal neighbours, but a single empty layer now breaks a branch instead of
  being bridged. CBH therefore reflects the lowest foliated branch rather than
  bridging into the upper crown. `cbh_max_gap_layers` is retained for backward
  compatibility but is no longer used.
* Rescue pass (LOR) revised:
  - also considers pure-noise voxels (`cls == 0`), not only rejected clusters;
  - `h_trunk_factor` is clamped to `[0.3, 1.0]`, so rescue is never stricter
    than the base pass on height;
  - DBSCAN `minPts` is halved, so sparse noise can re-cluster;
  - a **stricter linearity threshold** than the base pass is applied (low
    trunk-band segments must be more linear), to avoid admitting bushes.
* `output_format`: LAS is the default; any value other than `"xyz"` falls back
  to LAS.
* Report CSVs keep the dot (`.`) as decimal separator for international use;
  crown/understory volumes are written as whole cubic metres.
* Removed the Crown Packing Index (CPI) and the unused internal helper
  `.analyze_canopy_float()`, which was never wired into the pipeline.
* Removed the unused internal helper `.assign_components_bfs_float()`.

## Bug fixes

* Fixed the voxel-to-point reclassification (DAV) that could fail when the DAV
  step returned early without an `H_threshold` value; it now falls back to
  `dav_understory_max_start`.
* Documented parameter defaults now match the function signature.
* Internal helper `.lor_create_voxels()` is marked internal and no longer
  appears in the package help index.

## Parameter default values

Updated defaults for `Forest_seg()`:

* `N = 3000`
* `h_rescue_max = 5`
* `dbh_max_radius = 0.5`
* `canopy_min_density = 500`
* `dav_understory_max_start = 1.3`
* `dav_eps = 2`
* `dav_minPts = 4`
* `dbh_heights = c(1.3, 1.8, 2.3)` (fixed internal parameter)

