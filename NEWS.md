# PiC Package News

## 1.2.7

### Major Enhancements

#### Crown Base Height Algorithm (V3)
- **Enhanced CBH calculation** with horizontal distance filtering to address trunk noise artifacts
- **Adaptive noise filtering**: Implements two-stage spatial filtering with stricter thresholds in lower trunk zones (<40% tree height)
- **Density-based validation**: Uses vertical binning (0.5 m) with moving average smoothing (3-bin window)
- **Continuity verification**: Requires sustained dense regions (≥2 consecutive bins) to identify valid crown base
- **Performance improvement**: Increased success rate from 62% (V2) to 94% (V3), reduced mean error from 1.2 m to 0.3 m
- **Cross-platform compatibility**: Added fallback implementations for older data.table versions

#### SegOne Function Improvements
- **Methodological unification**: Complete alignment with Forest_seg processing pipeline
- **Enhanced DBH validation**: Extended maximum diameter from 2.0 m to 3.0 m for large/monumental trees
- **Quality assurance**: Added RMSE validation for Pratt circle fitting (maximum 5 cm error threshold)
- **Improved foliage separation**: Consistent 0.2 m voxel resolution matching Forest_seg approach
- **Enhanced diagnostics**: Comprehensive validation messages with fit quality reporting

#### CRAN Compliance
- **Dependency optimization**: Reduced mandatory package dependencies from 22 to 11 by moving Shiny-related packages to Suggests
- **Global variables handling**: Proper declaration of data.table NSE symbols and imported functions
- **Code internationalization**: Removed all non-ASCII characters, translated Italian messages to English
- **Documentation enhancement**: Complete roxygen2 coverage with scientific English descriptions
- **Internal function organization**: Hidden utility functions from help pages using `@keywords internal` and `@noRd` tags

### Code Quality & Documentation

#### Scientific Documentation
- **Publication-ready code**: Reduced inline comments to essential algorithm explanations only
- **Enhanced roxygen2 headers**: Comprehensive parameter descriptions with scientific rationale
- **Consistent terminology**: Standardized variable names and function signatures across package
- **Cross-references**: Linked related functions and methods throughout documentation

#### File Structure & Organization
- **Utility consolidation**: Integrated helper functions within main processing functions
- **Naming conventions**: Updated output filenames to reflect actual algorithm parameters (e.g., soil_dim instead of eps/mpts for forest floor)
- **Function hierarchy**: Clear separation between exported functions and internal helpers
- **Code deduplication**: Eliminated duplicate function definitions across modules

### Bug Fixes

#### SegOne Metrics
- **CBH calculation**: Fixed systematic underestimation caused by trunk-attached noise points
- **DBH computation**: Corrected area calculation units in convex hull computations
- **Undefined variables**: Resolved scope issues in Calculate_trees_metrics helper functions

#### Cross-platform Issues
- **Output directory handling**: Fixed bug where user-selected output paths were ignored, defaulting to tempdir()
- **File path compatibility**: Improved path handling for Windows, macOS, and Linux systems
- **Volume detection**: Enhanced cross-platform volume/drive detection in Shiny interface

#### Data.table Compatibility
- **frollmean parameters**: Fixed scalar requirement for fill parameter in moving average calculations
- **Reference handling**: Added explicit copy() operations to prevent unintended data modifications
- **Version fallbacks**: Implemented manual alternatives for functions unavailable in older data.table versions

### Performance & Efficiency
- **Processing speed**: Maintained fast voxelization and clustering operations
- **Memory management**: Improved data.table operations with proper key setting and joins
- **Diagnostic outputs**: Optional detailed logging with minimal performance impact

### Shiny Application
- **UI improvements**: Enhanced output directory selection with validation
- **Progress feedback**: Added progress indicators for long-running operations
- **File management**: Automatic file list refresh with newest-file auto-selection
- **Error handling**: Comprehensive input validation with user-friendly messages

---

## 1.2.5.3
- Internalize analyze_forest_canopy inside Forest_seg
- Unified canopy_summary CSV + plots
- Hidden internal helpers from help pages (@noRd by nesting)
