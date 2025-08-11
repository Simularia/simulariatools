# simulariatools 3.0.0

## Breaking changes

- Deprecated functions `contourPlot()` and `createBaseMap()` have been
  removed.
- `downloadBasemap()` now returns a `tiff` file with `GeoTIFF` metadata,
  instead of `png`.

## New features

- `contourPlot2()` introduces new arguments `mask` and `inverse_mask`.
  These allow plotting data only inside (or outside) a polygon specified
  by a `shp` file.
- Labels in `plotAvgTemp()`, `plotStabilityClass()` and `plotAvgRad()`
  now respect the locale setting. The optional argument `locale` has
  also been added to override the system locale. Currently supported
  locales are Italian and English; unsupported locales are converted to
  English.
- `plotAvgRad()` now includes options to change the y-axis label and the
  title.

## Minor improvements and fixes

- `contourPlot2()` now masks content outside the plot domain.
  Previously, values outside plot domain were set to NA's, which could
  potentially cause artefacts at the borders.
- `contourPlot2()` has been updated to fix some breaking changes
  introduced by `ggplot2` since version 3.5.0 where `guide_legend()`
  only draws a key glyph for a layer when the value is in the layer’s
  data.
- `contourPlot2()` now removes `-Inf` from the levels list (if present)
  when plotting only contour lines.
- In `plotAvgRad()`, a missing colour for average radiation in the
  legend has been fixed.
- In `plotAvgTemp()`, a missing colour for average temperature in the
  legend has been fixed.
- In `plotAvgTemp()`, a bug related to the name of the column with
  temperature data has been fixed.
- In `stabilityClass()`, the tables defining the stability class as
  functions of radiation, wind speed and cloud cover have been slightly
  updated.
- In `stabilityClass()` the `impact` method has been deprecated and
  renamed to `iaea`.
- In `plotStabilityClass()`, a bug where a missing class was not shown
  in the legend has been fixed.
- Almost all the dependencies from *Openair* have been removed.
- Other minor bugs have been fixed and documentation has been updated
  and hopefully improved.


# simulariatools 2.5.1

- Removed Unicode symbols (on CRAN request).
- Explicitly add x and y scales limits to `contourPlot2`.
- Updated deprecated functions from `scales` package.
- Bug fix: avoid `Inf` duplication in `contourPlot2` legend.
- Documentation: spell check and improvements.


# simulariatools 2.5.0

- Dependency from other packages has been reviewed. `RColorBrewer`,
  `dplyr` and `png` are not required any more.
- `raster` becomes suggested, since it is only required by the
  deprecated `contourPlot()` function. Geo processing is now performed
  by the required `terra` package.
- `Openair` is now a suggested package.
- Bugs were fixed in the Pasquill stability class functions
  `stabilityClass()` and `plotStabilityClass()`. Furthermore, some
  working examples have been added.
- In `contourPlot2()`, deprecation of `stat(level)` and `size` of
  `ggplot2()` have been fixed.
- Improved documentation and `README` file.


# simulariatools 2.4.0

- The package is ready to be submitted to CRAN.
- Fixed many syntax errors, cleaned up code to try to comply with best
  practices.
- Fixed warnings in the documentation and added some working examples in
  some functions.
- Added some tests.
- Added a sample dataset `stMeteo` with hourly values of wind speed,
  wind direction, temperature, global radiation, total cloud cover and
  stability class.
- Renamed `NEWS` to `NEWS.md`.
- Updated `README` file.


# simulariatools 2.3.1

- Trying to fix issues with fonts in `contourPlot2()`.
- Updated readme file and fixed some minor issues in the documentation.


# simulariatools 2.3.0

- BREAKING CHANGE: in raster plots in `contourPlot2` (tile = TRUE)
  intervals are closed on the left and open on the right. In previous
  versions it was the opposite. In this way an interval `0 - 1` does
  include values equal to 0 and does not include values equals to 1.


# simulariatools 2.2.3

- `magick` package is now suggested (not required). If it is not
  installed, it will not be possible to read (and plot) background base
  maps in `contourPlot2`.
- In `contourPlot2` do not add `Inf` as upper limit in levels in the
  case of delta maps, when the first level is negative. Note that this
  will probably change the output of some scripts.
- Vector field lengths are not normalised any more and changed default
  scale factor. This modification aims to facilitate comparison of
  vector fields from different source files.
- Updated readme and added a brief example.


# simulariatools 2.2.2

- Fixed labels of `contourPlot2` which occasionally were not properly
  formatted.


# simulariatools 2.2.1

- Minor update in the documentation.


# simulariatools 2.2.0

- Added boolean option `tile` to plot tiles instead of contours.


# simulariatools 2.1.1

- Fixed bug in `importRaster()`.


# simulariatools 2.1.0

## Breaking changes
  - `vectorField()` has now a unique parameter for scaling vector
    length;
  - Length of the arrow head in `vectorField()` has been removed in
    exchange for a sensible default;
- Bug fixes.


# simulariatools 2.0.0

## New features

- New `contourPlot2()` function:
- Based on *ggplot2* v3.3.0 and `geom_contour_filled` function;
- Colour bands are not overlapped any more therefore the maps are more
  accurate;
- Legend labels are more informative and smarter (you can also use +/-
  `Inf` for upper and lower bound);
- Legend type for contour lines (no filling) is now a line;
- Added possibility to specify column names to the data;
- Axis labels are rounded to 0 decimals (they are in metres);
- Background image can be any format (jpeg, png, etc.);
- Almost entirely a substitute of `contourPlot()`.
- New `downloadBasemap()` function to download base map from the Italian
  National Geoportal.

## Bug fixes and minor improvements

- `vectorField` has better defaults for vector lengths.
- Changed position of legend in `plotAvgRad()`.
- The package is now compliant with R cmd check (CRAN).
- Many other minor bug fixes and checks.


# simulariatools 1.4.0

- Introduced function `vectorField` to plot meteorological vector field.


# simulariatools 1.3.1

- Function `importADSOBIN` is now compatible with `arinfopy` version
  2.3.2.


# simulariatools 1.3.0

- Function `contourPlot` accepts a `size` parameter for contour line
  thickness.
- Function `contourPlot` accepts a `cover` boolean parameter to
  specify if contours should be colour covered or not.
- Bug fix in `plotAvgRad`.


# simulariatools 1.2.1

- Function `contourPlot` accepts a `bare` boolean parameter to remove
  any graphical element of the plot (axis, titles, legend, ...).


# simulariatools 1.2.0

- Function `contourPlot` accepts a `smoothness` parameter to improve the
  horizontal resolution of the dataset (smaller cells) by the given
  (integer) factor. The new raster object to be plotted is obtained by
  bilinear interpolation.


# simulariatools 1.1.0

- New `importADSOBIN` function to read ADSO/BIN files.
- `plotStabilityClass` uses seasons instead of quarters.
- `plotAvgTemp` correctly manages time zones.
- Option `verbose` of `importRaster()`set to `FALSE` by default (it was
  `TRUE`).
- Option `fname` of `importRaster` renamed to `file` and set to
  `file.chooser` by default.
- Documentation improvement.


# simulariatools 1.0.7

- `plotStabilityClass`:
  - Correct order in color bars from 'A' to 'F'.
    
    
# simulariatools 1.0.6

- `contourPlot`:
  - Fixed bug when lowest level to plot is negative, as in delta maps;
  - Fixed bug when there is an overlay to plot.


# simulariatools 1.0.5

- Borders in contour plots have width set to `0px` by default.


# simulariatools 1.0.4

- Improved treatment of borders in `contourPlot`.


# simulariatools 1.0.3

- Fixed bug when extending domain to close contour lines.


# simulariatools 1.0.2

- Fixed bug in user defined colours for `contourPlot`.


# simulariatools 1.0.1

- New `colors` parameter to `contourPlot` for user defined color
  palette.


# simulariatools 1.0

- New `geom_hollow_polygon` and `stat_hollow_contour` to correctly plot
  hollows in filled contour plots.


# simulariatools 0.6.5

- Add verbose option in `importRaster`.
- Add option `kz` in `importRaster` to multiply `z` values.


# simulariatools 0.6.4

- Added transparency level in `contourPlot` input.


# simulariatools 0.6.3

- `contourPlot` is able to correctly plot negative values. Improved
  management of borders. Bug fixes.


# simulariatools 0.6.2

- Vertical shift of the labels in the `contourPlot()` legend.


# simulariatools 0.6.1

- Fixed library dependence for `plotAvgTemp`.
- Added `stMeteo` dataset for testing.


# simulariatools 0.6

- New `contourPlot()` function to plot contour levels diagrams of
  pollutant concentration.
- Code cleaning.


# simulariatools 0.5.3

* Bug fixes.


# simulariatools 0.5.2

* Bug fixes.


# simulariatools 0.5.1

- in `importRaster()` added the possibility to shift X and/or Y
  coordinates by dX/dY.


# simulariatools 0.5

- Added `createBaseMap()` function to create base maps for contour
  plots. It currently works only with png files.
- Bug fix in destaggering of raster files. destaggering is now default
  to FALSE.
- Default fonts changed from Helvetica to Arial to avoid problems on
  windows machines.


# simulariatools 0.4.1

- Bug fix in destaggering of raster files.


# simulariatools 0.4

- New `importRaster()` function to read all *gdal* supported raster
  files.


# simulariatools 0.3.1

- Patched `importSurferGrd.R` to read GRD files generated by
  CALPUFF/CALPOST.


# simulariatools 0.3

- Added optional title parameter in `plotAvgTemp`.
- In `plotAvgTemp`, plots table of plotted values below the graph.
- Bug fixes in `plotAvgRad`


# simulariatools 0.2.1

- `stabilityClass()` has three methods to compute stability class:
  `impact`, as in ARIA Impact, `pasquill` (standard classification) and
  `custom`.
- `plotStabClass()` function has been renamed to `plotStabilityClass()`.
- It is now possible to select the name of the column with stability
  class values in `plotStabilityClass()`.
- Destaggering of grid fields is applied by default in
  `importSurferGrd()`.
- A little bit of clean-up in the documentation.
