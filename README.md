
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulariatools

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.165117.svg)](https://doi.org/10.5281/zenodo.596741)
<!-- badges: end -->

`simulariatools` is a collection of functions and tools useful to pre
and post process data for air quality assessment. It is developed and
maintained by the people at [Simularia](www.simularia.it).

If you use this package in your work, please consider citing it. Refer
to its [Zenodo DOI to cite it](https://doi.org/10.5281/zenodo.596741).

## Installation

Installation of `simulariatools` from GitHub can be easily using the
`devtools` package:

``` r
    library("devtools")
    install_github("Simularia/simulariatools")
```

The same commands can also be used to upgrade the package.

We also try to keep up to date versions of the package
[here](https://www.dropbox.com/sh/71252ydg6h3xvgc/AADiWKkh_tJHK97e5-ILAQVWa?dl=0)
if you can’t build the package yourself.

## List of functions

-   `contpourPlot2()`
-   `downloadBasemap()`
-   `importRaster()`.
-   `importADSOBIN()`.
-   `importSurferGrd()`.
-   `plotAvgRad()`.
-   `plotAvgTemp()`.
-   `plotStabilityClass()`.
-   `removeOutliers()`.
-   `rollingMax()`.
-   `stabilityClass()`.
-   `vectorField()`.
-   `contourPlot()` obsolete.
-   `createBaseMap()` obsolete.

If you are interested, you can track development and contribute at
<http://github.com/simularia/simulariatools>

Contact person: Giuseppe Carlino Simularia s.r.l.
<g.carlino@simularia.it>

## Contributors

Matteo Paolo Costa

## License

`simulariatools` is distributed under the GPL-2 license.
