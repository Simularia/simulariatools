# simulariatools

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.165117.svg)](https://doi.org/10.5281/zenodo.596741)

`simulariatools` is a collection of functions and tools useful to pre and post
process data for air quality assessment. It is developed and mantained by the
people at [Simularia](www.simularia.it).

## List of functions

- `contpourPlot2()`
- `downloadBasemap()`
- `importRaster()`.
- `importADSOBIN()`.
- `importSurferGrd()`.
- `plotAvgRad()`.
- `plotAvgTemp()`.
- `plotStabilityClass()`.
- `removeOutliers()`.
- `rollingMax()`.
- `stabilityClass()`.
- `vectorField()`.
- `contourPlot()` obsolete.
- `createBaseMap()` obsolete.

## Install

You can install it from github repository, through the `devtools` package:

    library("devtools")
    install_github("Simularia/simulariatools")

The last command is also needed to upgrade the package.


## Contact

If you are interested, you can track development and contribute at http://github.com/simularia/simulariatools

Contact person:
    Giuseppe Carlino
    Simularia s.r.l.
    g.carlino@simularia.it


## Contributors

Matteo Paolo Costa (Arianet s.r.l.)


## License

`simulariatools` is distributed under the GPL-2 license.
