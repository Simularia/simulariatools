---
title: "Georeferenced basemaps"
---





In simulriatools 3.1.0 [contourPlot2()] is able to extract the bounding box of
the plot from a geo-referenced basemap file.

The main motivation behind this new feature is to minimize errors arising from the potential
misalignment between the basemap and the matrix of data to be plotted.
By using a GeoTIFF basemap, we can safely assume that the contour plot will be accurately 
aligned with it.
However, we still need to pay attention to the CRS of the data.
Since the [data.frame()] to be plotted is not CRS aware, we must check
whether the coordinates `x` and `y` are in the same reference system as the GeoTIFF file.

Here's a simple example of this new feature in action.



``` r
pm25 <- importADSOBIN(
    file = "../../development/avg_PM25",
    variable = "PM25",
    verbose = TRUE,
    k = 1000
)
#> 
#> ADSO/BIN statistics ---------------------------------------------
#>          Deadline        : 1 - 2018-01-01 03:00:00
#>          Vertical level  : 1 - 0.00
#>        X (min, max, dx)  :  579500.000   585500.000      100.000
#>        Y (min, max, dy)  : 4994000.000  5000000.000      100.000
#>     PM25 (min, max, mean):    0.00e+00     3.01e+00     1.11e-02
#> -----------------------------------------------------------------

basmap_file <- "../../development/avg_PM25_bm.tiff"
contourPlot2(pm25, basemap = basemap_file)
#> Error in `h()`:
#> ! error in evaluating the argument 'x' in selecting a method for function 'rast': object 'basemap_file' not found
```



