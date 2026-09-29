#' Import generic raster file
#'
#' A function to import a variable from a generic raster file, including datasets
#' with more than one time deadline.
#'
#' @param file character. Path to the raster file.
#' @param k numeric. Factor applied to x and y coordinates (default = 1).
#' For example, it can be used to convert the grid coordinates from km
#' to m (k = 1000).
#' @param kz numeric. Factor applied to the variable values (default = 1).
#' @param dx,dy numeric. Constant to shift x and y coordinates (default = 0).
#' @param destaggering Use `TRUE` to apply destaggering to X and Y coordinates
#'   (default = FALSE). See the `Details` section.
#' @param variable character. The name of the variable to be imported. If missing,
#' the function stops with an error message printing the list of available variables.
#' @param verbose logical. If `TRUE`, prints out basic statistics (default = FALSE).
#'
#' @details
#' This function is based on the \pkg{terra} package and it can import any format
#' managed by it, such as NetCDF.
#'
#' Destaggering applies a shift equal to half grid size in both horizontal
#' directions. It is useful for importing data from the SPRAY air quality dispersion
#' model and it is not applied by default.
#'
#' An optional summary output can be printed out by setting the `verbose` parameter
#' to `TRUE`. For datasets with time deadlines, statistics are printed for each time step.
#'
#' @return A data.frame with x, y and z columns for the grid cells coordinates
#' and the variable value. If the raster has more than one time deadline, the columns are
#' `x, y, variable, z, time` where `variable` is the layer name and `z` is the cell value.
#'
#' @seealso [importADSOBIN()], [importSurferGrd()]
#'
#' @importFrom terra has.time time rast varnames res xmin xmax ymin ymax shift global as.data.frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Import binary (netcdf) file and convert coordinates from km to m,
#' # without destaggering. Variable name is "NOx".
#' mydata <- importRaster(
#'     file = "/path_to_file/filename.nc",
#'     variable = "NOx",
#'     k = 1000,
#'     destaggering = FALSE
#' )
#'
#' # Import binary (netcdf) file and convert coordinates from km to m,
#' # with shift of 100 m in both directions:
#' mydata <- importRaster(
#'     file = "/path_tofile/filename.nc",
#'     variable = "pm10",
#'     k = 1000,
#'     dx = 100,
#'     dy = 100
#' )
#' }
importRaster <- function(
    file = file.choose(),
    k = 1,
    kz = 1,
    dx = 0,
    dy = 0,
    destaggering = FALSE,
    variable = NULL,
    verbose = FALSE
) {
    if (missing(variable)) {
        t <- terra::rast(file)
        variables <- terra::varnames(t)
        stop("Missing variables. Choose one from: ", list(variables))
    }
    t <- terra::rast(file, subds = as.character(variable))

    # Apply conversion factor
    terra::xmax(t) <- terra::xmax(t) * k
    terra::xmin(t) <- terra::xmin(t) * k
    terra::ymax(t) <- terra::ymax(t) * k
    terra::ymin(t) <- terra::ymin(t) * k

    # Apply value factor
    t <- t * kz

    # Apply destaggering
    if (destaggering == TRUE) {
        t <- terra::shift(
            t,
            dx = terra::res(t)[1] / 2.,
            dy = terra::res(t)[2] / 2.
        )
    }

    # Shift coordinates
    t <- terra::shift(t, dx = dx, dy = dy)

    # Check if SpatRaster has more than 1 deadline
    srHasTime <- terra::has.time(t) && terra::nlyr(t) > 1

    # Print some values
    if (verbose == TRUE) {
        xvalues <- c(terra::xmin(t), terra::xmax(t), terra::res(t)[1])
        yvalues <- c(terra::ymin(t), terra::ymax(t), terra::res(t)[2])
        zvalues <- c(
            terra::global(t, min, na.rm = TRUE),
            terra::global(t, max, na.rm = TRUE),
            terra::global(t, mean, na.rm = TRUE)
        )
        message("Raster statistics -----------------------------------------------")
        message(sprintf(
            "%8s (min, max, dx)  : %12.3f %12.3f %12.3f",
            "X",
            xvalues[1],
            xvalues[2],
            xvalues[3]
        ))
        message(sprintf(
            "%8s (min, max, dy)  : %12.3f %12.3f %12.3f",
            "Y",
            yvalues[1],
            yvalues[2],
            yvalues[3]
        ))
        if (srHasTime) {
            tLabels <- format(terra::time(t))
            for (idx in seq_along(tLabels)) {
                message(sprintf(
                    "%8s (time, min, max, mean): %s %12.2e %12.2e %12.2e",
                    variable,
                    tLabels[idx],
                    zvalues$min[idx],
                    zvalues$max[idx],
                    zvalues$mean[idx]
                ))
            }
        } else {
            message(sprintf(
                "%8s (min, max, mean): %12.2e %12.2e %12.2e",
                variable,
                zvalues[1],
                zvalues[2],
                zvalues[3]
            ))
        }
        message("-----------------------------------------------------------------")
    }

    # Export dataframe with x, y, z columns if no time is present.
    # Otherwise export x, y, variable, z, time
    if (srHasTime) {
        grd3D <- terra::as.data.frame(t, xy = TRUE, time = TRUE, wide = FALSE, row.names = FALSE)
        colnames(grd3D) <- c("x", "y", "variable", "z", "time")
    } else {
        grd3D <- terra::as.data.frame(t, xy = TRUE, time = FALSE, wide = TRUE, row.names = FALSE)
        colnames(grd3D) <- c("x", "y", "z")
    }
    return(grd3D)
}
