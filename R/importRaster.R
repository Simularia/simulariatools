#' Import generic raster file
#'
#' The function import the first layer of a generic raster file. Data are
#' imported as an array of x, y, z columns.
#'
#' Supported files include those managed by the \pkg{raster} package (as
#' netcdf),
#'
#' Destaggering is useful for importing data from the SPRAY model and it is not
#' applied by default.
#'
#' An optional summary output can be printed by setting the `verbose` parameter.
#'
#'
#' @param file The raster file to be imported.
#' @param k A numerical factor to be applied to x and y coordinates (default =
#'   1).
#' @param kz A numerical factor to be applied to z values (default = 1).
#' @param dx Shifts x coordinates by dx (default = 0).
#' @param dy float. Shift y coordinates by dy (default = 0).
#' @param destaggering Use `TRUE` to apply destaggering to X and Y coordinates
#'   (default = FALSE).
#' @param variable The name of the variable to be imported.
#' @param verbose If `TRUE`, prints out basic statistics (default = FALSE).
#'
#' @return It returns a dataframe with x, y and z columns.
#'
#' @seealso \code{\link{importADSOBIN}} to import ADSO/BIN files. See
#'   [importADSOBIN()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Import binary (netcdf) file and convert coordinates from km to m,
#' # without destaggering:
#' mydata <- importRaster(file = "/path_to_file/filename.nc",
#'                        k = 1000,
#'                        destaggering = FALSE)
#'
#' # Import binary (netcdf) file and convert coordinates from km to m,
#' # with shift of 100 m in both directions:
#' mydata <- importRaster(file = "/path_to_file/filename.nc",
#'                        k = 1000,
#'                        dx = 100,
#'                        dy = 100)
#' }
importRaster <- function(file = file.choose(),
                         k = 1,
                         kz = 1,
                         dx = 0,
                         dy = 0,
                         destaggering = FALSE,
                         variable = NULL,
                         verbose = FALSE) {

    if (is.null(variable)) {
        t <- raster::raster(file, ncdf = TRUE)
        variable <- as.character(t@data@names)
    } else {
        t <- raster::raster(file, varname = as.character(variable), ncdf = TRUE)
    }
    
    # Apply conversion factor
    raster::xmax(t) <- raster::xmax(t) * k
    raster::xmin(t) <- raster::xmin(t) * k
    raster::ymax(t) <- raster::ymax(t) * k
    raster::ymin(t) <- raster::ymin(t) * k
    
    # Apply value factor
    t <- t * kz
    
    # Apply destaggering
    if (destaggering == TRUE) {
        t <- raster::shift(t, 
                           dx = raster::res(t)[1] / 2., 
                           dy = raster::res(t)[2] / 2.)
    }
    
    # Shift coordinates
    t <- raster::shift(t, dx = dx, dy = dy)
    
    # Print some values
    if (verbose == TRUE) {
        cat("\nRaster statistics -----------------------------------------------")
        xvalues <- c(raster::xmin(t), raster::xmax(t), raster::res(t)[1])
        cat(sprintf("\n%8s (min, max, dx)  :", "X"))
        cat(sprintf(fmt = "%12.3f", xvalues))
    
        yvalues <- c(raster::ymin(t), raster::ymax(t), raster::res(t)[2])
        cat(sprintf("\n%8s (min, max, dy)  :", "Y"))
        cat(sprintf(fmt = "%12.3f", yvalues))
    
        zvalues <- c(raster::cellStats(t, min), raster::cellStats(t, max),
                     raster::cellStats(t, mean))
        cat(sprintf("\n%8s (min, max, mean):", variable))
        cat(sprintf(fmt = "%12.2e", zvalues))
        
        cat("\n-----------------------------------------------------------------\n")
    }

    
    # Export dataframe with x, y, x columns
    grd3D <- raster::rasterToPoints(t)
    grd3D <- data.frame(grd3D)
    colnames(grd3D) <- c("x", "y", "z")
    return(grd3D)
}