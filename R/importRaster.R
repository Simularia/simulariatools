#' Import generic raster file
#' 
#' The function import the first layer of a generic raster file. Data are imported as an array of x, y, z columns.
#' X and y coordinats can be converted from km to m (default k=1000) and viceversa.
#' Destaggering is applied by default.
#' 
#' In output summery data are plotted.
#' 
#'  
#' @param fname (character) raster file to be imported.
#' @param k float. Factor to be applied to x and y coordinates (default = 1).
#' @param dx float. Shift x coordinates by dx (default = 0).
#' @param dy float. Shift y coordinates by dy (default = 0).
#' @param destaggering logical. If `TRUE` destaggering is applied (default = FALSE).
#' @param variable (string) variable name to be extracted (if any).
#' 
#' @return A dataset with x, y and z columns is returned.
#' 
#' @import raster
#' 
#' @export
#' 
#' @examples
#' # Import binary file and convert coordinates from km to m, without destaggering:
#' mydata <- importSurferGrd("/path_to_file/filename.grd", k = 1000, destaggering = FALSE)
#' 
#' # Import binary file and convert coordinates from km to m, with shift of 100 m in both directions:
#' mydata <- importSurferGrd("/path_to_file/filename.grd", k = 1000, dx = 100, dy = 100)
#' 
importRaster <- function(fname, k = 1, dx = 0, dy = 0, destaggering = FALSE, variable = NULL) {
    
    if ( is.null(variable)) {
        t <- raster(fname)
    } else {
        t <- raster(fname, varname = variable)
    }
    
    # Apply conversion factor
    xmax(t) <- xmax(t) * k
    xmin(t) <- xmin(t) * k
    ymax(t) <- ymax(t) * k
    ymin(t) <- ymin(t) * k
    
    # Apply destaggering
    if (destaggering == TRUE) {
        t <- shift(t, x = res(t)[1] / 2., y = res(t)[2] / 2.)
    }
    
    # Shift coordinates
    t <- shift(t, x = dx, y = dy)
    
    # Print some values
    xvalues <- c(xmin(t), xmax(t), res(t)[1])
    cat("\nX (min, max, dx)  :")
    cat(sprintf(fmt = "%12.3f", xvalues))

    yvalues <- c(ymin(t), ymax(t), res(t)[2])
    cat("\nY (min, max, dy)  :")
    cat(sprintf(fmt = "%12.3f", yvalues))

    zvalues <- c(cellStats(t, min), cellStats(t, max), cellStats(t, mean))
    cat("\nZ (min, max, mean):")
    cat(sprintf(fmt = "%12.2e", zvalues))

    
    # Export matrix
    grd3D <- rasterToPoints(t)
    grd3D <- data.frame(grd3D)
    colnames(grd3D) <- c("x", "y", "z")
    return(grd3D)
}

    