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
#' @param k integer. Factor to be applied to x and y coordinates (default = 1).
#' @param destaggering logical. If `TRUE` destaggering is applied (default = FALSE).
#' 
#' @return A dataset with x, y and z columns is returned.
#' 
#' @import raster
#' 
#' @export
#' 
#' @examples
#' # Import binary grid file and convert coordinates from km to m, without destaggering:
#' mydata <- importSurferGrd("/path_to_file/filename.grd", k = 1000, destaggering = FALSE)
#' 
#' 
importRaster <- function(fname, k = 1, destaggering = FALSE) {
    require("raster")
    
    t <- raster(fname)
    
    # Apply conversion factor
    xmax(t) <- xmax(t) * k
    xmin(t) <- xmin(t) * k
    ymax(t) <- ymax(t) * k
    ymin(t) <- ymin(t) * k
    
    # Apply destaggering
    if (destaggering == FALSE) {
        t <- shift(t, x = -res(t)[1] / 2., y = -res(t)[2] / 2.)
    }
    
    # Print some values
    xvalues <- c(xmin(t), xmax(t), res(t)[1])
    cat("\nX (min, max, dx)  :")
    cat(sprintf(fmt = "%12d", xvalues))

    yvalues <- c(ymin(t), ymax(t), res(t)[2])
    cat("\nY (min, max, dy)  :")
    cat(sprintf(fmt = "%12d", yvalues))

    zvalues <- c(cellStats(t, min), cellStats(t, max), cellStats(t, mean))
    cat("\nZ (min, max, mean):")
    cat(sprintf(fmt = "%12.2e", zvalues))

    
    # Export matrix
    grd3D <- rasterToPoints(t)
    grd3D <- data.frame(grd3D)
    colnames(grd3D) <- c("x", "y", "z")
    return(grd3D)
}

    