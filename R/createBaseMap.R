#' Create base map (OBSOLETE)
#'
#' Create base map. This is meant to be the deepest layer of contour plot map.
#' Axes coordinates are supposed to be in meters.
#'
#' @param imageFile (string) Path to the background 'png' file.
#' @param domain Six components vector with the domain SW corner coordinates,
#'   the X and Y extensions, and the number of breaks along the to axis (X, Y,
#'   DX, DY, NX, NY)
#' @param font_size This is the font size for axis labels
#' @param font_family This is the font family for labels
#'
#' @return A \code{ggplot2} plot.
#'
#' @export
#' @examples
#' \dontrun{
#' # Import image 'img'. Divide the axis with 9 ticks.
#' v <- createBaseMap(img, c(minx, miny, extent, extent, 9, 9), font_size=10)
#' }
#' 
createBaseMap <- function(imageFile, 
                          domain = c(0, 0, 1000, 1000, 5, 5), 
                          font_size = 10, 
                          font_family = "sans") {
        
    # domain boundary
    xmin <- domain[1]
    xmax <- xmin + domain[3]
    ymin <- domain[2]
    ymax <- ymin + domain[4]
    
    # numeber of divisions of the two axis
    nx   <- domain[5]
    ny   <- domain[6]
    
    # Graphical options
    opts <- vector("list", length=4)
    opts[[1]] <- scale_x_continuous(name="x [m]", 
                                    breaks=seq(xmin, xmax, length=nx), 
                                    limits=c(xmin, xmax), 
                                    expand=c(0,1))
    opts[[2]] <- scale_y_continuous(name="y [m]", 
                                    breaks=seq(ymin, ymax, length=ny), 
                                    limits=c(ymin, ymax), 
                                    expand=c(0,1))
    opts[[3]] <- coord_equal()
    opts[[4]] <- theme_bw(base_size = font_size, 
                          base_family = font_family)
    
    # create baseline plot
    v <- qplot(1:10, 1:10, geom="blank")
    
    # background raster image
#     img <- raster(image)
#     img <- rasterToPoints(img)
    img <- png::readPNG(imageFile)
    v <- v + annotation_raster(img, xmin, xmax, ymin, ymax)
    
    # apply graphical options
    v <- v + opts
    
    return(v)
}