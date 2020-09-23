#' Download basemap from Italian National Geoportal 
#' 
#' This function tries to download the aerial orthophoto of the requested domain from the 
#' [Italian National Geoportal](http://www.pcn.minambiente.it/mattm/en/services/).
#' The output is given in *png* format at the path given in the `file` parameter.
#' 
#' @return No value is returned.

#' @param file Path to output file.
#' @param xSW South West Easting UTM coordinate of the basemap (in metres).
#' @param ySW South West Northing UTM coordinate of the basemap (in metres).
#' @param xExt Easting extension in metres.
#' @param yExt Northing extension in metres.
#' @param crs UTM Coordinate Reference System: either 32 or 33.
#' @param width The basemap width.
#' @param height The basemap height.
#' @param units The unit of measure of width and height. 
#'             It can be `px` (pixels, the default), `in` (inches), `cm` or `mm`
#' @param res The resolution in dpi. 
#' 
#' @export
#' @importFrom utils download.file
#' 
#' @examples 
#' \dontrun{
#' # Download a basemap of a domain with SW coordinates (410000, 5000500) in the 
#' # UTM32 CRS and extension 5000m in both directions.
#' 
#' downloadBasemap(file = "./basemap.png", 
#'                 xSW = 410000, ySW = 5000500, xExt = 5000, yExt = 5000)
#'
#'
#' # Download a basemap of a domain with SW coordinates (410000, 5000500) in the 
#' # UTM32 CRS and extension 5000m in both directions.
#' # The file has to be 2048 x 2048 pixels.
#' 
#' downloadBasemap(file = "./basemap.png", 
#'                 xSW = 410000, ySW = 5000500, xExt = 5000, yExt = 5000, 
#'                 width = 2048, height = 2048)
#'                 
#'                 
#' # Download a basemap of a domain with SW coordinates (410000, 5000500) in the 
#' # UTM32 CRS and extension 5000m in both directions.
#' # The file has to be 10cm x 10cm with a resolution of 150 dpi.
#' 
#' downloadBasemap(file = "./basemap.png", 
#'                 xSW = 410000, ySW = 5000500, xExt = 5000, yExt = 5000, 
#'                 width = 10, height = 10, units = "cm", res = 150)
#' 
#' }
downloadBasemap <- function(file = file,
                            xSW = 410000, ySW = 5000500, 
                            xExt = 5000, yExt = 5000,
                            crs = 32,
                            width = 1024,
                            height = 1024,
                            units = "px",
                            res = 72) {

    # Check if output file has been defined
    if (missing(file)) {
        stop("Please define the output file.")
    }
    
    # Trying to access PCN
    message("Trying to access http://wms.pcn.minambiente.it")
    
    # Indirizzo base per WMS ortofoto 2012 da PCN
    url1 <- "http://wms.pcn.minambiente.it/ogc?map=/ms_ogc/WMS_v1.3/raster/ortofoto_colore_12.map&"
    
    # File size
    if (units == "in") {
        width <- width * res
        height <- height * res
    } else if (units == "cm") {
        width <- width / 2.54 * res
        height <- height / 2.54 * res
    } else if (units == "mm") {
        width <- (width / 10) / 2.54 * res
        height <- (height / 10) / 2.54 * res
    } else if (units == "px") {
        width <- width
        height <- height
    } else {
        stop("units not recognized.")
    }
    width <- round(width, digits = 0)
    height <- round(height, digits = 0)
    
    # CRS
    if (crs == 32) {
        # "EPSG:32632" UTM 32
        crsStr <- "EPSG:32632"
    } else if (crs == 33) {
        # "EPSG:32633" UTM 33
        crsStr <- "EPSG:32633"
    } else {
        stop("CRS not recognized.")
    }
    
    # FORMAT
    format <- "png"
    
    url2 <- paste("VERSION=1.3.0&REQUEST=GetMap&LAYERS=OI.ORTOIMMAGINI.2012&STYLES=default&CRS=",
                  crsStr, "&BBOX=", xSW, ",", ySW, ",", xSW + xExt, ",",
                  ySW + yExt, "&WIDTH=", width, "&HEIGHT=", height, "&FORMAT=image/",
                  format, sep = "")
    url <- paste(url1, url2, sep = "")
    
    # DL png file with specific CRS, BBOX, WIDTH, HEIGHT and FORMAT
    download.file(url, destfile = file, quiet = TRUE)
    
    message(paste("Please check for output file at:", file))
}