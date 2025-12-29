#' Download basemap from Italian National Geoportal
#'
#' Download the aerial orthophoto of the requested domain from the
#' [Italian National Geoportal](https://gn.mase.gov.it/portale/home).
#'
#' @param file Path to output file. If file exists, it will be overwritten.
#' @param xSW South West Easting UTM coordinate of the basemap (in metres).
#' @param ySW South West Northing UTM coordinate of the basemap (in metres).
#' @param xExt Easting extension in metres.
#' @param yExt Northing extension in metres.
#' @param crs Coordinate Reference System as UTM zone: either 32 (default) or 33.
#' @param width The basemap width (default = 1024).
#' @param height The basemap height (default = 1024).
#' @param units The unit of measure of width and height.
#'        It can be `px` (pixels, the default), `in` (inches), `cm` or `mm`
#' @param res The resolution in dpi (default = 72).
#'
#' @details
#' The domain is specified by the South-West point coordinates, and its
#' extension in the `x` and `y` directions.
#' The Coordinate Reference System (CRS) is in UTM 32 or 33.
#'
#' Note that, even if the downloading is successful the file might be empty
#' due to some weird behaviour of the remote server from the PCN.
#'
#' @return
#' The output is a *tiff* encoded with `GeoTIFF` metadata at the path
#' provided. No value is returned.
#'
#' @export
#' @importFrom utils download.file
#'
#' @examples
#' \dontrun{
#' # Download a basemap of a domain with SW coordinates (410000, 5000500)
#' # in the UTM32 CRS and extension 5000m in both directions.
#'
#' downloadBasemap(
#'     file = "./basemap.tif",
#'     xSW = 410000, ySW = 5000500, xExt = 5000, yExt = 5000
#' )
#'
#' # Download a basemap of a domain with SW coordinates (410000, 5000500)
#' # in the UTM32 CRS and extension 5000m in both directions.
#' # The file has to be 2048 x 2048 pixels.
#'
#' downloadBasemap(
#'     file = "./basemap.tif",
#'     xSW = 410000, ySW = 5000500, xExt = 5000, yExt = 5000,
#'     width = 2048, height = 2048
#' )
#'
#' # Download a basemap of a domain with SW coordinates (410000, 5000500)
#' # in the UTM32 CRS and extension 5000m in both directions.
#' # The file has to be 10cm x 10cm with a resolution of 150 dpi.
#'
#' downloadBasemap(
#'     file = "./basemap.tif",
#'     xSW = 410000, ySW = 5000500, xExt = 5000, yExt = 5000,
#'     width = 10, height = 10, units = "cm", res = 144
#' )
#' }
downloadBasemap <- function(
    file = file,
    xSW = NA,
    ySW = NA,
    xExt = NA,
    yExt = NA,
    crs = 32,
    width = 1024,
    height = 1024,
    units = "px",
    res = 72
) {
    # Check if output file has been defined
    if (missing(file)) {
        stop("Please define the output file.", call. = FALSE)
    }
    file <- normalizePath(file)

    # Check if SW point coords are missing
    if (!is.numeric(xSW) || !is.numeric(ySW)) {
        stop("The coordinates of the S-W point of the domain are not properly defined.")
    }

    # Check if domain extension is missing
    if (!is.numeric(xExt) || !is.numeric(yExt)) {
        stop("The domain extension is not properly defined.")
    }

    # Trying to access PCN
    message("Trying to access http://wms.pcn.minambiente.it")

    # Indirizzo base per WMS ortofoto 2012 da PCN
    url1 <- "http://wms.pcn.minambiente.it/ogc?map=/ms_ogc/WMS_v1.3/raster/ortofoto_colore_12.map&"

    # Manage units transformation
    this_res <- 1
    if (units == "in") {
        this_res <- res
    } else if (units == "cm") {
        this_res <- res / 2.54
    } else if (units == "mm") {
        this_res <- res / 25.4
    } else if (units == "px") {
        this_res <- 1.0
    } else {
        stop("units not recognized.", call. = FALSE)
    }

    # Convert dimensions to pixels
    width <- round(width * this_res, digits = 0)
    height <- round(height * this_res, digits = 0)

    # CRS
    if (crs == 32) {
        # "EPSG:32632" UTM 32
        crsStr <- "EPSG:32632"
    } else if (crs == 33) {
        # "EPSG:32633" UTM 33
        crsStr <- "EPSG:32633"
    } else {
        stop("CRS not recognized.", call. = FALSE)
    }

    # FORMAT
    format <- "tiff"

    url2 <- paste(
        "VERSION=1.3.0&REQUEST=GetMap&LAYERS=OI.ORTOIMMAGINI.2012&STYLES=default&CRS=",
        crsStr,
        "&BBOX=",
        xSW,
        ",",
        ySW,
        ",",
        xSW + xExt,
        ",",
        ySW + yExt,
        "&WIDTH=",
        width,
        "&HEIGHT=",
        height,
        "&FORMAT=image/",
        format,
        sep = ""
    )
    url <- paste(url1, url2, sep = "")

    # DL png file with specific CRS, BBOX, WIDTH, HEIGHT and FORMAT
    success <- download.file(url, destfile = file, quiet = TRUE)
    if (success == 0) {
        message(paste("Please check for output file at:", file))
    } else {
        stop("Error downloading the required basemap")
    }
}
