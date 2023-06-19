#' ADSO/BIN data import function
#'
#' Import data from ADSO/BIN binary file. It requires an active Python
#' installation with the `arinfopy` library.
#'
#' @import reticulate
#'
#' @param file     The ADSO/BIN file to be imported.
#' @param variable A string with the name of the variable to be imported.
#' @param slice    An integer corresponding to the  horizontal slice (vertical
#'   level) of 3D variables (default = 1). In the case of a 2D variable, it is
#'   ignored.
#' @param deadline An integer representing the temporal deadline (default = 1).
#'   It can optionally be a string with date time (see examples).
#' @param k A numeric factor to be applied to x and y coordinates (default = 1).
#' @param kz A numeric factor to be applied to z values to rescale them (default
#'   = 1).
#' @param dx A number to shift x coordinates by dx (default = 0).
#' @param dy A number to shift y coordinates by dy (default = 0).
#' @param destaggering Use `TRUE` to apply destaggering to X and Y coordinates
#'   (default = `FALSE`).
#' @param raster.object Use `TRUE` to return a `raster` object instead of a
#'   dataframe with (X, Y, Z) columns (default = `FALSE`).
#' @param verbose Use `TRUE` to print out basic statistics (default = `FALSE`).
#'
#' @details The `importADSIOBIN()` function was developed to import data from an
#'   ADSO/BIN binary file. It relies on the `arinfopy' (version >= 2.2.0) python
#'   library. For more information on the library see the [GitHub
#'   repository](https://github.com/Simularia/arinfopy).
#'
#'   For more information on the active `python` installation, check the
#'   documentation of \pkg{reticulate}.
#'
#' @return In standard use, `importADSOBIN()` return a data frame with
#' `(X, Y, Z)` columns. Column Z contains the values of the requested variable. 
#' If the `raster.object` option is set, it returns a RasterLayer object.
#'
#' @seealso \code{\link{importRaster}} to import NetCDF files.
#' 
#' @importFrom terra rast
#' @importFrom lubridate parse_date_time
#' 
#' @examples
#' 
#' \dontrun{
#' # Read ground level (slice = 1) value of variable M001S001.
#' pm10 <- importADSOBIN(file = "average_2018.bin",
#'                       variable = "M001S001",
#'                       slice = 1)
#'
#' # Read deadline 12 of the second vertical level of temperature:
#' temperature <- importADSOBIN(file = "swift_surfpro_01-10_01_2018",
#'                              variable = "TEMPK",
#'                              slice = 2,
#'                              deadline = 12)
#'
#' # Read varibale M001S001 at ground level, at given date and time,
#' # and print basic information:
#' nox <- importADSOBIN(file = "conc_01-10_07_2018",
#'                      variable = "M001S001",
#'                      slice = 1,
#'                      deadline = "2018/07/02 12:00",
#'                      verbose = TRUE)
#' }
#' 
#' @export
#' 
importADSOBIN <- function(file = file.choose(),
                          variable = NULL,
                          slice = 1, 
                          deadline = 1,
                          k = 1,
                          kz = 1,
                          dx = 0,
                          dy = 0,
                          destaggering = FALSE,
                          raster.object = FALSE,
                          verbose = FALSE) {
    
    # Load arinfopy lib with reticulate
    ap <- reticulate::import("arinfopy")
    # Load file
    abin <- ap$adsobin(file)
    
    # Read rec3, rec4 and rec5
    rec3 <- abin$getRecord3(1)
    rec4 <- abin$getRecord4(1)
    rec5 <- abin$getRecord5(1)
    
    # Get list of variables
    nomvar2d <- trimws(unlist(rec5['nomvar2d']), "both")
    nomvar3d <- trimws(unlist(rec5['nomvar3d']), 'both')
    
    # Check existence of the requested variable
    if (is.null(variable) || (!(variable %in% nomvar2d) && !(variable %in% nomvar3d))) {

        stop(paste0("\nVariable name not existing or unspecified.\n",
                    "Please select a variable in the following list:\n",
                    "2D: ", list(unname(nomvar2d)), "\n",
                    "3D: ", list(unname(nomvar3d))),
             call. = FALSE)
    }
    
    # Manage deadlines
    ld <- abin$getDeadlines()
    ld <- lapply(ld, lubridate::parse_date_time,
                 orders = c("ymd H", "ymd HM", "ymd HMS",
                            "dmy H", "dmy HM", "dmy HMS",
                            "ymd"),
                 tz = "UTC")
    if (!is.numeric(deadline)) {
        tmp <- suppressWarnings(
                    lubridate::parse_date_time(deadline, 
                                               orders = c("ymd H", "ymd HM", "ymd HMS",
                                                          "dmy H", "dmy HM", "dmy HMS"),
                                               tz = "UTC"))
        deadline <- match(tmp, ld)
    }
    
    # Check if deadline is available
    if (!(deadline <= length(ld)) | is.na(deadline)) {
        stop(paste0("\nDeadline not existing.\n"),
             call. = FALSE)
    }
    
    
    # If 2D variable set slice to 1
    if (variable %in% nomvar2d && slice != 1) {
        warning("\nA vertical level > 1 has been selected for a 2D variable.",
                "\n`slice` has been forced to 1\n",
                call. = FALSE)
        slice <- 1
    }

    # Check if slice is available
    vlevels <- rec4$sgrid
    if (!(slice <= length(vlevels))) {
        stop(paste0("\nSlice (vertical level) not available.\n"),
             call. = FALSE)
    }
    
    # Scale X and Y variables
    xmin <- rec4$xlso * k
    ymin <- rec4$ylso * k
    
    # Get grid size
    xgrid <- rec4$dxmai
    ygrid <- rec4$dymai
    
    # Get number of points
    immai <- rec3$immai
    jmmai <- rec3$jmmai
    
    # Destaggering
    if (destaggering == TRUE) {
        xmin <- xmin + xgrid/2
        ymin <- ymin + ygrid/2
    }
    
    # Build X and Y vectors as:
    # (x1, y1), (x2, y1), ..., (xN, y1), (x1, y2), (x2, y2), ..., (xN, y2), ...
    X <- seq(xmin, by = xgrid, length.out = immai)
    Y <- seq(ymin, by = ygrid, length.out = jmmai)
    xv <- rep(X, times = jmmai)
    yv <- rep(Y, each = immai)
    
    
    # Get values from getSlice method of arinfopy
    value <- abin$getSlice(variable = variable, 
                           slice = slice, 
                           deadline = deadline)
    
    # Scale concentration values
    value <- value * kz
    
    # Build (X, Y, Z) dataframe
    grd3D <- cbind(xv, yv, value)
    grd3D <- data.frame(grd3D)
    colnames(grd3D) <- c("x", "y", "z")

    # Print some values
    if (verbose == TRUE) {
        cat("\nADSO/BIN statistics ---------------------------------------------")
        cat(sprintf("\n         Deadline        : %d - %s", 
                    deadline,
                    strftime(ld[[deadline]], 
                             format = "%Y-%m-%d %H:%M:%S", 
                             tz = "UTC")))
        cat(sprintf("\n         Vertical level  : %d - %.2f", slice, vlevels[slice]))
        xvalues <- c(min(X), max(X), xgrid)
        # cat("\nX (min, max, dx)  :")
        cat(sprintf("\n%8s (min, max, dx)  :", "X"))
        cat(sprintf(fmt = "%12.3f", xvalues))
        
        yvalues <- c(min(Y), max(Y), ygrid)
        # cat("\nY (min, max, dy)  :")
        cat(sprintf("\n%8s (min, max, dy)  :", "Y"))
        cat(sprintf(fmt = "%12.3f", yvalues))
        
        zvalues <- c(min(value), max(value), mean(value))
        # cat("\nZ (min, max, mean):")
        cat(sprintf("\n%8s (min, max, mean):", variable))
        cat(sprintf(fmt = "%12.2e", zvalues))
        
        cat("\n-----------------------------------------------------------------\n")
    }
    
    # Return as grid
    if (raster.object == TRUE) {
        grd3D <- terra::rast(grd3D, type = "xyz")
    }
    return(grd3D)
}