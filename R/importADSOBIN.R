#' ADSO/BIN data import function
#' 
#' Function to import ADSO/BIN binary file type. It requires an installation of the `arinfopy` python library and uses the \pkg{reticulate} package to import it.
#' 
#' @import reticulate
#' 
#' @param file (character) ADSO/BIN file to be imported.
#' @param k float. Factor to be applied to x and y coordinates (default = 1).
#' @param kz float. Factor to be applied to z values (default = 1).
#' @param dx float. Shift x coordinates by dx (default = 0).
#' @param dy float. Shift y coordinates by dy (default = 0).
#' @param destaggering logical. If `TRUE` destaggering is applied (default =
#'   FALSE).
#' @param variable string, the name of the variable to be imported.
#' @param slice integer, horizontal slice for 3D variables (default = 1).
#' @param deadline integer, temporal deadline (default = 1). It can optionally be a string with date time (see examples).
#' @param raster.object boolean, if TRUE the function returns a `raster` object (default = FALSE).
#' @param verbose logical. If `TRUE` print out basic statistics (default =
#'   FALSE).
#'   
#' @details 
#' The `importADSIOBIN()` function was developed to import data from an ADSO/BIN binary file. It relies on the `arinfopy` python library (https://github.com/Simularia/arinfopy). 
#' 
#' @return 
#' In standard use, `importADSOBIN()` return a data frame with (X, Y, Z) columns. Column Z contains the values of the requested variable.
#' 
#' If \option{raster.object} option is activated the `importADSOBIN()` returns a `raster` object.
#'   
#' @export
#' 
#' @examples
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
#' # Read varibale M001S001 at ground level, at given date and time, and print basic information:
#' nox <- importADSOBIN(file = "conc_01-10_07_2018",
#'                      variable = "M001S001",
#'                      slice = 1,
#'                      deadline = "2018/07/02 12:00",
#'                      verbose = TRUE)
#' 
importADSOBIN <- function(file = file.choose(),
                          k = 1,
                          kz = 1,
                          dx = 0,
                          dy = 0,
                          destaggering = FALSE,
                          variable = NULL,
                          slice = 1, 
                          deadline = 1,
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
    nomvar2d = unlist(rec5['nomvar2d'])
    nomvar3d = unlist(rec5['nomvar3d'])
    
    # Check existence of the requested variable
    if (is.null(variable) || (!(variable %in% nomvar2d) && !(variable %in% nomvar3d))) {

        stop(paste0("\nVariable name not existing or unspecified.\n",
                    "Please select a variable in the following list:\n",
                    "2D: ", list(unname(nomvar2d)), "\n",
                    "3D: ", list(unname(nomvar3d))),
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
    
    # Manage deadlines
    ld <- abin$getDeadlines()
    ld <- lapply(ld, lubridate::parse_date_time,
                 orders = c("ymd H", "ymd HM", "ymd HMS",
                            "dmy H", "dmy HM", "dmy HMS"),
                 tz = "UTC")
    if (!is.numeric(deadline)) {
        tmp <- lubridate::parse_date_time(deadline, 
                                          orders = c("ymd H", "ymd HM", "ymd HMS",
                                                     "dmy H", "dmy HM", "dmy HMS"),
                                          tz = "UTC")
        deadline <- match(tmp, ld)
    }
    
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
        cat(sprintf("\n         Deadline        :%s", strftime(ld[[deadline]], 
                                                               fmt = "%Y-%m-%d %H:%M:%S", 
                                                               tz = "UTC")))
        cat(sprintf("\n         Vertical level  : %d", slice))
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
        grd3D <- raster::rasterFromXYZ(grd3D)
    }
    return(grd3D)
}