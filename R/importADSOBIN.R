#' Import ADSO/BIN as raster
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
#' @param deadline intefer, temporal deadline (default = 1)
#' @param verbose logical. If `TRUE` print out basic statistics (default =
#'   TRUE).
#'   
#' @return A dataset with x, y and z columns is returned.
#'   
#' @export
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
    
    xmin <- rec4$xlso * k
    ymin <- rec4$ylso * k
    
    xgrid <- rec4$dxmai
    ygrid <- rec4$dymai
    
    immai <- rec3$immai
    jmmai <- rec3$jmmai
    
    if (destaggering == TRUE) {
        xmin <- xmin + xgrid/2
        ymin <- ymin + ygrid/2
    }
    
    X <- seq(xmin, by = xgrid, length.out = immai)
    Y <- seq(ymin, by = ygrid, length.out = jmmai)
    xv <- rep(X, times = jmmai)
    yv <- rep(Y, each = immai)
    
    value <- abin$getSlice(variable = variable, 
                           slice = slice, 
                           deadline = deadline)
    value <- value * kz
    
    grd3D <- cbind(xv, yv, value)
    grd3D <- data.frame(grd3D)
    colnames(grd3D) <- c("x", "y", "z")

        # Print some values
    if (verbose == TRUE) {
        cat("\nRaster statistics -----------------------------------------------")
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
    
    return(grd3D)
}