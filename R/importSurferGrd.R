#' Import Grid file
#' 
#' A function to import data from Surfer text grid file.
#' 
#' Surfer grd file is imported and an array of x, y, z columns is returned
#' X and y coordinates can be converted from km to m (default k=1000) and vice versa.
#' Destaggering is applied by default.
#' 
#'  
#' @param fname Surfer grd file to be imported
#' @param k Factor to apply to x and y coordinates
#' @param destaggering Boolean variable to apply or not destaggering.
#' 
#' @return A dataset with x, y and z columns is returned.
#' 
#' 
#' @examples
#' \dontrun{
#' # Import Surfer Grd file and convert coordinates from km to m, with destaggering
#' mydata <- importSurferGrd("/path_to_file/filename.grd", k = 1000)
#' 
#' # Import Surfer Grd file and do not convert coordinates, without destaggering
#' mydata <- importSurferGrd("/path_to_file/filename.grd", k = 1, destaggering = FALSE)
#' }
#' 
#' @export
#' 
importSurferGrd <- function(fname, k = 1000, destaggering = FALSE) {
    
    t <- file(fname, "r")
    
    t1 <- readLines(t, 1) # DSAA code
    t2 <- scan(t, n = 8, quiet = TRUE)    # Header
    
    nx <- t2[1]
    ny <- t2[2]
    xmin <- t2[3] * k 
    xmax <- t2[4] * k 
    ymin <- t2[5] * k
    ymax <- t2[6] * k
    zmin <- t2[7]
    zmax <- t2[8]
    
    # Destaggering
    if (destaggering == FALSE) {
        deltax <- (xmax - xmin) / (nx - 1)
        deltay <- (ymax - ymin) / (ny - 1)
        xmin <- xmin - deltax / 2
        xmax <- xmax - deltax / 2
        ymin <- ymin - deltay / 2
        ymax <- ymax - deltay / 2        
    }
    
    print(paste("Z min = ", zmin), quote = F)
    print(paste("Z max = ", zmax), quote = F)
    
#     map <- scan(t, as.numeric(0), quiet = TRUE)
    map <- scan(t, nmax = nx * ny, quiet = T)    
    close(t)
    
    if (length(as.vector(map)) != nx * ny) 
        stop("Dimension of grid data does not match that of header")
    
    grd <- matrix(map, nx, ny)    
    
    grd3d <- reshape2::melt(grd)
    stepx <- (xmax - xmin) / (nx - 1)
    stepy <- (ymax - ymin) / (ny - 1)
    
    grd3d[, 1] <- (grd3d[, 1] - 1) * stepx + xmin
    grd3d[, 2] <- (grd3d[, 2] - 1) * stepy + ymin
    names(grd3d) <- c("x", "y", "z")
    
    return(grd3d)
}