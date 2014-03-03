#' Import Grid file
#' 
#' It imports Surfer like grid file
#' 
#' Surfer grd file is imported and an array of x, y, z columns is returned
#' X and y coordinats can be converted from km to m (default k=1000) and viceversa.
#' 
#'  
#' @param fname Surfer grd file to be imported
#' @param k Factor to apply to x and y coordinates
#' 
#' @import reshape2
#' 
#' @export
#' 
importSurferGrd <- function(fname, k=1000) {
    require("reshape2")
        
    t <- file(fname, "r")
    
    t1 <- readLines(t, 1) # DSAA code
    t2 <- scan(t, n=8, quiet = TRUE)    # Header
    
    nx <- t2[1]
    ny <- t2[2]
    xmin <- t2[3] * k 
    xmax <- t2[4] * k 
    ymin <- t2[5] * k
    ymax <- t2[6] * k
    zmin <- t2[7]
    zmax <- t2[8]
    
    #     # Destaggering
    #     deltax <- (xmax - xmin) / nx
    #     deltay <- (ymax - ymin) / ny
    #     xmin <- xmin - deltax/2
    #     xmax <- xmax - deltax/2
    #     ymin <- ymin - deltay/2
    #     ymax <- ymax - deltay/2
    
    print(paste("Z min = ", zmin), quote=F)
    print(paste("Z max = ", zmax), quote=F)
    
    map <- scan(t, as.numeric(0), quiet = TRUE)
    
    close(t)
    
    if (length(as.vector(map)) != nx * ny) 
        stop("Dimension of grid data does not match that of header")
    
    grd <- matrix(map, nx, ny)    
    
    grd3d <- melt(grd)
    stepx <- (xmax - xmin) / (nx - 1)
    stepy <- (ymax - ymin) / (ny - 1)
    
    grd3d[,1] <- (grd3d[,1] - 1) * stepx + xmin
    grd3d[,2] <- (grd3d[,2] - 1) * stepy + ymin
    names(grd3d) <- c("x", "y", "z")
    
    return(grd3d)
}