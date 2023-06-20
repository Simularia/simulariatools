#' Stability class.
#'
#' Computes stability class given net radiation, total cloud cover and wind
#' speed.
#' 
#' `stabilityClass()` computes stability class according to IAEA method based
#' on net radiation, total cloud cover tcc and wind speed. 
#' Net radiation and wind are used by day; tcc and wind are used by night.
#' 
#' Three different alogorithms are implemented; see source code for details.
#' 
#' @param rad The net radiation in W/m^2
#' @param tcc The total cloud cover in a range from 1  to 8
#' @param ws wind speed in m/s
#' @param option This is to determine which specific categories to use
#' to determine the stability
#'  class. It can be \code{impact} to comply with ARIA Impact(tm),
#'  \code{pasquill} or \code{custom}.
#' 
#' @return \code{stabilityClass} returns a numeric vector with Pasquill
#' stability classes coded as: A = 1, B = 2, ... , F = 6.
#' 
#' @seealso [plotStabilityClass()]
#' 
#' @export
#' 
#' @examples
#' 
#' # Compute stability class with custom algorithm
#' stMeteo$cst <- stabilityClass(rad = stMeteo$rad,
#'                               tcc = stMeteo$tcc,
#'                               ws = stMeteo$ws,
#'                               option = "custom")
#'                               
stabilityClass <- function(rad, tcc, ws, option="impact") {
    
    # check if the input vectors have the same length
    if (length(rad) != length(tcc) || length(tcc) != length(ws) || length(rad) != length(ws))
        stop("The length of the three vectors is different.", call. = FALSE)
    
    # check for stability option
    if (option != "impact" && option != "pasquill" && option != "custom") 
        stop("Invalid stability option.", call. = FALSE)
    
    if (option == "impact") {
        # Impact radiation vector (night, night, night, day)
        limrad <- 6
        radlim <- c(limrad, limrad, limrad, 145.4, 290.75, 581.5, 9999)
        
        # Impact velocity table
        vel <- c(1, 2, 4, 6, 7,  999)
        
        # Impact cloud code table
        nuvo <- c(4, 8, 999)
        
        # Impact stability classes
        tabStab <- array(NA, dim = c(6,7))
        tabStab[1,] <- c(6, 6, 4, 4, 2, 1, 1)
        tabStab[2,] <- c(6, 5, 4, 4, 2, 2, 1)
        tabStab[3,] <- c(6, 5, 4, 4, 3, 2, 1)
        tabStab[4,] <- c(5, 4, 4, 4, 3, 3, 2)
        tabStab[5,] <- c(4, 4, 4, 4, 4, 3, 3)
        tabStab[6,] <- c(4, 4, 4, 4, 4, 4, 3)
        
    } else if (option == "pasquill") {
    
        # Pasquill raditaion vector (night, night, night, day)
        limrad <- 1
        radlim <- c(limrad, limrad, 290.75, 581.5, 9999)
        
        # Pasquill velcocity vector
        vel <- c(2, 3, 5, 6, 999)  
        
        # Pasquill cloud cover vector
        nuvo <- c(4, 999)
        
        # Pasquill stability classes
        tabStab <- array(NA, dim = c(5, 5))
        tabStab[1,] <- c(6, 5, 2, 1, 1)
        tabStab[2,] <- c(6, 5, 3, 2, 1)
        tabStab[3,] <- c(5, 4, 3, 3, 2)
        tabStab[4,] <- c(4, 4, 4, 4, 3)
        tabStab[5,] <- c(4, 4, 4, 4, 3)        
    } else {
        
        limrad <- 1
        radlim <- c(limrad, limrad, limrad, 145.4, 290.75, 581.5, 9999)
        
        # Velocity vector
        vel <- c(2, 3, 4, 6, 999)  
        
        # Cloud cover vector
        nuvo <- c(2, 5, 999)
        
        # Stability classes
        tabStab <- array(NA, dim = c(5, 7))
        tabStab[1,] <- c(6, 5, 4, 4, 2, 1, 1)
        tabStab[2,] <- c(6, 5, 4, 4, 2, 2, 1)
        tabStab[3,] <- c(4, 4, 4, 4, 3, 3, 2)
        tabStab[4,] <- c(4, 4, 4, 4, 3, 3, 3)
        tabStab[5,] <- c(4, 4, 4, 4, 4, 4, 3)        
    }
    
    catStab <- -9
    
    lvec <- length(rad)
    
    for (i in 1:lvec) {
        iv <- 1
        while (ws[i] >= vel[iv]) iv <- iv + 1
    
        ir <- 1
        while (rad[i] >= radlim[ir]) ir <- ir + 1
        
        if (ir == 1) {
            while (tcc[i] >= nuvo[ir]) ir <- ir + 1
        }
        
        catStab[i] <- tabStab[iv, ir]
    }
    
    return(catStab)
}