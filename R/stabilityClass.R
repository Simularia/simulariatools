#' Stability class.
#'
#' \code{stabilityClass} computes stability class.
#' 
#' It computes stability class according to IAES method based on net radiation and wind.
#' Net radiation and wind are used by day; tcc and wind are used by night.
#' 
#' @param radg The net radiation in W/m^2
#' @param tcc The total cloud cover in a range from 1  to 8
#' @param ws wind speed in m/s
#' 
#' @export
#' 
stabilityClass <- function(radg, tcc, ws, option="impact") {
    
    if (length(radg) != length(tcc) || length(tcc) != length(ws) || length(radg) != length(ws))
        stop("The length of the three vectors is different.")
    
    pasquill <- c("A", "B", "C", "D", "E", "F")
    
    
    if (option == "impact") {
        # Impact table (WARNING: to be verified)
        vel <- c(1, 2, 4, 6, 7,  999)
        
        tabStab <- array(NA, dim=c(6,7))
        tabStab[1,] <- c(6, 6, 4, 4, 2, 1, 1)
        tabStab[2,] <- c(6, 5, 4, 4, 2, 2, 1)
        tabStab[3,] <- c(6, 5, 4, 4, 3, 2, 1)
        tabStab[4,] <- c(5, 4, 4, 4, 3, 3, 2)
        tabStab[5,] <- c(4, 4, 4, 4, 4, 3, 3)
        tabStab[6,] <- c(4, 4, 4, 4, 4, 4, 3)
     } else {
        vel <- c(2, 3, 4, 6, 999)    
    
        tabStab <- array(NA, dim=c(5,7))
        tabStab[1,] <- c(6, 5, 4, 4, 2, 1, 1)
        tabStab[2,] <- c(6, 5, 4, 4, 2, 2, 1)
        tabStab[3,] <- c(4, 4, 4, 4, 3, 3, 2)
        tabStab[4,] <- c(4, 4, 4, 4, 3, 3, 3)
        tabStab[5,] <- c(4, 4, 4, 4, 4, 4, 3)        
     }
    
    limrad <- 6
    rad <- c(limrad, limrad, limrad, 145.4, 290.75, 581.5, 9999)
    
    nuvo <- c(1, 4, 999)
        
    catStab <- -9
    
    lvec <- length(radg)
    
    for (i in 1:lvec) {
        iv <- 1
        while (ws[i] >= vel[iv]) iv <- iv+1
        
        ir <- 1
        while (radg[i] >= rad[ir]) ir <- ir+1
        
        if (ir == 1) {
            while (tcc[i] > nuvo[ir]) ir <- ir + 1
        }
        
#         idx <- tabStab[iv, ir]
#         catStab[i] <- pasquill[idx]
        catStab[i] <- tabStab[iv, ir]
    }
    catStab   
}