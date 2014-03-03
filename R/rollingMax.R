#' Compute rolling max
#' 
#' The maximum value along a series of data is computed.
#' 
#' @param mydata A vector of data
#' @param length The length of data subset where the maximum values has to be picked.
#'   
#' @export
#' 
rollingMax <- function(mydata, length=24) {
    
    lvec <- length(mydata)
    out <- rep(NA, lvec)
    if ((length %% 2) == 0) {
        iimin <- length / 2 - 1
        iimax <- length / 2
    } else {
        iimin <- (length - 1) / 2
        iimax <- iimin
    }
    for (i in 1:lvec) {
        imin = max(0, i - iimin)
        imax = min(i + iimax, lvec)
        out[i] <- max(mydata[imin:imax], na.rm=T)
    }
    
    return(out)
}