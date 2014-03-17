#' Compute rolling max
#' 
#' The rolling maximum value along a series of data is computed.
#' 
#' It computes the maximum value centered alonga a subset of data.
#' 
#' @param mydata A vector of data
#' @param length The length of data subset where the maximum values has to be picked.
#'   
#' @export
#' 
#' @examples
#' # Compute rolling max along 24 hours on hourly time serie
#' t$radg24h <- rollingMax(mydata=t$radg, length=24)
#' 
#' # Compute rolling max along 30 days on a hourly time serie
#' t$radg30d <- rollingMax(mydata=t$radg24h, length=24*30)
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