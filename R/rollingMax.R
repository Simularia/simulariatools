#' Compute rolling max
#' 
#' The rolling maximum value along a series of data is computed.
#' 
#' It computes the maximum value centred along a subset of data.
#' 
#' @param mydata A vector of data
#' @param length The length of data subset where the maximum values has 
#' to be picked. The value must be greater or equal than 3.
#' 
#' @return A numneric vector of the same length as `mydata`.
#'   
#' @export
#' @examples
#' # Compute rolling max along 24 hours on hourly time series
#' data(airquality)
#' solar.R.24 <- rollingMax(mydata = airquality$Solar.R, length = 24)
#' 
rollingMax <- function(mydata, length = 24) {
    
    if (length <= 2) 
        stop("The window length must be greater than 2.")
    
    lvec <- length(mydata)
    out <- rep(NA, lvec)
    
    if ((length %% 2) == 0) {
        iimin <- length / 2 - 1
        iimax <- length / 2
    } else {
        iimin <- (length - 1) / 2
        iimax <- iimin
    }
    for (i in seq_along(mydata)) {
        imin <- max(1, i - iimin)
        imax <- min(i + iimax, lvec)
        out[i] <- max(mydata[imin:imax], na.rm = TRUE)
    }
    
    return(out)
}