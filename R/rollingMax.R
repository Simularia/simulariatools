#' Compute Rolling Max
#'
#' The function computes the rolling maximum value along a time series.
#'
#' It calculates the maximum over consecutive elements centered within a
#' specified window.
#'
#' For each index i, it considers a window of `length` points centered around i.
#' When `length` is odd, the center falls exactly on i and the window extends
#' equally to both sides.
#' When `length` is even, the window extends one less point to the left than to
#' the right and the rolling max is not exactly centered.
#'
#' Values near the start of the series use windows with fewer than `length`
#' data points if there are not enough preceding elements to form a full window.
#' Similarly for values at the end.
#'
#' @param mydata A numeric vector of data values
#' @param length An integer specifying the window size (number of observations)
#'   to consider. Must be at least 3 (default = 24).
#'
#' @return A numeric vector containing rolling maximum values, with same
#' dimensions as `mydata`.
#'
#' @export
#' @examples
#' # Compute rolling max over a 24-hour period on hourly time series data
#' data(stMeteo)
#' ws_24h <- rollingMax(mydata = stMeteo$ws, length = 24)
#'
rollingMax <- function(mydata, length = 24) {

    # Check if window size is a positive integer >= 2
    if (!is.numeric(length) || as.integer(length) != length || length <= 2) {
        stop("The window size must be a positive integer greater than 2.")
    }

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
