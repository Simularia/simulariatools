#' Remove data outliers
#'
#' Remove data outliers based on the interquartile range.
#'
#' @param x vector of data.
#' @param k factor to applied to the interquartile range (default = 1.5).
#'
#' @details
#' The interquartile range IQR is computed from input dataset as IQR = Q3 - Q1,
#' where Q1 is 25th percentile and Q3 is the 75th percentile.
#' Values larger than Q3 + k * IQR and smaller than Q1 - k * IQR are deemed
#' as outliers and substituted with NA's.
#'
#' The default value of k is 1.5.
#'
#' @return A numeric vector with the same length as input vector.
#'
#' @importFrom stats quantile
#'
#' @export
#' @examples
#' mydata <- c(-10 * runif(10), runif(10))
#' removeOutliers(mydata)
#'
removeOutliers <- function(x, k = 1.5) {

    # Compute quantiles
    qnt <- stats::quantile(x, probs = c(1 / 4, 3 / 4), na.rm = TRUE)

    # Compute the interquartile range and apply multiplication factor.
    H <- k * (qnt[2] - qnt[1])

    # set to NA what is outside k * IQR
    x[x < (qnt[1] - H)] <- NA
    x[x > (qnt[2] + H)] <- NA

    return(x)
}
