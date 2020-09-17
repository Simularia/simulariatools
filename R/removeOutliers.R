#' Remove data outliers
#' 
#' Outliers are removed from a data set. The interquartile range is computed. 
#' Data larger or smaller than k * IQR are removed. 
#' 
#' @param x vector of data to be cleaned
#' @param k factor to applied to the interquartile range
#' 
#' @return vector of clenaed data
#' 
#' @importFrom stats quantile
#' 
#' @export
#' @examples
#' \dontrun{
#' # Remove data outside 5 time the interquartile range
#' mydata$ws <- removeOutliers(mydata$ws, 5)
#' }
#' 
removeOutliers <- function(x, k) {

    # Compute quantiles
    qnt <- quantile(x, probs = c(1/4, 3/4), na.rm = TRUE)
    
    # Compute the interquartile range
    H <- k * (qnt[2] - qnt[1])
    
    # set to NA what is outside k * IQR
    x[x < (qnt[1] - H)] <- NA
    x[x > (qnt[2] + H)] <- NA
    
    return(x)
}