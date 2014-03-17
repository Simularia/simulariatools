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
#' @export
#' 
#' @examples
#' # Remove data outside 5 time the interquartile range
#' mydata$ws <- removeOutliers(mydata$ws, 5)
#' 
#' 
removeOutliers <- function(x, k) {

    # Compute quantiles
    qnt <- quantile(x, probs=c(0.25, 0.75), na.rm=TRUE)
    
    # Compute the interquartile range
    H <- k * IQR(x, na.rm=TRUE)
    
    # set to NA what is outside k * IQR
    x[x < (qnt[1] - H)] <- NA
    x[x > (qnt[2] + H)] <- NA
    
    return(x)
}