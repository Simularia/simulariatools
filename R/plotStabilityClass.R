#' Plot stability class
#' 
#' Plot histogram of stability class on season or hour base.
#' 
#' Numerical values of stability classes are mapped as: 1 = A, 2 = B, ..., 6 = F.
#' 
#' @param mydata A data frame containing \code{date} and \code{stability class}
#' fields.
#' @param sc The name of the stability class field.
#' @param type type determines how the data are split and then plotted.
#' Accepted values are "season" (default) and "hour".
#'
#' @return A \code{ggplot2} plot.
#' 
#' @importFrom scales percent
#' 
#' @export
#' @examples
#' \dontrun{
#' plotStabClass(t, cs = "PGT", type = "season")
#' plotStabClass(t, cs = "stability", type = "hour")
#' }
plotStabilityClass <- function(mydata, sc="sc", type="season") {
    
    if (!requireNamespace("openair", quietly = TRUE)) {
        stop("Please install openair from CRAN.")
    }
    
    # Fix No visible binding for global variable
    season <- clname <- hour <- NULL

    if (type != "season" && type != "hour") 
        stop("Unspecified plot type.")
    
    if ( !(sc %in% colnames(mydata)) ) 
        stop("Undefined stability class field name.")

    # Check if stability class is in range 1 to 6
    if (max(mydata[,sc]) > 6 || min(mydata[,sc]) < 0)
        stop("Stability class is out of range [0,6].")
    
    pasquill <- c("A", "B", "C", "D", "E", "F")
    mydata$clname <- pasquill[mydata[,sc]]
    mydata$clname <- factor(mydata$clname, levels = sort(unique(mydata$clname),
                                                         decreasing = TRUE))
    
    if (type == "season") {
        mydata <- openair::cutData(mydata, type = "season")
        # mydata$quarter <- quarters(mydata$date)
        # mydata$quarter[mydata$quarter == "Q1"] <- "Inverno"
        # mydata$quarter[mydata$quarter == "Q2"] <- "Primavera"
        # mydata$quarter[mydata$quarter == "Q3"] <- "Estate"
        # mydata$quarter[mydata$quarter == "Q4"] <- "Autunno"
        # mydata$season <- factor(mydata$season, 
        # levels = c("Inverno", "Primavera", "Estate", "Autunno"))
        
        mydata$season <- factor(mydata$season, levels = unique(mydata$season))

        v <- ggplot(mydata, aes(x = season, fill = clname)) +
            geom_bar(position = "fill")

    } else {
        mydata$hour <- factor(as.numeric(format(mydata$date, format = "%H")))
        # v <- ggplot(mydata, aes(x = hour, fill = clname)) +
            geom_bar(position = "fill")
    }
    v <- v + 
        scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1)) +
        scale_fill_brewer(palette = "Spectral", direction = -1) + 
        labs(x = "", y = "Percentuale (%)") +
        theme_bw(base_family = "sans") +
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(
            label.position = "bottom", 
            label.hjust = 0.5, 
            title = NULL, 
            direction = "horizontal",
            ncol = 6,
            reverse = TRUE)) 
    
    return(v)
}