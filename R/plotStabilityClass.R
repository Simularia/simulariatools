#' Plot stability class
#' 
#' Plot histogram of stability class on season or hour base.
#' 
#' Numerical values of stability classes are mapped as: 1 = A, 2 = B, ..., 6 = F.
#' 
#' @param mydata A data frame containing \code{date} and \code{stability class} fields.
#' @param sc The name of the stability class field.
#' @param type type determines how the data are split and then plotted. Accepetd values are "season" (default) and "hour".
#'
#' @return A \code{ggplot2} plot.
#' 
#' @import ggplot2
#' @import scales
#' 
#' @export
#' 
#' @examples
#' plotStabClass(t, cs="ClassStab", type="season")
#' plotStabClass(t, type="hour")
#' 
plotStabilityClass <- function(mydata, sc="sc", type="season") {

#     require("ggplot2")
#     require("scales")
    
    if (type != "season" && type != "hour") 
        stop("Unspecified plot type.")
    
    if ( !(sc %in% colnames(mydata)) ) 
        stop("Undefined stability class field name.")

#     if (max(mydata[,sc]) > 6 || min(mydata[,sc]) < 0)
#         stop("Stability class is out of range [0,6].")
    
    pasquill <- c("A", "B", "C", "D", "E", "F")
    mydata$clname <- pasquill[mydata[,sc]]
    
#     simudata$sc[simudata$sc == 1] <- "A"
#     simudata$sc[simudata$sc == 2] <- "B"
#     simudata$sc[simudata$sc == 3] <- "C"
#     simudata$sc[simudata$sc == 4] <- "D"
#     simudata$sc[simudata$sc == 5] <- "E"
#     simudata$sc[simudata$sc == 6] <- "F"


    if (type == "season") {
        mydata$quarter <- quarters(mydata$date)
        mydata$quarter[mydata$quarter == "Q1"] <- "Inverno"
        mydata$quarter[mydata$quarter == "Q2"] <- "Primavera"
        mydata$quarter[mydata$quarter == "Q3"] <- "Estate"
        mydata$quarter[mydata$quarter == "Q4"] <- "Autunno"
        mydata$quarter <- factor(mydata$quarter, levels=c("Inverno", "Primavera", "Estate", "Autunno"))
        v <- ggplot(data=mydata, aes(x=quarter)) + 
            geom_bar(aes(fill=clname), position="fill")
    } else {
        mydata$hour <- factor(as.numeric(format(mydata$date, format="%H")))
        v <- ggplot(data=mydata, aes(x=hour)) +
            geom_bar(aes(fill=clname), position="fill")
    }
    v <- v + scale_y_continuous(labels=percent, breaks=seq(0,1,0.1)) +
        scale_fill_brewer(palette="Spectral") +
        labs(x="", y="Percentuale (%)") + 
        guides(fill=guide_legend(label.position="bottom", label.hjust=0.5, title=NULL, direction="horizontal")) +
        theme_bw(base_family="Arial") + 
        theme(legend.position="bottom")
    
    return(v)
}