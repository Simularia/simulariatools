#' Plot hourly average radiation
#' 
#' Plot a histogram with hourly average of solar radiation, together with June and December hourly averages (minimum and maximum).
#'
#' @param mydata A data frame containing fields date and radg
#' @param radg   Name of the column representing radiation
#' 
#' @return A \code{ggplot2} plot.
#' 
#' @import openair
#' @import ggplot2
#' @import scales
#' 
#' @export
#' 
#' @examples
#' plotAvgRad(mydata)
#' plotAvgRad(mydata, rad="radg")
#'
plotAvgRad <- function(mydata, rad="radg") {
    require("openair")
    require("ggplot2")
    require("scales")
    
    mydata <- subset(mydata, select=c("date", rad))
    mydata_dec <- selectByDate(mydata, month=12)
    mydata_jun <- selectByDate(mydata, month=6)
    
    means <- aggregate(mydata["radg"], format(mydata["date"],"%H"), mean, na.rm = TRUE)
    max_jun <- aggregate(mydata_jun["radg"], format(mydata_jun["date"],"%H"), max, na.rm = TRUE)
    max_dec <- aggregate(mydata_dec["radg"], format(mydata_dec["date"],"%H"), max, na.rm = TRUE)
    means$date <- as.numeric(means$date)
    max_jun$date <- as.numeric(max_jun$date)
    max_dec$date <- as.numeric(max_dec$date)
    
    v <- ggplot(data=means, aes(date, radg)) + geom_histogram(aes(date, radg, color="Media", fill="Media"), stat="identity", show_guide=FALSE)  + 
        geom_line(data=max_dec, aes(x=date, y=radg, color="Massimo Dicembre"), size=1) + 
        geom_line(data=max_jun, aes(x=date, y=radg, color="Massimo Giugno"), size=1) + 
        #         scale_y_continuous(breaks=seq(0,1000,100)) + labs(x="", y=ylabel) +
        scale_y_continuous(breaks=seq(0,1000,100)) + labs(x="", y=expression(paste("Radiazione Globale [W/", m^{2},"]"))) +
        scale_x_continuous(breaks=0:23) +
        #         scale_x_datetime(breaks=date_breaks(width=avg.time)) +
        scale_color_manual(values=c("Media"="steelblue", "Massimo Dicembre"="darkgreen","Massimo Giugno"="darkorange2"), guide=guide_legend(title=NULL)) +
        scale_fill_manual(values=c("Media"="steelblue"), guide=F)  + 
        theme_bw(base_family="Helvetica") +
        theme(legend.position=c(0,1), legend.justification=c(0,1))
    
    return(v)
}