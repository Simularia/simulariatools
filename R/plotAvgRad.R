#' Plot hourly average radiation
#' 
#' Plot a histogram with hourly average of solar radiation, together with hourly maxima for June and December.
#'
#' @param mydata A data frame containing fields `date` and `radg`
#' @param date  Name of the column representing date and time
#' @param rad   Name of the column representing radiation
#' 
#' @return A \code{ggplot2} plot.
#' 
#' @export
#' @examples
#' \dontrun{
#' plotAvgRad(mydata)
#' plotAvgRad(mydata, rad="radg")
#' }
plotAvgRad <- function(mydata, date="date", rad="radg") {
    
    mydata <- as.data.frame(mydata)
    # Rename columns
    names(mydata)[names(mydata) == date] <- 'date'
    names(mydata)[names(mydata) == rad] <- 'rad'
    # Select datetime and radiation
    mydata <- subset(mydata, select=c(date, rad))
    mydata_dec <- openair::selectByDate(mydata, month=12)
    mydata_jun <- openair::selectByDate(mydata, month=6)
    
    means <- aggregate(mydata[rad], format(mydata["date"],"%H"), mean, na.rm = TRUE)
    max_jun <- aggregate(mydata_jun[rad], format(mydata_jun["date"],"%H"), max, na.rm = TRUE)
    max_dec <- aggregate(mydata_dec[rad], format(mydata_dec["date"],"%H"), max, na.rm = TRUE)
    means$date <- as.numeric(means$date)
    max_jun$date <- as.numeric(max_jun$date)
    max_dec$date <- as.numeric(max_dec$date)
    
    means$rad <- means[[rad]]
    max_jun$rad <- max_jun[[rad]]
    max_dec$rad <- max_dec[[rad]]
    
    v <- ggplot(data=means, aes(x=date, y=rad)) +
        geom_bar(aes(color="Media", fill="Media"), stat="identity", show.legend = FALSE)  + 
        geom_line(data=max_dec, aes(x=date, y=rad, color="Massimo Dicembre"), size=1) + 
        geom_line(data=max_jun, aes(x=date, y=rad, color="Massimo Giugno"), size=1) + 
        #         scale_y_continuous(breaks=seq(0,1000,100)) + labs(x="", y=ylabel) +
        scale_y_continuous(breaks=seq(0,1000,100)) + labs(x="", y=expression(paste("Radiazione Globale [W/", m^{2},"]"))) +
        scale_x_continuous(breaks=0:23) +
        #         scale_x_datetime(breaks=date_breaks(width=avg.time)) +
        scale_color_manual(values=c("Media"="steelblue", "Massimo Dicembre"="darkgreen","Massimo Giugno"="darkorange2"), guide=guide_legend(title=NULL)) +
        scale_fill_manual(values=c("Media"="steelblue"), guide=F)  + 
        theme_bw(base_family="Arial") +
        theme(legend.position=c(0,1), legend.justification=c(0,1))
    
    return(v)
}