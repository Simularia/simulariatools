#' Plot average temperature
#' 
#' \code{plotAvgTemp} builds a plot with histograms of monthly average temperature and two lines with maximim and minimum monthly temperature.
#'  
#' @param mydata A data frame containing fields date and temp
#' @param temp   Name of the column representing temperature
#' @param avg.time This defines the time period to average to (see openair::timeAverage). Default is "1 month".
#' @param ylabel The label to be plot along y axis
#' 
#' @return A ggplot2 plot object.
#' 
#' @note \code{plotAvgTemp} uses \code{openair::timeAvearge} to compute average.
#' 
#' @export
#' 
#' @examples
#' 
#' # Plot histogram with monthly averages together with maxima and minima curves
#' plotAvgTemp(mydata)
#' 
plotAvgTemp <- function(mydata, temp="temp", avg.time="1 month", ylabel="Temperatura [°C]") {
    require("openair")
    require("ggplot2")
    require("scales")
    
    mydata_mean <- timeAverage(mydata, statistic="mean", avg.time=avg.time)
    mydata_max <- timeAverage(mydata, statistic="max", avg.time=avg.time)
    mydata_min <- timeAverage(mydata, statistic="min", avg.time=avg.time)
    mydata_mean <- merge(mydata_mean, mydata_min, by="date", all=T)
    mydata_mean <- merge(mydata_mean, mydata_max, by="date", all=T)
    mydata_mean <- subset(mydata_mean, select=c("date", "temp.x", "temp.y", "temp"))
    colnames(mydata_mean) <- c("date", "temp", "temp.min", "temp.max")
    # mydata_mean$month <- months.POSIXt(mydata_mean$date)
    # mydata_mean$month <- factor(mydata_mean$month, levels=month.name)
    
    v <- ggplot(data=mydata_mean, aes(date, temp)) + 
        geom_histogram(aes(date, temp, color="Media", fill="Media"), stat="identity", show_guide=FALSE) + 
        geom_line(aes(x=date, y=temp.min, color="Minimo"), size=1) + 
        geom_line(aes(date, temp.max, color="Massimo"), size=1) + 
        scale_y_continuous(breaks=seq(-20,40,5)) + labs(x="", y=ylabel) +
        scale_x_datetime(breaks=date_breaks(width=avg.time), labels = date_format("%b")) +
        scale_color_manual(values=c("Media"="steelblue", "Minimo"="darkgreen","Massimo"="darkorange2"), guide=guide_legend(title=NULL)) +
        scale_fill_manual(values=c("Media"="steelblue"), guide=F)  + 
        theme_bw(base_family="Helvetica") +
        theme(legend.position=c(0,1), legend.justification=c(0,1))
 
    return(v)
}