#' Plot average temperature
#' 
#' \code{plotAvgTemp} builds a bar plot of time average temperature and two
#' line plots with maximum and minimum temperature.
#'  
#' @param mydata A data frame containing fields date and temp
#' @param temp   Name of the column representing temperature
#' @param avg.time This defines the time period to average to 
#' (see openair::timeAverage). Default is "1 month".
#' @param ylabel The label to be plot along y axis
#' @param title Option plot title
#' 
#' @return A plot with average, min and max temperature in a given 
#' range of time.
#' 
#' @note \code{plotAvgTemp} uses \code{openair::timeAvearge} to compute
#' average.
#' 
#' @export
#' 
#' @import grid
#' @importFrom reshape2 melt
#' 
#' @examples
#' # Plot histogram with monthly averages together with maxima and minima 
#' # curves
#' data(stMeteo)
#' plotAvgTemp(stMeteo)
#' plotAvgTemp(stMeteo, temp = "temperature", 
#'             avg.time = "1 month", ylabel = "Temperatura [C]")
plotAvgTemp <- function(mydata, temp = "temp",
                        avg.time = "1 month",
                        ylabel = "Temperatura [C]",
                        title = "") {

    # Fix No visible binding for global variable
    temp.min <- temp.max <- NULL
    degree <- variable <- value <- .x <- NULL
    
    TZ <- attr(mydata$date, "tzone")
    if (is.null(TZ))
        TZ <- "GMT"
    
    if (!requireNamespace("openair", quietly = TRUE)) {
        stop("Please install openair from CRAN.", call. = FALSE)
    }
    mydata_mean <- openair::timeAverage(mydata,
                                        statistic = "mean",
                                        avg.time = avg.time)
    mydata_max <- openair::timeAverage(mydata,
                                       statistic = "max",
                                       avg.time = avg.time)
    mydata_min <- openair::timeAverage(mydata,
                                       statistic = "min",
                                       avg.time = avg.time)
    
    mydata_mean <- merge(mydata_mean, mydata_min, by = "date", all = TRUE)
    mydata_mean <- merge(mydata_mean, mydata_max, by = "date", all = TRUE)
    mydata_mean <- subset(mydata_mean, 
                          select = c("date", "temp.x", "temp.y", "temp"))
    colnames(mydata_mean) <- c("date", "temp", "temp.min", "temp.max")
    mydata_mean$date <- as.Date(mydata_mean$date, tz = TZ)
    
    v <- ggplot(mydata_mean, aes(date, temp)) + 
        geom_bar(aes(color = "Media",  fill = "Media"),
                 stat = "identity",
                 show.legend = FALSE) + 
        geom_line(aes(x = date, y = temp.min, color = "Minima"),  size = 1) + 
        geom_line(aes(date, temp.max, color = "Massima"),  size = 1) + 
        scale_y_continuous(labels = scales::math_format(.x * degree), 
                           breaks = seq(-20, 40, 5)) + 
        labs(title = title, x = "", y = ylabel) +
        scale_x_date(breaks = scales::date_breaks(width = avg.time),
                     labels = scales::date_format("%b")) +
        scale_color_manual(values = c("Media" = "steelblue", 
                                      "Minima" = "darkgreen", 
                                      "Massima" = "darkorange2"), 
                           guide = guide_legend(title = NULL)) +
        scale_fill_manual(values = c("Media" = "steelblue"), guide = NULL)  + 
        theme_bw(base_family = "sans") +
        theme(legend.position = c(0.01, 0.99), 
              legend.justification = c(0, 1),
              legend.box.margin = margin(t = 0, unit = "mm"))

    # Prepare table of data to be plot in the lower part of the figure
    # See http://learnr.wordpress.com/2009/04/29/
    #                   ggplot2-labelling-data-series-and-adding-a-data-table/

    mydata <- reshape2::melt(mydata_mean, 
                             measure.vars = c("temp.min", "temp", "temp.max"))
    mydata$value <- round(mydata$value, digits = 1)
    data_table <- ggplot(mydata,  aes(date, factor(variable),
                                      label = format(value, nsmall = 1))) +
        geom_text(size = 3.5) +
        scale_y_discrete(labels = c("min", "media", "max")) +
        theme_bw() + 
        labs(title = NULL, x = NULL, y = NULL) +
        theme(
            plot.margin = unit(c(-0.5, 2, 0, 2), "lines"),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank())
    
    mylayout <- grid::grid.layout(nrow = 2,
                                  ncol = 1,
                                  heights = unit(c(2, 0.25), c("null", "null")))
    
    #     grid.show.layout(mylayout)
    vplayout <- function(...) {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = mylayout))
    }
    subplot <- function(x, y) grid::viewport(layout.pos.row = x,
                                             layout.pos.col = y)
    mmplot <- function(a, b) {
        vplayout()
        print(a, vp = subplot(1, 1))
        print(b, vp = subplot(2, 1))
    }

    myplot <- mmplot(v, data_table)
}