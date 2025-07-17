#' Plot average temperature
#'
#' \code{plotAvgTemp} builds a bar plot of time average temperature and two
#' line plots with maximum and minimum temperature.
#'
#' @param mydata dataframe with data to plot. date and time column must be
#' named as "date".
#' @param temp Name of the column representing temperature (default = "temp")
#' @param avg.time Defines the time period to average to.
#' Currently the only supported period is "1 month" (default).
#' @param ylabel The label along the y axis. If missing a default label is plotted.
#' @param title Optional plot title
#' @param locale Locale to use for day and month names. Default is current
#' locale. Supported locales are listed in stringi::stri_locale_list().
#' All other labels are in English by default or in Italian if its locale is
#' specified.
#'
#' @return A plot with average, min and max temperature in a given
#' range of time.
#'
#' @note \code{plotAvgTemp} uses \code{openair::timeAvearge} to compute average.
#'
#' @seealso [plotStabilityClass()], [plotAvgRad()]
#'
#' @export
#'
#' @import grid
#' @importFrom reshape2 melt
#' @importFrom scales breaks_width label_date label_math
#'
#' @examples
#' # Plot histogram with monthly averages together with maxima and minima
#' # curves
#' data("stMeteo")
#' plotAvgTemp(stMeteo)
#' plotAvgTemp(stMeteo, temp = "temperature",
#'             avg.time = "1 month", ylabel = "Temperatura [C]")
#'
#' # Override default locale
#' plotAvgTemp(stMeteo, avg.time = "1 month", locale = "it_IT")
#'
#' # Add title
#' plotAvgTemp(stMeteo, title = "Monthly temperature")
#'
plotAvgTemp <- function(mydata, temp = "temp",
                        avg.time = "1 month",
                        ylabel = NULL,
                        title = "",
                        locale = NULL) {

    # Fix No visible binding for global variable
    temp.min <- temp.max <- NULL
    degree <- variable <- value <- .x <- NULL

    # avg.time has only one value allowed
    stopifnot(avg.time == "1 month")

    # Fix name of temperature column
    names(mydata) <- sub(temp, "temp", names(mydata))

    # Get locale if not explicitely set
    if (is.null(locale)) {
        locale <- Sys.getlocale(category = "LC_TIME")
    }

    # Check if ylabel has been passed as an argument
    if (missing(ylabel)) {
        if (grepl("it", locale)) {
            ylabel <- "Temperatura [C]"
        } else {
            ylabel <- "Temperature [C]"
        }
    }

    # If undefined set timezone to GMT
    TZ <- attr(mydata$date, "tzone")
    if (is.null(TZ) || !TZ %in% OlsonNames()) {
        attr(mydata$date, "tzone") <- "GMT"
    }

    mydata[["Month"]] <- strftime(mydata[["date"]], format = "%m")
    mydata_mean <- stats::aggregate(temp ~ Month, data = mydata, FUN = "mean", na.rm = TRUE)
    mydata_min <- stats::aggregate(temp ~ Month, data = mydata, FUN = "min", na.rm = TRUE)
    mydata_max <- stats::aggregate(temp ~ Month, data = mydata, FUN = "max", na.rm = TRUE)

    mydata_mean <- merge(mydata_mean, mydata_min, by = "Month", all = TRUE)
    mydata_mean <- merge(mydata_mean, mydata_max, by = "Month", all = TRUE)
    mydata_mean <- subset(mydata_mean,
                          select = c("Month", "temp.x", "temp.y", "temp"))
    colnames(mydata_mean) <- c("date", "temp", "temp.min", "temp.max")
    mydata_mean[["date"]] <- ISOdate(2021, mydata_mean$date, 1)

    if (grepl("it", locale)) {
        media <- "Media"
        mediaShort <- "Media"
        minima <- "Minima"
        minimaShort <- "Min"
        massima <- "Massima"
        massimaShort <- "Max"
    } else {
        media <- "Average"
        mediaShort <- "Avg"
        minima <- "Minimum"
        minimaShort <- "Min"
        massima <- "Maximum"
        massimaShort <- "Max"
    }

    bar_plot <- ggplot(mydata_mean, aes(date, temp)) +
        geom_bar(aes(colour = media,  fill = media), stat = "identity") +
        geom_line(aes(x = date, y = temp.min, colour = minima),  linewidth = 1, key_glyph = "timeseries") +
        geom_line(aes(x = date, y = temp.max, colour = massima),  linewidth = 1, key_glyph = "timeseries") +
        labs(title = title, x = "", y = ylabel) +
        scale_x_date(breaks = scales::breaks_width(width = avg.time),
                     labels = scales::label_date("%b", locale = locale)) +
        scale_y_continuous(labels = scales::label_math(.x * degree),
                           breaks = seq(-20, 40, 5)) +
        scale_color_manual(name = NULL,
                           limits = c(media, massima, minima),
                           breaks = c(media, massima, minima),
                           values = c("steelblue", "darkorange2", "darkgreen")) +
        scale_fill_manual(name = NULL,
                           limits = c(media, massima, minima),
                           breaks = c(media, massima, minima),
                           values = c("steelblue", "darkorange2", "darkgreen")) +
        theme_bw(base_family = "sans") +
        theme(legend.position = c(0.01, 0.99),
              legend.key.spacing.y = unit(2, "pt"),
              legend.justification = c(0, 1),
              legend.box.margin = margin(t = 0, unit = "mm"),
              panel.grid.major.x = element_blank()) +
        NULL

    # Prepare table of data to be plot in the lower part of the figure
    # See http://learnr.wordpress.com/2009/04/29/
    #                   ggplot2-labelling-data-series-and-adding-a-data-table/

    mydata <- reshape2::melt(mydata_mean,
                             measure.vars = c("temp.min", "temp", "temp.max"))
    mydata$value <- round(mydata$value, digits = 1)

    data_table <- ggplot(mydata,
                         aes(date, factor(variable), label = format(value, nsmall = 1))) +
        geom_text(size = 3.5) +
        scale_y_discrete(labels = c(minimaShort, mediaShort, massimaShort)) +
        theme_bw() +
        labs(title = NULL, x = NULL, y = NULL) +
        theme(plot.margin = unit(c(-0.5, 2, 0, 2), "lines"),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank())

    mylayout <- grid::grid.layout(nrow = 2,
                                  ncol = 1,
                                  heights = unit(c(2, 0.25), c("null", "null")))

    vplayout <- function(...) {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = mylayout))
    }
    subplot <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
    mmplot <- function(a, b) {
        vplayout()
        print(a, vp = subplot(1, 1))
        print(b, vp = subplot(2, 1))
    }

    mmplot(bar_plot, data_table)
}
