#' Plot hourly average radiation
#'
#' Plot a histogram with hourly average of solar radiation, together with
#' hourly maxima for June and December.
#'
#' @param mydata A data frame containing data to plot.
#' @param date The name of the column representing date and time. Data must be of
#' class `POSIXlt` or `POSIXct` (default = "date"). If the timezone is unspecified,
#' it is set to GMT.
#' @param rad   Name of the column representing radiation (default = "radg").
#' @param ylabel The label along the y axis. If missing a default label is plotted.
#' @param title Optional plot title
#' @param locale Locale to use for legend. Default is English, the only other
#' one currently supported is Italian.
#'
#' @return A \code{ggplot2} plot.
#'
#' @seealso [plotStabilityClass()], [plotAvgTemp()]
#'
#' @export
#'
#' @importFrom stats aggregate
#' @importFrom ggplot2 ggplot geom_bar geom_line labs margin element_blank
#'                     guide_legend scale_color_manual scale_fill_manual
#'                     theme unit scale_x_continuous scale_y_continuous
#'
#' @examples
#' data(stMeteo)
#' plotAvgRad(stMeteo, date = "date", rad = "radg")
#'
plotAvgRad <- function(
    mydata,
    date = "date",
    rad = "radg",
    ylabel = NULL,
    title = "",
    locale = NULL
) {
    mydata <- as.data.frame(mydata)

    # Rename columns
    names(mydata)[names(mydata) == date] <- "date"
    names(mydata)[names(mydata) == rad] <- "rad"

    # If undefined set timezone to GMT
    this_timezone <- attr(mydata$date, "tzone")
    if (is.null(this_timezone) || !this_timezone %in% OlsonNames()) {
        attr(mydata$date, "tzone") <- "GMT"
    }

    # Select datetime and radiation
    mydata <- subset(mydata, select = c(date, rad))
    mydata["month"] <- lubridate::month(mydata[["date"]])
    mydata_jun <- mydata[mydata$month == 6, ]
    mydata_dec <- mydata[mydata$month == 12, ]

    means <- stats::aggregate(
        mydata["rad"],
        by = format(mydata["date"], "%H"),
        FUN = mean,
        na.rm = TRUE
    )
    max_jun <- stats::aggregate(
        mydata_jun["rad"],
        format(mydata_jun["date"], "%H"),
        FUN = max,
        na.rm = TRUE
    )
    max_dec <- stats::aggregate(
        mydata_dec["rad"],
        format(mydata_dec["date"], "%H"),
        FUN = max,
        na.rm = TRUE
    )
    means$date <- as.numeric(means$date)
    max_jun$date <- as.numeric(max_jun$date)
    max_dec$date <- as.numeric(max_dec$date)

    # Get locale if not explicitly set
    if (is.null(locale)) {
        locale <- Sys.getlocale(category = "LC_TIME")
    }

    if (grepl("it", locale)) {
        media <- "Media"
        minima <- "Massimo Dicembre"
        massima <- "Massimo Giugno"
    } else {
        media <- "Average"
        minima <- "December maximum"
        massima <- "June maximum"
    }

    # Check if ylabel has been passed as an argument
    if (missing(ylabel)) {
        if (grepl("it", locale)) {
            ylabel <- expression(paste("Radiazione Globale [W/", m^2, "]"))
        } else {
            ylabel <- expression(paste("Global Radiation [W/", m^2, "]"))
        }
    }

    v <- ggplot(data = means, aes(x = date, y = rad)) +
        geom_bar(
            aes(color = media, fill = media),
            stat = "identity"
        ) +
        geom_line(
            data = max_dec,
            aes(x = date, y = rad, color = minima),
            linewidth = 1,
            key_glyph = "timeseries"
        ) +
        geom_line(
            data = max_jun,
            aes(x = date, y = rad, color = massima),
            linewidth = 1,
            key_glyph = "timeseries"
        ) +
        scale_y_continuous(breaks = seq(0, 1000, 100)) +
        scale_x_continuous(breaks = 0:23) +
        scale_color_manual(
            name = NULL,
            values = c("steelblue", "darkorange2", "darkgreen"),
            limits = c(media, massima, minima),
            breaks = c(media, massima, minima),
            guide = guide_legend(title = NULL)
        ) +
        scale_fill_manual(
            name = NULL,
            values = c("steelblue", "darkorange2", "darkgreen"),
            limits = c(media, massima, minima),
            breaks = c(media, massima, minima),
            guide = guide_legend(title = NULL)
        ) +
        labs(x = NULL, y = ylabel) +
        theme_bw(base_family = "sans") +
        theme(
            legend.position = c(0.01, 0.99),
            legend.key.spacing.y = unit(2, "pt"),
            legend.justification = c(0, 1),
            legend.box.margin = margin(t = 0, unit = "cm"),
            panel.grid.major.x = element_blank()
        )

    return(v)
}
