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
#' @param ylabel The label along the y axis.
#' If missing a default label is plotted.
#' @param title Optional plot title
#' @param locale Locale to use for day and month names. Default is current
#' locale. Supported locales are listed in stringi::stri_locale_list().
#' All other labels are in English by default or in Italian if its locale is
#' specified.
#'
#' @return A plot with average, min and max temperature in a given
#' range of time.
#'
#' @seealso [plotStabilityClass()], [plotAvgRad()]
#'
#' @export
#'
#' @import grid
#' @importFrom reshape2 melt
#' @importFrom scales breaks_width label_date label_math
#' @importFrom ggplot2 ggplot geom_bar geom_line labs scale_x_date
#'                     expansion margin element_blank geom_text
#'                     scale_y_discrete
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
    # temp.min <- temp.max <- NULL
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
    # If the user explicitely set ylabel = NULL set it to an empty string.
    # This is for adding an extra space on the right and imporve alignment
    # with the table below.
    if (is.null(ylabel)) {
        ylabel <- ""
    }

    # If undefined set timezone to GMT
    time_zone <- attr(mydata$date, "tzone")
    if (is.null(time_zone) || !time_zone %in% OlsonNames()) {
        attr(mydata$date, "tzone") <- "GMT"
    }

    # Compute statistics
    mydata[["Month"]] <- strftime(mydata[["date"]], format = "%m")
    mydata_mean <- stats::aggregate(
        temp ~ Month,
        data = mydata, FUN = "mean", na.rm = TRUE
    )
    mydata_min <- stats::aggregate(
        temp ~ Month,
        data = mydata, FUN = "min", na.rm = TRUE
    )
    mydata_max <- stats::aggregate(
        temp ~ Month,
        data = mydata, FUN = "max", na.rm = TRUE
    )

    mydata_mean <- merge(mydata_mean, mydata_min, by = "Month", all = TRUE)
    mydata_mean <- merge(mydata_mean, mydata_max, by = "Month", all = TRUE)
    mydata_mean <- subset(mydata_mean,
                          select = c("Month", "temp.x", "temp.y", "temp"))
    colnames(mydata_mean) <- c("date", "temp", "temp.min", "temp.max")
    mydata_mean[["date"]] <- ISOdate(2021, mydata_mean$date, 1)

    # Default locale is "en"
    media <- "Average"
    media_short <- "Avg"
    minima <- "Minimum"
    minima_short <- "Min"
    massima <- "Maximum"
    massima_short <- "Max"

    if (grepl("it", locale)) {
        media <- "Media"
        media_short <- "Media"
        minima <- "Minima"
        minima_short <- "Min"
        massima <- "Massima"
        massima_short <- "Max"
    }

    # Arrange data in long format
    mydata <- reshape2::melt(mydata_mean,
                             measure.vars = c("temp.min", "temp", "temp.max"))
    mydata$value <- round(mydata$value, digits = 1)
    date_min <- min(mydata$date)
    date_max <- max(mydata$date)

    # bar plot
    bar_plot <- ggplot(mydata[mydata$variable == "temp", ], aes(date, value)) +
        geom_bar(aes(colour = media,  fill = media), stat = "identity") +
        geom_line(
            data = mydata[mydata$variable == "temp.min", ],
            aes(x = date, y = value, colour = minima),
            linewidth = 1, key_glyph = "timeseries"
        ) +
        geom_line(
            data = mydata[mydata$variable == "temp.max", ],
            aes(x = date, y = value, colour = massima),
            linewidth = 1, key_glyph = "timeseries"
        ) +
        labs(title = title, x = NULL, y = ylabel) +
        scale_x_date(
            breaks = scales::breaks_width(width = avg.time),
            expand = expansion(add = 6),
            labels = scales::label_date("%b", locale = locale)
        ) +
        scale_y_continuous(labels = scales::label_math(.x * degree),
                           breaks = seq(-20, 40, 5)) +
        scale_color_manual(
            name = NULL,
            limits = c(media, massima, minima),
            breaks = c(media, massima, minima),
            values = c("steelblue", "darkorange2", "darkgreen")
        ) +
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

    # data table
    data_table <- ggplot(mydata,
                         aes(date,
                             factor(variable),
                             label = format(value, nsmall = 1))) +
        geom_text(size = 3.5) +
        scale_x_date(
            breaks = scales::breaks_width(width = avg.time),
            limits = as.Date(date_min, date_max),
            labels = scales::label_date("%b", locale = locale)
        ) +
        scale_y_discrete(labels = c(minima_short, media_short, massima_short)) +
        theme_bw() +
        labs(title = NULL, x = NULL, y = "") +
        theme_bw(base_family = "sans") +
        theme(
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none",
            panel.border = element_blank(),
            panel.grid = element_blank()
        )

    # Convert plots to grob
    g1 <- ggplot2::ggplotGrob(bar_plot)
    g2 <- ggplot2::ggplotGrob(data_table)

    # Fix panel width to 90% of available space
    panel_width_id_g1 <- unique(g1$layout[g1$layout$name == "panel", "l"])
    panel_width_id_g2 <- unique(g2$layout[g2$layout$name == "panel", "l"])
    gwidth <- grid::unit(0.9, "npc")
    g1$widths[panel_width_id_g1] <- gwidth
    g2$widths[panel_width_id_g2] <- gwidth

    # Function to plot two grobs in 1 column and 2 rows
    gg_vertical_draw <- function(a, b) {
        # Define grid layout: 2 rows. The lowest one is 1/8 high the other
        mylayout <- grid::grid.layout(nrow = 2,
            ncol = 1,
            heights = unit(c(1, 0.125), c("null", "null"))
        )
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = mylayout))
        grid::pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
        grid.draw(a)
        grid::upViewport()
        grid::pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
        grid.draw(b)
    }
    gg_vertical_draw(g1, g2)
}
