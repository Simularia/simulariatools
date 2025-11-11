#' Plot average temperature
#'
#' \code{plotAvgTemp} builds a bar plot of time average temperature and two
#' line plots with maximum and minimum temperature.
#'
#' @param mydata A dataframe containing data to plot.
#' @param date The name of the column representing date and time. Data must be of
#' class `POSIXlt` or `POSIXct` (default = "date"). If timezone is unspecified,
#' it is set to GMT.
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
#' @importFrom stats reshape
#' @importFrom scales breaks_width label_date label_math
#' @importFrom ggplot2 ggplot geom_col geom_line labs scale_x_continuous
#'                     expansion margin element_blank geom_text
#'                     scale_y_discrete
#'
#' @examples
#' # Plot average monthly temperature and curves with monthly maximum and minimum
#' data(stMeteo)
#' str(stMeteo)
#' plotAvgTemp(stMeteo)

#' # Add a custom title
#' plotAvgTemp(stMeteo, title = "Monthly temperature")
#'
#' # Override default locale
#' plotAvgTemp(stMeteo, avg.time = "1 month", locale = "it_IT")
#'
plotAvgTemp <- function(
    mydata,
    date = "date",
    temp = "temp",
    avg.time = "1 month",
    ylabel = NULL,
    title = "",
    locale = NULL
) {
    # Fix No visible binding for global variable
    degree <- rid <- variable <- value <- .x <- NULL

    # Fix name of datetime and temperature
    names(mydata) <- sub(date, "date", names(mydata))
    names(mydata) <- sub(temp, "temp", names(mydata))

    # Check if date column exist and is a datetime object
    if (!"date" %in% names(mydata) || !"POSIXt" %in% class(mydata$date)) {
        stop("A `date` column of class <POSIXt> is required.")
    }

    # Special case for italian locale
    if (!is.null(locale) && locale == "it") {
        locale <- "it_IT"
    }

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
        attr(mydata$date, "tzone") <- "UTC"
    }

    # Compute statistics grouping by month
    if (avg.time == "1 month") {
        mydata[["Month"]] <- strftime(mydata[["date"]], format = "%m")
        mydata_mean <- stats::aggregate(
            temp ~ Month,
            data = mydata,
            FUN = "mean",
            na.rm = TRUE
        )
        mydata_min <- stats::aggregate(
            temp ~ Month,
            data = mydata,
            FUN = "min",
            na.rm = TRUE
        )
        mydata_max <- stats::aggregate(
            temp ~ Month,
            data = mydata,
            FUN = "max",
            na.rm = TRUE
        )

        # Merge data
        mydata_mean <- merge(mydata_mean, mydata_min, by = "Month", all = TRUE)
        mydata_mean <- merge(mydata_mean, mydata_max, by = "Month", all = TRUE)
        mydata_mean <- subset(
            mydata_mean,
            select = c("Month", "temp.x", "temp.y", "temp")
        )
    } else {
        stop("Only avg.time = \"1 month\" is currently supported")
    }

    # Set column names and create a row index column
    colnames(mydata_mean) <- c("rid", "temp", "temp.min", "temp.max")
    # FIXME:make it more generic
    mydata_mean[["rid"]] <- as.numeric(mydata_mean[["rid"]])

    # Arrange data in long format
    mydata <- stats::reshape(
        mydata_mean,
        direction = "long",
        varying = list(2:4),
        times = c("temp", "temp.min", "temp.max"),
        timevar = "variable",
        v.names = "value"
    )
    mydata$value <- round(mydata$value, digits = 1)

    # Manage labels: default locale is "en"
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

    if (avg.time == "1 month") {
        # Build x axis labels in the required locale.
        # Store original locale.
        original_locale <- Sys.getlocale(category = "LC_TIME")
        # Switch to required locale
        if (!grepl("en", locale)) {
            Sys.setlocale(category = "LC_TIME", locale = locale)
        }
        # Build labels in the required locale
        x_labels <- format(
            seq.Date(
                from = as.Date("2021/1/1"),
                to = as.Date("2021/12/1"),
                by = "1 month"
            ),
            format = "%b"
        )
        # Go back to original locale, anyway
        Sys.setlocale(category = "LC_TIME", locale = original_locale)
    } else {
        stop("Only avg.time = \"1 month\" is currently supported")
    }

    # bar plot for average + lines for min and max
    bar_plot <- ggplot(
        mydata[mydata$variable == "temp", ],
        aes(rid, value)
    ) +
        geom_col(
            aes(colour = media, fill = media),
        ) +
        geom_line(
            data = mydata[mydata$variable == "temp.min", ],
            aes(x = rid, y = value, colour = minima),
            linewidth = 1,
            key_glyph = "timeseries"
        ) +
        geom_line(
            data = mydata[mydata$variable == "temp.max", ],
            aes(x = rid, y = value, colour = massima),
            linewidth = 1,
            key_glyph = "timeseries"
        ) +
        labs(title = title, x = NULL, y = ylabel) +
        scale_x_continuous(
            labels = x_labels,
            breaks = seq(from = 1, to = length(x_labels), by = 1),
            expand = expansion(mult = 0.02),
        ) +
        scale_y_continuous(
            labels = scales::label_math(.x * degree),
            breaks = seq(-20, 40, 5)
        ) +
        scale_color_manual(
            name = NULL,
            limits = c(media, massima, minima),
            breaks = c(media, massima, minima),
            values = c("steelblue", "darkorange2", "darkgreen")
        ) +
        scale_fill_manual(
            name = NULL,
            limits = c(media, massima, minima),
            breaks = c(media, massima, minima),
            values = c("steelblue", "darkorange2", "darkgreen")
        ) +
        theme_bw(base_family = "sans") +
        theme(
            legend.position = c(0.01, 0.99),
            legend.key.spacing.y = unit(2, "pt"),
            legend.justification = c(0, 1),
            legend.box.margin = margin(t = 0, unit = "mm"),
            panel.grid.major.x = element_blank()
        )

    # data table
    data_table <- ggplot(
        mydata,
        aes(rid, factor(variable), label = format(value, nsmall = 1))
    ) +
        geom_text(size = 3.5) +
        scale_x_continuous(
            labels = x_labels,
            breaks = seq(from = 1, to = length(x_labels), by = 1),
            expand = expansion(add = c(0.47, 0.5), mult = 0.02),
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
        mylayout <- grid::grid.layout(
            nrow = 2,
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
