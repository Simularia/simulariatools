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
#' @param locale Locale to use for day and month names. Default is current
#' locale. Supported locales are listed in stringi::stri_locale_list().
#' All other labels are in English by default or in Italian if its locale is
#' specified.
#'
#' @return A \code{ggplot2} plot.
#'
#' @seealso [stabilityClass()], [plotAvgRad()], [plotAvgTemp()]
#'
#' @importFrom scales label_percent
#'
#' @export
#' @examples
#' data("stMeteo")
#'
#' # Season plot of stability class pgt
#' plotStabilityClass(stMeteo, sc = "pgt", type = "season")
#'
#' # Hourly plot of stability class pgt
#' plotStabilityClass(stMeteo, sc = "pgt", type = "hour")
#'
#' # Override default locale
#' plotStabilityClass(stMeteo, sc = "pgt", type = "season", locale = "it_IT")
#'
plotStabilityClass <- function(mydata, sc = "sc", type = "season", locale = NULL) {

    # Get locale if not explicitely set
    if (is.null(locale)) {
        locale <- Sys.getlocale(category = "LC_TIME")
    }

    # Fix No visible binding for global variable
    season <- clname <- hour <- NULL

    if (type != "season" && type != "hour")
        stop("Unspecified plot type.", call. = FALSE)

    if (!(sc %in% colnames(mydata)))
        stop("Undefined stability class field name.", call. = FALSE)

    # Check if stability class is in range 1 to 6
    if (max(mydata[, sc]) > 6 || min(mydata[, sc]) < 0)
        stop("Stability class is out of range [0,6].", call. = FALSE)

    pasquill <- c("A", "B", "C", "D", "E", "F")
    mydata$clname <- pasquill[mydata[, sc]]
    mydata$clname <- factor(mydata$clname, levels = sort(unique(mydata$clname),
                                                         decreasing = TRUE))

    if (grepl("it", locale)) {
        winterLabel <- "Inverno (DGF)"
        springLabel <- "Primavera (MAM)"
        summerLabel <- "Estate (GLA)"
        autumnLabel <- "Autunno (SON)"
        xlabel <- "Ora"
        ylabel <- "Percentuale (%)"
        legendTitle <- ""
    } else {
        winterLabel <- "Winter (DJF)"
        springLabel <- "Spring (MAM)"
        summerLabel <- "Summer (JJA)"
        autumnLabel <- "Autumn (SON)"
        xlabel <- "Hour"
        ylabel <- "Percentage (%)"
        legendTitle <- ""
    }

    if (type == "season") {
        mydata$month <- as.numeric(format(mydata$date, format = "%m"))
        mydata$season <- season(mydata$month)
        mydata$season[mydata$season == 1] <- winterLabel
        mydata$season[mydata$season == 2] <- springLabel
        mydata$season[mydata$season == 3] <- summerLabel
        mydata$season[mydata$season == 4] <- autumnLabel
        mydata$season <- factor(mydata$season, levels = unique(mydata$season))

        v <- ggplot(mydata, aes(x = season, fill = clname)) +
            geom_bar(position = "fill")

    } else {
        mydata$hour <- factor(as.numeric(format(mydata$date, format = "%H")))
        v <- ggplot(mydata, aes(x = hour, fill = clname)) +
            geom_bar(position = "fill")
    }
    v <- v +
        scale_y_continuous(labels = scales::label_percent(),
                           breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
        scale_fill_brewer(palette = "Spectral", direction = -1) +
        labs(x = xlabel, y = ylabel) +
        theme_bw(base_family = "sans") +
        theme(legend.position = "bottom",
              panel.grid.major.x = element_blank()) +
        guides(fill = guide_legend(
            label.position = "bottom",
            label.hjust = 0.5,
            title = legendTitle,
            direction = "horizontal",
            ncol = 6,
            reverse = TRUE))

    return(v)
}


season <- function(x) {
    res <- lapply(lubridate::month(x), function(x) {
        # Winter
        if (x %in% c(1, 2, 12)) s <- 1
        # Spring
        else if (x %in% c(3, 4, 5)) s <- 2
        # Summer
        else if (x %in% c(6, 7, 8)) s <- 3
        # Autumn
        else if (x %in% c(9, 10, 11)) s <- 4
        else s <- NULL
        s
    })
    res <- unlist(res)

    return(res)
}
