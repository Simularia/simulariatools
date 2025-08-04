#' Plot stability class
#'
#' Histogram plot of stability classes by season or hour.
#'
#' Numerical values of stability classes are mapped as: 1 = A, 2 = B, ..., 6 = F.
#'
#' @param mydata The input data frame, which should contain \code{date} and \code{stability class} fields.
#' @param sc Name of the column in the data frame that represents the stability class.
#' @param type Specify how the data are to be split and plotted. Accepted values are "season" (default) and "hour".
#' @param locale Choose the locale for day and month names. The current locale is used by default, but you can also
#' specify a different one from the supported locales listed in stringi::stri_locale_list().
#' All labels will be in English by default or in Italian if its locale is specified.
#'
#' @return A \code{ggplot2} plot.
#'
#' @seealso [stabilityClass()], [plotAvgRad()], [plotAvgTemp()]
#'
#' @importFrom scales label_percent
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_bar labs element_blank guides
#'
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
    season <- clname <- hour <- ascissa <- NULL

    if (type != "season" && type != "hour")
        stop("Unspecified plot type.", call. = FALSE)

    if (!(sc %in% colnames(mydata)))
        stop("Undefined stability class field name.", call. = FALSE)

    # Check if stability class is in range 1 to 6
    if (max(mydata[, sc]) > 6 || min(mydata[, sc]) < 0)
        stop("Stability class is out of range [0,6].", call. = FALSE)

    pasquill <- factor(x = c("A", "B", "C", "D", "E", "F"))
    mydata$clname <- pasquill[mydata[, sc]]
    mydata$clname <- factor(mydata$clname, levels = sort(pasquill, decreasing = TRUE))

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
        mydata$ascissa <- factor(mydata$season, levels = unique(mydata$season))
        xlabel <- NULL
    } else {
        mydata$ascissa <- factor(as.numeric(format(mydata$date, format = "%H")))
    }

    # Color values for the 6 classes
    myColors <- c("#D53E4F", "#FC8D59", "#FEE08B", "#E6F598", "#99D594", "#3288BD")

    # Plot
    v <- ggplot(mydata, aes(x = ascissa, fill = clname)) +
        geom_bar(position = "fill", show.legend = TRUE)

    # Axis, legend, ...
    v <- v +
        scale_y_continuous(labels = scales::label_percent(),
                           breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
        scale_fill_manual(drop = FALSE,
                          values = myColors,
                          breaks = pasquill,
                          limits = pasquill
                          ) +
        labs(x = xlabel, y = ylabel) +
        theme_bw(base_family = "sans") +
        theme(legend.position = "bottom",
              panel.grid.major.x = element_blank()) +
        guides(fill = guide_legend(label.position = "bottom",
                                   label.hjust = 0.5,
                                   title = legendTitle,
                                   direction = "horizontal",
                                   ncol = 6,
                                   reverse = FALSE))

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
