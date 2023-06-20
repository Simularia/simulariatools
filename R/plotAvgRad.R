#' Plot hourly average radiation
#' 
#' Plot a histogram with hourly average of solar radiation, together with
#' hourly maxima for June and December.
#'
#' @param mydata A data frame containing fields with solar radiation time 
#' series.
#' @param date  Name of the column representing date and time.
#' @param rad   Name of the column representing radiation.
#' 
#' @return A \code{ggplot2} plot.
#' 
#' @seealso [plotStabilityClass()], [plotAvgTemp()]
#' 
#' @export
#' 
#' @importFrom stats aggregate
#' 
#' @examples
#' data(stMeteo)
#' plotAvgRad(stMeteo, date = "date", rad = "radg")
#' 
plotAvgRad <- function(mydata, date="date", rad="radg") {
    
    if (!requireNamespace("openair", quietly = TRUE)) {
        stop("Please install openair from CRAN.", call. = FALSE)
    }
    mydata <- as.data.frame(mydata)
    # Rename columns
    names(mydata)[names(mydata) == date] <- 'date'
    names(mydata)[names(mydata) == rad] <- 'rad'
    # Select datetime and radiation
    mydata <- subset(mydata, select = c(date, rad))
    mydata_dec <- openair::selectByDate(mydata, month = 12)
    mydata_jun <- openair::selectByDate(mydata, month = 6)
    
    means <- stats::aggregate(mydata["rad"], 
                              by = format(mydata["date"],"%H"), 
                              FUN = mean, na.rm = TRUE)
    max_jun <- stats::aggregate(mydata_jun["rad"], 
                                format(mydata_jun["date"],"%H"), 
                                FUN = max, na.rm = TRUE)
    max_dec <- stats::aggregate(mydata_dec["rad"],
                                format(mydata_dec["date"],"%H"),
                                FUN = max, na.rm = TRUE)
    means$date <- as.numeric(means$date)
    max_jun$date <- as.numeric(max_jun$date)
    max_dec$date <- as.numeric(max_dec$date)
    
    v <- ggplot(data = means, aes(x = date, y = rad)) +
        geom_bar(aes(color = "Media", fill = "Media"), 
                 stat = "identity", show.legend = FALSE) + 
        geom_line(data = max_dec, aes(x = date, y = rad,
                                      color = "Massimo Dicembre"), size = 1) + 
        geom_line(data = max_jun, aes(x = date, y = rad, 
                                      color = "Massimo Giugno"), size = 1) + 
        scale_y_continuous(breaks = seq(0, 1000, 100)) +
        scale_x_continuous(breaks = 0:23) +
        scale_color_manual(values = c("Media" = "steelblue",
                                      "Massimo Dicembre" = "darkgreen",
                                      "Massimo Giugno" = "darkorange2"),
                           guide = guide_legend(title = NULL)) +
        scale_fill_manual(values = c("Media" = "steelblue"), guide = NULL) + 
        labs(x = NULL,
             y = expression(paste("Radiazione Globale [W/", m^{2},"]"))) +
        theme_bw(base_family = "sans") +
        theme(legend.position = c(0.01, 0.99), 
              legend.justification = c(0, 1),
              legend.box.margin = margin(t = 0, unit = "cm"))

    return(v)
}