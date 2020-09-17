#' Contour plot with hollow management
#' 
#' @import ggplot2
#' 
stat_hollow_contour <- function(mapping = NULL, data = NULL, geom = "hollow_polygon",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        geom = geom,
        stat = StatHollowContour,
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list( na.rm = na.rm, ... )
    )
}

StatHollowContour <- ggproto("StatHollowContour", Stat,
                        required_aes = c("x", "y", "z"),
                        # default_aes = aes(order = ..level..),
                        compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                 breaks = NULL, na.rm = FALSE) {
                            # If no parameters set, use pretty bins
                            if (is.null(bins) && is.null(binwidth) && is.null(breaks)) {
                                breaks <- pretty(range(data$z), 10)
                            }
                            # If provided, use bins to calculate binwidth
                            if (!is.null(bins)) {
                                binwidth <- diff(range(data$z)) / bins
                            }
                            # If necessary, compute breaks from binwidth
                            if (is.null(breaks)) {
                                breaks <- fullseq(range(data$z), binwidth)
                            }
                            # contour_lines(data, breaks, complete = complete)
                            contour_lines(data, breaks)
                        }
)


# contour_lines <- function(data, breaks, complete = FALSE) {
contour_lines <- function(data, breaks) {
    z <- tapply(data$z, data[c("x", "y")], identity)

    if (!requireNamespace("contoureR", quietly = TRUE)) {
        stop("Package \"contoureR\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    cl2 <- contoureR::getContourLines(x = data$x, y = data$y, z = data$z, levels = breaks)

    if (length(cl2) == 0) {
        warning("Not possible to generate contour data", call. = FALSE)
        return(data.frame())
    }

    data.frame(
        level = cl2$z,
        x = cl2$x,
        y = cl2$y,
        piece = cl2$GID,
        group = cl2$LID
    )
}