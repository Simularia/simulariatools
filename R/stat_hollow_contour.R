#' Contour plot with hollow management
#' 
#' @import ggplot2
#' @param mapping Set of aesthetic mappings created by [aes()] or
#'   [aes_()]. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param ... Other arguments passed on to [layer()].
#' @keywords internal
stat_hollow_contour <- function(mapping = NULL, data = NULL, 
                                geom = "hollow_polygon", position = "identity",
                                na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        geom = geom,
        stat = StatHollowContour,
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ... )
    )
}

StatHollowContour <- ggproto("StatHollowContour", Stat,
                        required_aes = c("x", "y", "z"),
                        # default_aes = aes(order = ..level..),
                        compute_group = function(data, scales,
                                                 bins = NULL,
                                                 binwidth = NULL,
                                                 breaks = NULL, 
                                                 na.rm = FALSE) {
                            # If no parameters set, use pretty bins
                            if (is.null(bins) && is.null(binwidth) &&
                                is.null(breaks)) {
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
    cl2 <- contoureR::getContourLines(x = data$x, y = data$y, z = data$z,
                                      levels = breaks)

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