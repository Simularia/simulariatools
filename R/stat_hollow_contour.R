#' Contour plot with hollow management
#' 
#' @import ggplot2
#' 
#' @export
#' 
stat_hollow_contour <- function(mapping = NULL, data = NULL, geom = "hollow_polygon",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatHollowContour,
        geom = geom,
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list( na.rm = na.rm,
                       ...
                       )
    )
}


#' @export
StatHollowContour <- ggproto("StatHollowContour", Stat,
                        required_aes = c("x", "y", "z"),
                        default_aes = aes(order = ..level..),
                        
                        compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                 breaks = NULL, complete = FALSE, na.rm = FALSE) {
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
                            
                            contour_lines(data, breaks, complete = complete)
                        }
                        
)

contour_lines <- function(data, breaks, complete = FALSE) {
    z <- tapply(data$z, data[c("x", "y")], identity)

    cl <- grDevices::contourLines(
        x = sort(unique(data$x)), y = sort(unique(data$y)), z = z,
        levels = breaks)


    if (length(cl) == 0) {
        warning("Not possible to generate contour data", call. = FALSE)
        return(data.frame())
    }

    # Convert list of lists into single data frame
    lengths <- vapply(cl, function(x) length(x$x), integer(1))
    levels <- vapply(cl, "[[", "level", FUN.VALUE = double(1))
    xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
    ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
    pieces <- rep(seq_along(cl), lengths)
    # Add leading zeros so that groups can be properly sorted later
    # groups <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")

    # group by levels and then in the geometry group by piece
    # mlevels <- gsub("[.]", "", levels)
    # groups <- paste(data$group[1], sprintf("%s", mlevels), sep = "-")
    mlevels <- gsub("[.]", "0", levels)
    groups <- paste(data$group[1], sprintf("%s", mlevels), sep = "-")
    groups <- rep(groups, lengths)

    data.frame(
        level = rep(levels, lengths),
        x = xs,
        y = ys,
        piece = pieces,
        group = groups
    )
}

