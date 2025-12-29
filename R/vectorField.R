#' Vector field plot
#'
#' Simple function to plot a vector field given two components.
#'
#' This function plots a vector field given a data.frame with coordinates (x, y)
#' and corresponding velocity components (u, v). Vectors are coloured by
#' magnitude (speed).
#' The coordinates are assumed to be on a regular rectangular grid in the
#' UTM reference system.
#'
#' This function is heavily inspired by snippets of code in
#' *R Graphics Cookbook* by Winston Chang (https://r-graphics.org/index.html).
#'
#' @return A \code{ggplot2} object if `preview = TRUE`. A \code{ggplot2}
#' layer otherwise. In the latter case, the output should be piped to
#' a plot, such as \code{contourPlot2()} and the vector field will be overlapped.
#'
#'
#' @importFrom ggplot2 ggplot geom_segment aes
#'
#' @param data A dataframe containing data to be plotted in the form of:
#' *(x, y, u, v)*.
#' @param scale length factor of vector components
#' @param everyx keep one out of every *everyx* values, along *x* direction.
#' @param everyy keep one out of every *everyy* values, along *y* direction.
#' @param size arrow size.
#' @param preview (default = TRUE) create a plot. If FALSE it only creates
#' the ggplot2 directive to be added to another plot.
#'
#' @examples
#' \dontrun{
#' metU <- importADSOBIN(
#'     "/path/to/meteofile",
#'     variable = 'U',
#'     slice = 2,
#'     k = 1000,
#'     verbose = TRUE
#' )
#' metU <- as.data.frame(metU)
#' metU <- metU %>%
#'     mutate(u = z, z = NULL)
#'
#' metV <- importADSOBIN(
#'     "/path/to/meteofile",
#'     variable = 'V',
#'     slice = 2,
#'     k = 1000,
#'     verbose = TRUE
#'  )
#' metV <- as.data.frame(metV)
#' metV <- metV |>
#'     mutate(v = z, z = NULL)
#'
#' met <- merge(metU, metV, by = c("x", "y"))
#'
#' vectorField(
#'     met,
#'     everyx = 2,
#'     everyy = 2,
#'     scale = 10
#' ) +
#'     coord_fixed(ratio = 1, xlim = c(0, 1000), ylim = c(0, 1000)) +
#'     scale_color_viridis_c()
#'
#' # Overlap the vector field to a contour plot and set vector colours to black
#' met$ws <- sqrt(met$u^2 + met$v^2)
#' contourPlot2(met, z = "ws") +
#'     vectorField(
#'         met,
#'         everyx = 2,
#'         everyy = 2,
#'         scale = 10,
#'         preview = FALSE
#'     ) +
#'     scale_colour_gradient(low = "black", high = "black", guide = NULL)
#' }
#'
#' @export
#'
vectorField <- function(
    data,
    scale = 1.,
    everyx = 1,
    everyy = 1,
    size = 0.25,
    preview = TRUE
) {
    # Fix No visible binding for global variable
    x <- y <- u <- v <- magnitude <- NULL

    every_n <- function(x, by = 2) {
        x <- sort(x)
        x[seq(1, length(x), by = by)]
    }

    # Compute vector magnitude
    data <- as.data.frame(data)
    data$magnitude <- sqrt(data$u^2 + data$v^2)

    # Arrow length
    arrLength <- 0.01

    # Skip points
    keepx <- every_n(unique(data$x), by = everyx)
    keepy <- every_n(unique(data$y), by = everyy)
    datasub <- subset(data, x %in% keepx)
    datasub <- subset(datasub, y %in% keepy)

    # Vectors scale factor (1 m/s -> 100 m)
    scale <- scale * 100

    # Plot
    pl <- geom_segment(
        data = datasub,
        aes(x = x, y = y, xend = x + scale * u, yend = y + scale * v, colour = magnitude),
        arrow = arrow(length = unit(arrLength, "npc")),
        size = size
    )

    if (preview) {
        pl <- ggplot() + pl
    }
    return(pl)
}
