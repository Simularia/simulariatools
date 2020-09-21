#' New contour plot of pollutant concentration
#'
#' \code{contourPlot} plots a contour map of pollutants. 
#'
#' This is a convenience function to plot contour levels of a pollutant matrix
#' with \code{ggplot2} version >= 3.3.0. 
#' 
#' Domain data are exptected to be on a regular rectangular grid with UTM coordinates.
#' 
#' @return A \code{ggplot2} plot.
#' 
#' @param data A dataframe containing data to be plotted.
#' @param x (string) Name of the column with Easting data.
#' @param y (string) Name of the column with Northing data.
#' @param z (string) Name of the column with values data.
#' @param domain An array with min X, max X, min Y, max Y, number of ticks on X
#'   axis, number of ticks on Y axis (optional).
#' @param background String containing the path to the png file to be plotted as
#'   a basemap (optional).
#' @param underlayer Array of strings containing layers to be plotted between
#'   basemap and contour plot (optional).
#' @param overlayer Array of strings containing layers to be plotted on top of
#'   the contour plot (optional).
#' @param legend (string) Legend title (optional).
#' @param levels Array of levels for contour plot. If not set, automatic levels
#'   are plotted.
#' @param transparency float (between 0 and 1, default=0.66). Transparency level
#'   of the contour plot.
#' @param colors Colour palette for contour plot
#' @param bare Boolean (default FALSE) parameter to completely remove axis, legend, titles
#'   and any other graphical element from the plot.
#' @param size float with the thickness of the contour line.
#' @param fill boolean (default TRUE) to specify whether the contour plot 
#'   should be filled or not.
#'   
#' @importFrom openair quickText
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot annotation_custom geom_contour_filled 
#'                     scale_fill_manual scale_x_continuous scale_y_continuous
#'                     scale_color_manual coord_fixed theme_bw theme
#' @importFrom magick image_read
#' @importFrom grid rasterGrob
#'
#' @examples 
#' \dontrun{
#' volcano3d <- reshape::melt(volcano)
#' names(volcano3d) <- c("x", "y", "z")
#' contourPlot2(volcano3d, transparency = 1, 
#'              levels = c(80, 100, 120, 140, 160, 180, 200, Inf))
#' }
#' @export
#' 
contourPlot2 <- function(data,
                         x = "x",
                         y = "y",
                         z = "z",
                         domain = NULL,
                         background = NULL, 
                         underlayer = NULL,
                         overlayer = NULL,
                         legend = NULL, 
                         levels = NULL,
                         size = 0.,
                         fill = TRUE,
                         transparency = 0.75,
                         colors = NULL,
                         bare = FALSE) {

    # Check input data
    data <- data[, c(x, y, z)]
    vars <- c(x, y, z)
    for (i in seq_along(vars)) {
        if (!is.numeric(data[[vars[i]]])) {
            data[[vars[i]]] <- as.numeric(data[[vars[i]]])
        }
    }
    colnames(data) <- c("x", "y", "z")
    
    # Define plot domain
    if (missing(domain)) {
        xmin <- min(data$x)     # x coordinates minimum
        xmax <- max(data$x)     # x coordinates max
        ymin <- min(data$y)     # y coordinates min
        ymax <- max(data$y)     # y coordinates max
        nx <- 5                 # number of ticks along x axis
        ny <- 5                 # number of ticks along y axis
    } else {
        xmin <- domain[1]       # x coordinates minimum
        xmax <- domain[2]       # x coordinates max
        ymin <- domain[3]       # y coordinates min
        ymax <- domain[4]       # y coordinates max
        nx <- domain[5]         # number of ticks along x axis
        ny <- domain[6]         # number of ticks along y axis
    }

    # prettify legend title
    if (requireNamespace("openair", quietly = TRUE)) {
        lgndname <- openair::quickText(legend, auto.text = T)
    } else {
        lgndname <- legend
    }

    # Automatic scales
    if (missing(levels)) {
        if (is.null(colors)) {
            nlevels <- 7
        } else {
            nlevels <- length(colors)
        }
        levels <- pretty(range(data$z, na.rm = T), n = nlevels, min.n = 4)
    } 

    # labels for legend
    nlevels <- length(levels)
    if (levels[nlevels] != Inf) {
        levels <- append(levels, Inf)
        nlevels <- length(levels)
    }
    lab_levels <- paste(levels[1:(nlevels - 1)], "\U2013", levels[2:nlevels])
    if (levels[nlevels] == Inf) {
        lab_levels[nlevels - 1] <- paste(">", levels[nlevels - 1])
    }
    if (levels[1] == -Inf) {
        lab_levels[1] <- paste("<", levels[2])
    }
    
    # Colour palette 
    if (is.null(colors)) {
        myPalette <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11, name = "Spectral")))
        # Omit first colour for aesthetic reasons
        myColors <- myPalette(nlevels)
        myColors <- myColors[2:length(myColors)]
        myColorsLines <- cbind(myColors, "black")
    } else {
        myPalette = grDevices::colorRampPalette(colors, alpha = T)
        myColors <- myPalette(length(levels) - 1)
        myColorsLines <- cbind(myColors, "black")
    }
    
    # Background image
    if (missing(background)) {
        img <- matrix(data = NA, nrow = 10, ncol = 10)
        gimg <- grid::rasterGrob(img, interpolate = T)
    } else {
        img <- magick::image_read(background)
        gimg <- grid::rasterGrob(img)
    }
    
    # Underlayer
    if (missing(underlayer)) {
        underlayer <- geom_blank()
    }
    
    # Overlayer
    if (missing(overlayer)) {
        overlayer <- geom_blank()
    }
    
    # If fill is FALSE and size is 0 we set a default value for size
    if (isFALSE(fill) & size == 0) {
        size = 0.5
    }
    
    # Contour plot
    v <- ggplot(data) +
        annotation_custom(gimg, -Inf, Inf, -Inf, Inf)  +
        underlayer
    
    if (isTRUE(fill)) {
        v <- v +
            geom_contour_filled(aes(x = x, y = y, z = z,
                                    fill = stat(level)),
                                breaks = levels,
                                size = 0,
                                alpha = transparency) +
            scale_fill_manual(lgndname,
                              drop = FALSE,
                              guide = guide_legend(reverse = TRUE),
                              labels = lab_levels,
                              values = myColors)
    }

    # Contour lines
    if (size != 0) {
        lineLevels <- levels
        if (levels[length(levels)] == "Inf") {
            lineLevels <- levels[1:length(levels) - 1]
        }
        v <- v +
            geom_contour(aes(x = x, 
                             y = y, 
                             z = z, 
                             colour = factor(stat(level))),
                         breaks = lineLevels,
                         size = size,
                         linejoin = "round",
                         lineend = "round",
                         alpha = 1.,
                         show.legend = isFALSE(fill)) +
            scale_color_manual(lgndname,
                               drop = FALSE,
                               limits = factor(lineLevels),
                               guide = guide_legend(reverse = TRUE),
                               values = myColorsLines)
    }

    # Main scales and theme
    v <- v +
        scale_x_continuous(name = "x [m]",
                           breaks = seq(xmin, xmax, length.out = nx),
                           labels = myCoordsLabels,
                           expand = c(0, 0)) +
        scale_y_continuous(name = "y [m]",
                           breaks = seq(ymin, ymax, length.out = ny),
                           labels = myCoordsLabels,
                           expand = c(0, 0)) +
        overlayer +
        coord_fixed(ratio = 1,
                    xlim = c(xmin, xmax),
                    ylim = c(ymin, ymax)) +
        theme_bw(base_size = 10, 
                 base_family = "Arial") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

    # If requested, wipe all but main plot 
    if (isTRUE(bare)) {
        v <- v + theme(axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.position = "none",
                       panel.background = element_blank(),
                       panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.background = element_blank())
    }

    return(v)
}