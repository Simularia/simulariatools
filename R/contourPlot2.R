#' New contour plot of pollutant concentration field
#'
#' \code{contourPlot2} plots a contour map of a given quantity, such as the
#' ground concentration of an airborne pollutant or odour, defined on a
#' regular grid.
#'
#' @param data dataframe in long format, with three columns for Easting,
#' Northing and values to be plotted.
#' @param x name of the column with Easting data (default "x").
#' @param y name of the column with Northing data (default "y").
#' @param z name of the column with the values to be plotted (default
#' "z").
#' @param domain optional list with six numeric values defining the
#' boundaries of the domain to be plotted: minimum X, maximum X, minimum Y,
#' maximum Y, number of ticks on X axis, number of ticks on Y axis.
#' @param background optional path to a png file to be plotted as the base map.
#' @param underlayer optional list of layers to be plotted between base map
#' and contour plot.
#' @param overlayer optional list of layers to be plotted on top of the contour
#' plot.
#' @param legend optional title of the legend.
#' @param levels numeric vector of levels for contour plot. If not set,
#' automatic pretty levels are computed. If `-Inf` and `Inf` are used as the lowest
#' and highest limits of the array, the lowest and highest bands are unbounded
#' and the legend shows `<` and `>=` symbols.
#' @param transparency transparency level of the contour plot between 0.0
#' (fully transparent) and 1.0 (fully opaque). Default = 0.75).
#' @param colors colour palette for contour plot, as an array of colours.
#' @param bare boolean (default FALSE). If TRUE only the bare plot is shown:
#' axis, legend, titles and any other graphical element of the plot are removed.
#' @param size thickness of the contour line.
#' @param fill boolean (default TRUE). If TRUE the contour plot is filled with
#' colour.
#' @param tile boolean (default FALSE). If TRUE rectangular tiles are plotted.
#'
#' @details
#'
#' This is a convenience function to plot contour levels of a scalar quantity
#' such as pollutants computed by a dispersion model, with \code{ggplot2}
#' version >= 3.3.0.
#'
#' Data are required to be on a regular grid, typically (but not necessarily)
#' in UTM coordinates. The input dataframe has to be in long format, i.e. one
#' line per value to be plotted. The names of the columns corresponding to `x`,
#' `y` and `z` can be specified in the input parameters.
#'
#' If `tile = TRUE` data are shown as they are, without any graphical
#' interpolation required for contour plots. This is helpful when you want to
#' visualise the raw data.
#' Since version 2.4.0, when `tile = TRUE` the intervals include the lowest
#' bound and exclude the highest bound: `[min, max)`. Note: In previous version
#' it was the opposite.
#'
#' `underlayer` and `overlayer` layers are `ggplot2` objects to be shown at
#' different levels of the vertical stack of the plot. These are useful to
#' show topographical information related to the plot, such as sources
#' or receptors locations.
#'
#'
#' @return A \code{ggplot2} object.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot annotation_custom geom_contour_filled
#'                     scale_fill_manual scale_x_continuous scale_y_continuous
#'                     scale_color_manual coord_fixed theme_bw theme
#' @importFrom grid rasterGrob
#'
#' @export
#'
#' @examples
#' # Load example data in long format
#' data(volcano)
#' volcano3d <- reshape2::melt(volcano)
#' names(volcano3d) <- c("x", "y", "z")
#' # Contour plot with default options
#' v <- contourPlot2(volcano3d)
#' v
#'
#' # Set levels, and properly format the legend title:
#' contourPlot2(volcano3d,
#'              levels = c(-Inf, seq(100, 200, 20), Inf),
#'              legend = expression(PM[10]~"["~mu*g~m^-3~"]"))
#'
#' # Sometimes, instead of a contour plot it is better to plot the original
#' # raster data, without any interpolation:
#' contourPlot2(volcano3d,
#'              levels = c(-Inf, seq(100, 200, 20), Inf),
#'              tile = TRUE)
#'
#' # Since contourPlot2 returns a `ggplot2` object, you can add instructions as:
#' library(ggplot2)
#' v + ggtitle("Example volcano data") +
#'     labs(x = NULL, y = NULL)
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
                         tile = FALSE,
                         transparency = 0.75,
                         colors = NULL,
                         bare = FALSE) {

    # Consistency check
    if (isTRUE(tile)) {
        fill <- FALSE
        bare <- FALSE
        size <- 0.
    }

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
        lgndname <- openair::quickText(legend, auto.text = TRUE)
    } else {
        lgndname <- legend
    }

    # Automatic scales
    if (is.null(levels)) {
        if (is.null(colors)) {
            nlevels <- 7
        } else {
            nlevels <- length(colors)
        }
        levels <- pretty(range(data$z, na.rm = TRUE), n = nlevels, min.n = 4)
    }

    # Labels for legend
    nlevels <- length(levels)
    if (levels[1] >= 0 & levels[nlevels] != Inf) {
        levels <- append(levels, Inf)
        nlevels <- length(levels)
    }
    prettyLevels <- prettyNum(levels)
    lab_levels <- parse(text = paste(prettyLevels[1:(nlevels - 1)], "-", prettyLevels[2:nlevels]))
    if (levels[nlevels] == Inf & !isTRUE(tile)) {
        lab_levels[nlevels - 1] <- parse(text = paste("\"\">=", prettyLevels[nlevels - 1]))
    } else if (levels[nlevels] == Inf & isTRUE(tile)) {
        lab_levels[nlevels - 1] <- parse(text = paste("\"\">=", prettyLevels[nlevels - 1]))
    }
    if (levels[1] == -Inf) {
        lab_levels[1] <- parse(text = paste("\"\" <", prettyLevels[2]))
    }

    # Colour palette
    spectral <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598", "#FFFFBF",
                  "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")
    if (is.null(colors)) {
        myPalette <- grDevices::colorRampPalette(spectral)
        # Omit first colour for aesthetic reasons
        myColors <- myPalette(nlevels)
        myColors <- myColors[2:length(myColors)]
        myColorsLines <- cbind(myColors, "black")
    } else {
        myPalette <- grDevices::colorRampPalette(colors, alpha = TRUE)
        myColors <- myPalette(length(levels) - 1)
        myColorsLines <- cbind(myColors, "black")
    }

    # Background image
    img <- matrix(data = NA, nrow = 10, ncol = 10)
    gimg <- grid::rasterGrob(img, interpolate = FALSE)
    if (!missing(background)) {
        if (requireNamespace("magick", quietly = TRUE)) {
            img <- magick::image_read(background)
            gimg <- grid::rasterGrob(img)
        } else {
            warning("Missing magick package. Please install it to be able to read background basemap.")
        }
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
        size <- 0.5
    }
    if (isTRUE(tile)) {
        size <- 0.
        # data$z <- factor(data$z, levels = levels)
    }

    # Base layer
    v <- ggplot(data) +
        annotation_custom(gimg, -Inf, Inf, -Inf, Inf)  +
        underlayer

    # Tile plot
    if (isTRUE(tile)) {
        v <- v +
            geom_raster(aes(x = x, y = y,
                            fill = cut(z, breaks = levels, right = FALSE)),
                        alpha = transparency) +
            scale_fill_manual(lgndname,
                              drop = FALSE,
                              guide = guide_legend(reverse = TRUE),
                              labels = lab_levels,
                              values = myColors)
    }

    # Contour plot
    if (isTRUE(fill)) {
        v <- v +
            geom_contour_filled(aes(x = x, y = y, z = z,
                                    fill = after_stat(level)),
                                breaks = levels,
                                linewidth = 0,
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
                             colour = factor(after_stat(level))),
                         breaks = lineLevels,
                         linewidth = size,
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
        scale_x_continuous(breaks = seq(xmin, xmax, length.out = nx),
                           labels = myCoordsLabels) +
        scale_y_continuous(breaks = seq(ymin, ymax, length.out = ny),
                           labels = myCoordsLabels) +
        labs(x = "x [m]", y = "y [m]") +
        overlayer +
        coord_fixed(ratio = 1,
                    expand = FALSE,
                    xlim = c(xmin, xmax),
                    ylim = c(ymin, ymax)) +
        theme_bw(base_size = 10,
                 base_family = "sans") +
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
