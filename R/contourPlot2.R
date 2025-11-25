#' Contour Plot of pollutant concentration field
#'
#' @description
#'
#' The function \code{contourPlot2} generates a contour plot of a given quantity,
#' such as the ground concentration of an airborne pollutant or odour, defined on a
#' regular grid.
#'
#' @param data A dataframe in long format with three columns for Easting,
#' Northing and values to be plotted.
#' @param x charactrer. Name of the column containing Easting (longitude)
#' coordinates (default "x").
#' @param y character. Name of the column containing Northing (latitude)
#' coordinates (default "y").
#' @param z character. Name of the column containing concentration values
#' (default "z").
#' @param domain optional list of six numeric values defining the boundaries of
#' the domain to be plotted (minimum X, maximum X, minimum Y, maximum Y) and the
#' number of ticks on X & Y axis.
#' Example: c(340000, 346000, 4989500, 4995500, 5, 5).
#' If missing, all the full domain of the input data is considered, with 5 ticks
#' (deprecated, see `xlim`, `ylim`, `nticks`).
#' @param xlim optional list of two numeric values defining the abscissa axis boundaries
#' of the plot (minimum x, maximum x).
#' @param ylim optional list of two numeric values defining the ordinata axis boundaries
#' of the plot (minimum y, maximum y).
#' @param nticks optional listo of one or two numeric integers defining the number
#' of ticks on X & Y axes. If a single number is given, the same number of ticks
#' is plotted on both axes (default = 5 ticks).
#' @param background filename. Optional path to a raster file to be plotted as
#' the basemap (deprecated, see `basemap`)
#' @param basemap filename. Optional path to a raster file to be plotted as
#' the basemap (see Details)
#' @param underlayer optional list of layers to be plotted between base map
#' and contour plot. See Details
#' @param overlayer optional list of layers to be plotted on top of the contour
#' plot. See Details.
#' @param legend character. Optional title of the legend.
#' @param levels numeric vector of levels for contour plot. If not set,
#' automatic pretty levels are computed. If `-Inf` and `Inf` are used
#' as the lowest and highest limits of the array, the lowest and highest bands
#' are unbounded and the legend shows `<` and `>=` symbols.
#' @param transparency transparency level of the contour plot between 0.0
#' (fully transparent) and 1.0 (fully opaque). Default = 0.75).
#' @param colors colour palette for contour plot, as an array of colours.
#' @param bare boolean (default FALSE). If TRUE only the bare plot is shown:
#' axis, legend, titles and any other graphical element of the plot are removed.
#' @param size numeric. Width of the contour line.
#' @param fill logical. If TRUE the contour plot is filled with colour (default = TRUE).
#' @param tile logical. If TRUE rectangular tiles are plotted (default = FALSE).
#' @param mask character. Path to _shp_ file used as a mask. It must be a closed polygon.
#' @param inverse_mask logical. If `TRUE` areas on mask are masked. Default is
#' to mask areas outside the polygon defined in the _shp_ file.
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
#' The `basemap` can be a geo-referenced TIFF file. In that case, the plot limits
#' are automatically extracted from the picture extent. The limits can be explicitly
#' overridden by `xlim` and `ylim` arguments.
#'
#' `underlayer` and `overlayer` layers are \code{ggplot2} objects to be shown at
#' different levels of the vertical stack of the plot. These are useful to
#' show topographical information related to the plot, such as sources
#' or receptors locations.
#'
#' When a _shp_ file is given to the `mask` argument, the plot is drawn only
#' inside the polygon. In order to avoid boundary artifacts due to reduced
#' resolution, original data are resampled to higher resolution (currently
#' set to 10 times the original one). If`inverse_mask` is set to `TRUE`, the plot
#' is drawn outside the polygon. The *mask* feature is based on the
#' [terra::mask()] function.
#' The CRS of the _shp_ file is applied to the data in the data.frame.
#' Please keep in mind this feature is still experimental.
#'
#' @return A \code{ggplot2} object.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot annotation_raster geom_contour_filled
#'                     scale_fill_manual scale_x_continuous scale_y_continuous
#'                     scale_color_manual coord_fixed theme_bw theme
#'                     geom_blank guide_legend geom_raster after_stat
#'                     geom_contour labs element_blank
#' @importFrom grid rasterGrob
#' @importFrom terra crs mask rast as.raster ext resample xmin xmax ymin ymax
#'
#' @export
#'
#' @examples
#' # Load example data in long format
#' data(volcano)
#' volcano <- as.data.frame(volcano)
#' volcano3d <- reshape(volcano, direction = "long",
#'   varying = list(1:61),
#'   idvar = "x", timevar = "y", v.names = "z")
#' # Contour plot with default options
#' v <- contourPlot2(volcano3d)
#' v
#'
#' # Set levels, and properly format the legend title:
#' contourPlot2(
#'     volcano3d,
#'     levels = c(-Inf, seq(100, 200, 20), Inf),
#'     legend = expression("PM"[10] ~ "[" * mu * "g m"^-3 * "]")
#' )
#'
#' # Sometimes, instead of a contour plot it is better to plot the original
#' # raster data, without any interpolation:
#' contourPlot2(
#'     volcano3d,
#'     levels = c(-Inf, seq(100, 200, 20), Inf),
#'     tile = TRUE
#' )
#'
#' # Since contourPlot2 returns a `ggplot2` object, you can add instructions as:
#' library(ggplot2)
#' v +
#'     ggtitle("Example volcano data") +
#'     labs(x = NULL, y = NULL)
#'
contourPlot2 <- function(
    data,
    x = "x",
    y = "y",
    z = "z",
    domain = NULL,
    xlim = NULL,
    ylim = NULL,
    nticks = 5,
    background = NULL,
    basemap = NULL,
    underlayer = NULL,
    overlayer = NULL,
    legend = NULL,
    levels = NULL,
    size = 0.,
    fill = TRUE,
    tile = FALSE,
    transparency = 0.75,
    colors = NULL,
    mask = NULL,
    inverse_mask = FALSE,
    bare = FALSE
) {
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

    # Default limits for basemap
    xmin_im <- -Inf
    xmax_im <- Inf
    ymin_im <- -Inf
    ymax_im <- Inf

    # Default limits for plot
    xmin <- min(data$x)
    xmax <- max(data$x)
    ymin <- min(data$y)
    ymax <- max(data$y)

    # If we have a basemap, try to get boundaries from it
    # Otherwise get them from data
    if (!missing(basemap)) {
        imgr <- tryCatch(
            terra::rast(basemap),
            warning = function(w) w
        )
        if (inherits(imgr, "SpatRaster")) {
            xmin <- terra::xmin(imgr)
            xmax <- terra::xmax(imgr)
            ymin <- terra::ymin(imgr)
            ymax <- terra::ymax(imgr)
            xmin_im <- xmin
            xmax_im <- xmax
            ymin_im <- ymin
            ymax_im <- ymax
        }
    }

    # If required, override plot limits
    if (!missing(domain)) {
        warning(paste(
            "The \`domain\` argument is deprecated.",
            "Please use the \'xlim\', \'ylim\' and \'nticks\' arguments instead."
        ))
        xlim <- domain[1:2]
        ylim <- domain[3:4]
        nticks <- domain[5:6]
    }

    if (!missing(xlim)) {
        xmin <- xlim[1] # x coordinates minimum
        xmax <- xlim[2] # x coordinates max
        if (xmax <= xmin) stop("Check xlim argument: max(x) <= min(x)")
    }

    if (!missing(ylim)) {
        ymin <- ylim[1] # y coordinates min
        ymax <- ylim[2] # y coordinates max
        if (ymax <= ymin) stop("Check ylim argument: max(y) <= min(y)")
    }

    if (length(nticks) == 1) {
        nx <- nticks # number of ticks along x axis
        ny <- nticks # number of ticks along y axis
    } else {
        nx <- nticks[1] # number of ticks along x axis
        ny <- nticks[2] # number of ticks along y axis
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
    if (levels[1] >= 0 && levels[nlevels] != Inf) {
        levels <- append(levels, Inf)
        nlevels <- length(levels)
    }
    pretty_levels <- prettyNum(levels)
    lab_levels <- parse(
        text = paste(
            pretty_levels[1:(nlevels - 1)],
            "-",
            pretty_levels[2:nlevels]
        )
    )
    if (levels[nlevels] == Inf && !isTRUE(tile)) {
        lab_levels[nlevels - 1] <- parse(
            text = paste("\"\">=", pretty_levels[nlevels - 1])
        )
    } else if (levels[nlevels] == Inf && isTRUE(tile)) {
        lab_levels[nlevels - 1] <- parse(
            text = paste("\"\">=", pretty_levels[nlevels - 1])
        )
    }
    if (levels[1] == -Inf) {
        lab_levels[1] <- parse(text = paste("\"\" <", pretty_levels[2]))
    }

    # Colour palette
    spectral <- c(
        "#5E4FA2",
        "#3288BD",
        "#66C2A5",
        "#ABDDA4",
        "#E6F598",
        "#FFFFBF",
        "#FEE08B",
        "#FDAE61",
        "#F46D43",
        "#D53E4F",
        "#9E0142"
    )
    if (is.null(colors)) {
        my_palette <- grDevices::colorRampPalette(spectral)
        # Omit first colour for aesthetic reasons
        my_colors <- my_palette(nlevels)
        my_colors <- my_colors[2:length(my_colors)]
        my_colors_lines <- my_colors
    } else {
        my_palette <- grDevices::colorRampPalette(colors, alpha = TRUE)
        my_colors <- my_palette(length(levels) - 1)
        my_colors_lines <- my_colors
    }

    # Mask
    if (!missing(mask)) {
        if (!requireNamespace("sf", quietly = TRUE)) {
            stop(
                "Please install the `sf` package to use `mask`.",
                call. = FALSE
            )
        } else {
            mask_polygon <- sf::st_read(mask, quiet = TRUE)
            rdata <- terra::rast(
                data,
                type = "xyz",
                crs = terra::crs(mask_polygon)
            )

            # Resample
            rdata_ext <- terra::ext(rdata)
            resample_res <- terra::res(rdata) / 10
            resample_target <- terra::rast(
                extent = rdata_ext,
                resolution = resample_res
            )
            rdata <- terra::resample(rdata, resample_target)

            # mask
            rdata <- terra::mask(
                x = rdata,
                mask = mask_polygon,
                inverse = inverse_mask
            )
            data <- terra::as.data.frame(rdata, xy = TRUE)
            colnames(data) <- c("x", "y", "z")
        }
    }

    # Background image
    if (!missing(background)) {
        warning(paste(
            "The \`background\` argument is deprecated.",
            "Please use the \'basemap\' argument instead."
        ))
        basemap <- background
    }

    if (!missing(basemap)) {
        if (requireNamespace("magick", quietly = TRUE)) {
            img <- magick::image_read(basemap)
            gimg <- terra::as.raster(img)
        } else {
            warning(
                "Missing magick package. Please install it to read the basemap file."
            )
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
    if (isFALSE(fill) && size == 0) {
        size <- 0.5
    }
    if (isTRUE(tile)) {
        size <- 0.
    }

    # Base layer
    v <- ggplot(data)
    if (!missing(basemap)) {
        v <- v + annotation_raster(gimg, xmin_im, xmax_im, ymin_im, ymax_im)
    }

    # Add underlayer
    v <- v + underlayer

    # Tile plot
    if (isTRUE(tile)) {
        v <- v +
            geom_raster(
                aes(x = x, y = y, fill = cut(z, breaks = levels, right = FALSE)),
                show.legend = TRUE,
                alpha = transparency
            ) +
            scale_fill_manual(
                lgndname,
                drop = FALSE,
                guide = guide_legend(reverse = TRUE),
                labels = lab_levels,
                values = my_colors
            )
    }

    # Contour plot
    if (isTRUE(fill)) {
        v <- v +
            geom_contour_filled(
                aes(
                    x = x,
                    y = y,
                    z = z,
                    fill = after_stat(level)
                ),
                breaks = levels,
                linewidth = 0,
                show.legend = c("fill" = fill, "colour" = FALSE),
                alpha = transparency
            ) +
            scale_fill_manual(
                lgndname,
                aesthetics = c("fill"),
                drop = FALSE,
                guide = guide_legend(reverse = TRUE),
                labels = lab_levels,
                values = my_colors
            )
    }

    # Contour lines
    if (size != 0) {
        line_levels <- levels

        if (line_levels[length(line_levels)] == "Inf") {
            line_levels <- line_levels[seq_along(line_levels) - 1]
        }

        if (line_levels[1] == "-Inf") {
            line_levels <- line_levels[2:length(line_levels)]
        }

        v <- v +
            geom_contour(
                aes(x = x, y = y, z = z, colour = factor(after_stat(level))),
                breaks = line_levels,
                linewidth = size,
                linejoin = "round",
                lineend = "round",
                alpha = 1.0,
                show.legend = c("colour" = !fill)
            ) +
            scale_color_manual(
                lgndname,
                aesthetics = c("colour"),
                drop = FALSE,
                limits = factor(line_levels),
                guide = guide_legend(reverse = TRUE),
                values = my_colors_lines
            )
    }

    # Main scales and theme
    v <- v +
        scale_x_continuous(
            breaks = seq(xmin, xmax, length.out = nx),
            labels = myCoordsLabels
        ) +
        scale_y_continuous(
            breaks = seq(ymin, ymax, length.out = ny),
            labels = myCoordsLabels
        ) +
        labs(x = "x [m]", y = "y [m]") +
        overlayer +
        coord_fixed(
            ratio = 1,
            expand = FALSE,
            xlim = c(xmin, xmax),
            ylim = c(ymin, ymax)
        ) +
        theme_bw(
            base_size = 10,
            base_family = "sans"
        ) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )

    # If requested, wipe all but main plot
    if (isTRUE(bare)) {
        v <- v +
            theme(
                axis.line = element_blank(),
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
                plot.background = element_blank()
            )
    }

    return(v)
}
