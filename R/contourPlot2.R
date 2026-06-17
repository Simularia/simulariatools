#' Contour Plot of pollutant concentration field
#'
#' @description
#'
#' The function \code{contourPlot2} generates a contour plot of a scalar quantity,
#' such as the ground concentration of an airborne pollutant or odour, defined on a
#' regular grid.
#'
#' @param data A dataframe in long format with three columns for Easting,
#' Northing and values to be plotted.
#' @param x character. Name of the column containing Easting (longitude)
#' coordinates (default "x").
#' @param y character. Name of the column containing Northing (latitude)
#' coordinates (default "y").
#' @param z character. Name of the column containing concentration values
#' (default "z").
#' @param domain optional list of six numeric values defining the boundaries of
#' the domain to be plotted and the number of ticks on X & Y axis
#' (minimum X, maximum X, minimum Y, maximum Y, number of ticks on X axis,
#' number of ticks on Y axis). Example: c(340000, 346000, 4989500, 4995500, 5, 5).
#' If missing, the full domain of the input data is considered, with 5 ticks
#' (deprecated, see `xlim`, `ylim`, `nticks`).
#' @param xlim optional list of two numeric values defining the abscissa axis boundaries
#' of the plot (minimum x, maximum x).
#' @param ylim optional list of two numeric values defining the ordinate axis boundaries
#' of the plot (minimum y, maximum y).
#' @param nticks optional list of one or two numeric integers defining the number
#' of ticks on X & Y axes. If a single number is given, the same number of ticks
#' is plotted on both axes (default = 5 ticks).
#' @param background filename. Optional path to a raster file to be plotted as
#' the basemap (deprecated, see `basemap`).
#' @param basemap filename. Optional path to a raster file to be plotted as
#' the basemap (see Details).
#' @param underlayer optional list of layers to be plotted between basemap
#' and contour plot. See Details.
#' @param overlayer optional list of layers to be plotted on top of the contour
#' plot. See Details.
#' @param legend character. Optional title of the legend.
#' @param levels numeric vector of levels for contour plot. If not set,
#' automatic pretty levels are computed. If `-Inf` and `Inf` are used
#' as the lowest and highest limits of the array, the lowest and highest bands
#' are unbounded and the legend shows `<` and `>=` symbols.
#' @param size numeric. Width of the contour line.
#' @param fill logical. If TRUE, the contour plot is filled with colour (default = TRUE).
#' @param contour_labels logical. If TRUE and both fill and tile are FALSE,
#' level values are displayed along the contour lines. Default = FALSE.
#' @param tile logical. If TRUE, rectangular tiles are plotted (default = FALSE).
#' @param transparency transparency level of the contour plot between 0.0
#' (fully transparent) and 1.0 (fully opaque). Default = 0.75.
#' @param colors colour palette for contour plot, as an array of colours.
#' @param mask character. Path to `shp` file used as a mask. It must be a closed polygon.
#' @param inverse_mask logical. If TRUE, areas on mask are masked. Default is
#' to mask areas outside the polygon defined in the _shp_ file.
#' @param bare boolean (default FALSE). Deprecated in favour of `theme_void`.
#' @param theme_void boolean (default FALSE). If TRUE, only the bare plot is shown:
#' axis, legend, titles and any other graphical element of the plot are removed.
#'
#' @details
#'
#' This is a convenience function to plot contour levels of a scalar quantity
#' such as pollutants computed by a dispersion model, with \code{ggplot2}
#' version >= 3.3.0.
#'
#' Data are required to be on a regular grid, typically (but not necessarily)
#' in UTM coordinates. Each value is associated with the cell centre.
#' The input dataframe has to be in long format, i.e. one line per value to be plotted.
#' The names of the columns corresponding to `x`, `y` and `z` can be specified in the
#' input parameters.
#'
#' The `basemap` can be a geo-referenced TIFF file. In that case, the plot bounding box
#' is automatically derived from the picture extent. The axis limits can be explicitly
#' overridden by `xlim` and `ylim` arguments.
#'
#' If `tile = TRUE` data are shown as they are, without any graphical
#' interpolation required for contour plots. This is helpful when you want to
#' visualise the raw data.
#' Since version 2.4.0, when `tile = TRUE` the intervals include the lowest
#' bound and exclude the highest bound: `[min, max)`. Note: In previous versions
#' it was the opposite.
#'
#' `underlayer` and `overlayer` layers are \code{ggplot2} objects to be shown at
#' different levels of the vertical stack of the plot. These are useful to
#' show topographical information related to the plot, such as sources
#' or receptors locations.
#'
#' When a _shp_ file is given to the `mask` argument, the plot is drawn only
#' inside the polygon. In order to avoid boundary artifacts due to reduced
#' resolution, original data are resampled to higher resolution (currently
#' set to 10 times the original one). If `inverse_mask` is set to `TRUE`, the plot
#' is drawn outside the polygon. The `mask` feature is based on the
#' [terra::mask()] function.
#' The CRS of the _shp_ file is applied to the data in the data.frame.
#' Please keep in mind this feature is still experimental.
#'
#' @return A \code{ggplot2} object.
#'
#' @importFrom grDevices colorRampPalette contourLines
#' @importFrom ggplot2 ggplot annotation_raster geom_contour_filled
#' @importFrom ggplot2 scale_fill_manual scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_color_manual coord_fixed theme_bw theme
#' @importFrom ggplot2 geom_blank guide_legend geom_raster after_stat
#' @importFrom ggplot2 geom_contour geom_label labs element_blank
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
    contour_labels = FALSE,
    tile = FALSE,
    transparency = 0.75,
    colors = NULL,
    mask = NULL,
    inverse_mask = FALSE,
    bare = FALSE,
    theme_void = FALSE
) {
    # Fix check() no visible binding for global variable
    angle <- NULL
    label <- NULL

    # Consistency check
    if (isTRUE(tile)) {
        fill <- FALSE
        theme_void <- FALSE
        size <- 0.
        contour_labels <- FALSE
    }

    # Check if columns exist
    colNames <- colnames(data)
    lapply(list(x, y, z), function(nm) {
        if (isFALSE(nm %in% colNames)) {
            stop(paste("Column", nm, "is missing"), call. = FALSE)
        }
    })

    # Input data as numeric and rename columns
    data <- data.frame(
        x = as.numeric(data[[x]]),
        y = as.numeric(data[[y]]),
        z = as.numeric(data[[z]])
    )

    # Deprecate bare argument
    if (isTRUE(bare)) {
        theme_void <- bare
        warning(paste(
            "The \`bare\` argument is deprecated.",
            "Please use argument \`theme_void\` instead."
        ))
    }

    # Default limits for basemap
    xmin_im <- -Inf
    xmax_im <- Inf
    ymin_im <- -Inf
    ymax_im <- Inf

    # Init basemap extent (NULL when no basemap)
    basemap_extent <- NULL

    # Deprecate background
    if (!is.null(background)) {
        warning(paste(
            "The \`background\` argument is deprecated.",
            "Please use the \'basemap\' argument instead."
        ))
        # if we have both background and basemap, the latter has the precedence
        if (is.null(basemap)) {
            basemap <- background
        }
    }

    # If we have a basemap, try to get boundaries from it
    # Otherwise get them from data
    if (!is.null(basemap)) {
        imgr <- tryCatch(
            terra::rast(basemap),
            warning = function(w) w
        )
        if (inherits(imgr, "SpatRaster")) {
            # Bounding box for plot
            basemap_extent <- c(
                terra::xmin(imgr),
                terra::xmax(imgr),
                terra::ymin(imgr),
                terra::ymax(imgr)
            )
            # Bounding box for basemap image (same)
            xmin_im <- basemap_extent[1]
            xmax_im <- basemap_extent[2]
            ymin_im <- basemap_extent[3]
            ymax_im <- basemap_extent[4]
        }

        # [FIXME] avoid reading the image again, for performance reasons
        # Issue is that terra::rast() does not composite the color bands
        # If you try composite the color bands without image_read() the
        # performance can be work than reading again the image.
        if (requireNamespace("magick", quietly = TRUE)) {
            img <- magick::image_read(basemap)
            gimg <- terra::as.raster(img)
        } else {
            stop(
                "Missing magick package. Please install it to read the basemap file."
            )
        }
    }

    # If required, override plot limits
    if (!is.null(domain)) {
        warning(paste(
            "The \`domain\` argument is deprecated.",
            "Please use the \'xlim\', \'ylim\' and \'nticks\' arguments instead."
        ))
        xlim <- domain[1:2]
        ylim <- domain[3:4]
        nticks <- domain[5:6]
    }

    # Compute plot axis limits and tick counts
    bounds <- computeAxisBounds(
        data,
        xlim = xlim,
        ylim = ylim,
        nticks = nticks,
        basemap_extent = basemap_extent
    )
    xmin <- bounds$xmin
    xmax <- bounds$xmax
    ymin <- bounds$ymin
    ymax <- bounds$ymax
    nx <- bounds$nx
    ny <- bounds$ny

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
    if (levels[nlevels] == Inf) {
        lab_levels[nlevels - 1] <- parse(
            text = paste("\"\">=", pretty_levels[nlevels - 1])
        )
    }
    if (levels[1] == -Inf) {
        lab_levels[1] <- parse(text = paste("\"\" <", pretty_levels[2]))
    }

    # Internal band labels, reproducing exactly the ordered factor levels that
    # `geom_contour_filled()` generates (ggplot2:::pretty_isoband_levels). They
    # are used as the fill scale `limits` so the legend always lists every band
    # and colours map to the correct band, regardless of the data range. The
    # labelling must follow `format()` (not raw number printing): otherwise the
    # strings would not match the factor levels and bands would render with the
    # scale's `na.value` (grey). `dig_lab` starts at 3 and grows until the
    # formatted breaks are distinct, mirroring ggplot2's behaviour.
    dig_lab <- 3
    while (anyDuplicated(format(unique(levels), digits = dig_lab, trim = TRUE))) {
        dig_lab <- dig_lab + 1
    }
    band_limits <- sprintf(
        "(%s, %s]",
        format(levels[-nlevels], digits = dig_lab, trim = TRUE),
        format(levels[-1], digits = dig_lab, trim = TRUE)
    )

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
    } else {
        my_palette <- grDevices::colorRampPalette(colors, alpha = TRUE)
        my_colors <- my_palette(length(levels) - 1)
    }

    # Mask
    if (!is.null(mask)) {
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

    # Underlayer
    if (is.null(underlayer)) {
        underlayer <- geom_blank()
    }

    # Overlayer
    if (is.null(overlayer)) {
        overlayer <- geom_blank()
    }

    # If fill is FALSE and size is 0 we set a default value for size
    if (isFALSE(fill) && size == 0) {
        size <- 0.5
    }
    if (isTRUE(tile)) {
        size <- 0.
    }

    # Basemap
    v <- ggplot(data)
    if (!is.null(basemap)) {
        v <- v + annotation_raster(gimg, xmin_im, xmax_im, ymin_im, ymax_im)
    }

    # Fix the x, y scales to the plot boundaries with an invisible layer.
    # This guarantees the axis breaks (computed from xmin/xmax/ymin/ymax) are always
    # within the scale range and never dropped. Without it, when the contour
    # layer is empty (e.g. the data range is entirely outside `levels`) the
    # scales stay undtermined and the axis ticks and labels disappear.
    v <- v +
        geom_blank(
            data = data.frame(x = c(xmin, xmax), y = c(ymin, ymax)),
            aes(x = x, y = y)
        )

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
                limits = band_limits,
                guide = guide_legend(reverse = TRUE),
                labels = lab_levels,
                values = my_colors
            )
    }

    # Contour lines
    if (size != 0) {
        line_levels <- levels

        if (is.infinite(line_levels[length(line_levels)])) {
            line_levels <- line_levels[-length(line_levels)]
        }

        if (is.infinite(line_levels[1])) {
            line_levels <- line_levels[-1]
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
                values = my_colors
            )
    }

    # Contour labels
    if (isFALSE(fill) && isTRUE(contour_labels)) {
        # Build matrix for contourLines
        x_unique <- sort(unique(data$x))
        y_unique <- sort(unique(data$y))
        z_matrix <- matrix(NA, nrow = length(x_unique), ncol = length(y_unique))
        for (i in seq_len(nrow(data))) {
            xi <- match(data$x[i], x_unique)
            yi <- match(data$y[i], y_unique)
            z_matrix[xi, yi] <- data$z[i]
        }

        # Extract contour lines and build df with point/label/angle
        cl <- contourLines(x_unique, y_unique, z_matrix, levels = line_levels)
        label_data <- do.call(
            rbind,
            lapply(cl, function(cLine) {
                # Get middle point of each contour line
                n <- length(cLine$x)
                mid <- ceiling(n / 2)
                # Calcuate angle from adjacent points
                i1 <- max(1, mid - 1)
                i2 <- min(n, mid + 1)
                dx <- cLine$x[i2] - cLine$x[i1]
                dy <- cLine$y[i2] - cLine$y[i1]
                angle <- atan2(dy, dx) * 180 / pi
                # Normalize so that text is always readable
                if (angle > 90) {
                    angle <- angle - 180
                }
                if (angle < -90) {
                    angle <- angle + 180
                }
                data.frame(
                    x = cLine$x[mid],
                    y = cLine$y[mid],
                    label = cLine$level,
                    angle = angle
                )
            })
        )

        v <- v +
            geom_label(
                data = label_data,
                aes(x = x, y = y, label = label, angle = angle),
                size = 3,
                border.colour = NA,
                fill = "#FFFFFFFF",
                colour = "darkgrey"
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
    if (isTRUE(theme_void)) {
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

# Compute plot axis limits and tick counts for contourPlot2().
# Precedence (low -> high):
# - data range < basemap extent < xlim/ylim.
# -`basemap_extent`, when supplied, is c(xmin, xmax, ymin, ymax) from the raster.
#
# Returns list(xmin, xmax, ymin, ymax, nx, ny).
#
computeAxisBounds <- function(
    data,
    xlim = NULL,
    ylim = NULL,
    nticks = 5,
    basemap_extent = NULL
) {
    # Default limits from data range
    xmin <- min(data$x)
    xmax <- max(data$x)
    ymin <- min(data$y)
    ymax <- max(data$y)

    # Basemap extent overrides data defaults
    if (!is.null(basemap_extent)) {
        xmin <- basemap_extent[1]
        xmax <- basemap_extent[2]
        ymin <- basemap_extent[3]
        ymax <- basemap_extent[4]
    }

    # Explicit limits take precedence
    if (!is.null(xlim)) {
        xmin <- xlim[1]
        xmax <- xlim[2]
        if (xmax <= xmin) stop("Check xlim argument: max(x) <= min(x)")
    }
    if (!is.null(ylim)) {
        ymin <- ylim[1]
        ymax <- ylim[2]
        if (ymax <= ymin) stop("Check ylim argument: max(y) <= min(y)")
    }

    # Tick counts (single value applies to both axes)
    if (length(nticks) == 1) {
        nx <- nticks
        ny <- nticks
    } else {
        nx <- nticks[1]
        ny <- nticks[2]
    }

    list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, nx = nx, ny = ny)
}

