#' Contour plot of pollutant concentration
#'
#' \code{contourPlot} plots a contour map of pollutants.
#'
#' This is a convenience function to plot contour levels of a pollutant matrix
#' with \code{ggplot2}.
#'
#' @param data A dataframe containing data to be plotted in the form of X, Y and
#'   Z (levels).
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
#' @param smoothness integer factor to improve the horizontal resolution 
#'   (smaller cells) by bilinear interpolation.
#' @param colors Color palette for contour plot
#' @param bare Boolean (default FALSE) parameter to completely remove axis,
#'  legend, titles and any other graphical element from the plot.
#' @param size float with the thickness of the contour line.
#' @param cover boolean (default TRUE) to specify whether the contour plot 
#'   should be filled or not.
#'
#' @return A \code{ggplot2} plot.
#' 
#' @examples
#' \dontrun{
#' # Load example data in long format
#' data(volcano)
#' volcano3d <- reshape2::melt(volcano)
#' names(volcano3d) <- c("x", "y", "z")
#' # Contour plot with default options
#' contourPlot(volcano3d)
#' 
#' # Import variable CONCAN from inpufile, convert km to m (k = 1000):
#' data <- importRaster(paste0(dir, inputfile), 
#'                      k = 1000, 
#'                      variable = "CONCAN")
#'
#' # Simple contour plot
#' contourPlot(data)
#'
#' # Specifiy (sub)domain to be plotted; background image; legend title and 
#' # pollutant levels.
#' contourPlot(data, 
#'             domain(500000, 510000, 6000000, 6010000, 7, 7), 
#'             background = "img/background.png", 
#'             legend = "no2 [ug/m3]", 
#'             levels = c(10, 20, 30, 40))
#'
#' # Add underlayer (same for overlayer)
#' library(ggplot2)
#' library(maptools)
#' perimetro <- readShapeLines("path_to/perimetro.shp")
#' perimetro <- fortify(perimetro)
#' strada <- readShapeLines("path_to/strada.shp")
#' strada <- fortify(strada)
#' myUnderlayer <- vector(mode = "list", length = 2)
#' myUnderlayer[[1]] <- geom_polygon(data = perimetro, 
#'                                   aes(long, lat, group = group), 
#'                                   colour = "black", 
#'                                   fill = NA, 
#'                                   size = 0.1, 
#'                                   alpha = 0.5)
#' myUnderlayer[[2]] <- geom_path(data = strada, 
#'                                aes(long, lat, group = group), 
#'                                colour = "grey", 
#'                                size = 0.1, 
#'                                alpha = 0.5)
#' contourPlot(data = test, 
#'             background = "path_to/basemap.png", 
#'             underlayer = myUnderlayer)
#'
#' # Change default colour palette
#' contourPlot(data = test, 
#'             colors = RColorBrewer::brewer.pal(3, name = "PiYG"))
#' }
#'
#' @import ggplot2
#' @importFrom grDevices colorRampPalette
#' @importFrom raster rasterFromXYZ disaggregate xmin xmax ymin ymax values
#'                    crop merge extent extend rasterToPoints
#' 
#' @export
#' 
contourPlot <- function(data,
                        domain = NULL,
                        background = NULL, 
                        underlayer = NULL,
                        overlayer = NULL,
                        legend = NULL, 
                        levels = NULL,
                        size = 0.,
                        cover = TRUE,
                        transparency = 0.66,
                        smoothness = 1.,
                        colors = NULL,
                        bare = FALSE) {
  
  .Deprecated("contourPlot2")

  # Local binding for variables
  x <- y <- z <- NULL
  
  # Convert input to raster
  tt <- raster::rasterFromXYZ(data)
  
    # Resample raster
    tt <- raster::disaggregate(tt, fact = smoothness, method = 'bilinear')
    
    # Define plot domain
        if (missing(domain)) {
            xmin <- raster::xmin(tt)    # x coordinates minimum
            xmax <- raster::xmax(tt)    # x coordinates max
            ymin <- raster::ymin(tt)    # y coordinates min
            ymax <- raster::ymax(tt)    # y coordinates max
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
    
    # Automatic scales
    if (missing(levels)) {
        nlevels <- 7
        levels <- pretty(range(raster::values(tt), na.rm = TRUE),
                         n = nlevels, min.n = 4)
    }
    lab_levels <- levels

    # Extend data domain to be plotted
    for (i in (1:1)) {

        for (idx in seq(1, 1)) {
            # Extend top and boottom rows
            ttx1 <- raster::crop(tt, raster::extent(tt, 1, 1, 1,
                                                    raster::ncol(tt)))
            raster::ymin(ttx1) <- raster::ymax(tt)
            raster::ymax(ttx1) <- raster::ymax(tt) + raster::res(tt)[2]
            ttxN <- raster::crop(tt, raster::extent(tt, raster::nrow(tt),
                                                    raster::nrow(tt), 1, 
                                                    raster::ncol(tt)))
            raster::ymax(ttxN) <- raster::ymin(tt)
            raster::ymin(ttxN) <- raster::ymin(tt) - raster::res(tt)[2]
            ttE <- raster::merge(tt, ttx1)
            ttE <- raster::merge(ttE, ttxN)

            # Extend left and right columns
            tty1 <- raster::crop(tt, raster::extent(tt, 1,
                                                    raster::nrow(tt), 1, 1))
            raster::xmin(tty1) <- raster::xmin(tt) - raster::res(tt)[1]
            raster::xmax(tty1) <- raster::xmin(tt)
            ttyN <- raster::crop(tt, raster::extent(tt, 1, 
                                                    raster::nrow(tt),
                                                    raster::ncol(tt),
                                                    raster::ncol(tt)))
            raster::xmin(ttyN) <- raster::xmax(tt)
            raster::xmax(ttyN) <- raster::xmax(tt) + raster::res(tt)[1]
            ttE <- raster::merge(ttE, tty1)
            ttE <- raster::merge(ttE, ttyN)
            tt <- ttE
        }
        et <- raster::extent(raster::xmin(ttE) - 1 * raster::res(tt)[1],
                             raster::xmax(ttE) + 1 * raster::res(tt)[1],
                             raster::ymin(ttE) - 1 * raster::res(tt)[2],
                             raster::ymax(ttE) + 1 * raster::res(tt)[2])
        mv <- min(raster::values(tt), na.rm = TRUE)
        j <- 1
        while (lab_levels[j] < mv) {
            j <- j + 1
        }
        if (j != 1) {
            ev <- lab_levels[j - 1]
        } else {
            ev <- lab_levels[1]
        }
        # ev <- ev - ev/10.
        if (lab_levels[1] < 0.) {
            ev <- lab_levels[1] - 1.
        }

        ttE <- raster::extend(ttE, et, value = ev)
    }
    
    # convert raster to dataframe 
    ttP <- raster::rasterToPoints(ttE)
    ttDF <- data.frame(ttP)
    colnames(ttDF) <- c("x", "y", "z")
    
    # boundaries of extended domain
    xminE <- raster::xmin(ttE)
    yminE <- raster::ymin(ttE)
    xmaxE <- raster::xmax(ttE)
    ymaxE <- raster::ymax(ttE)
    
    # color palette (omit first color)
    spectral <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598", "#FFFFBF",
                           "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")
    if (is.null(colors)) {
        myPalette <- grDevices::colorRampPalette(spectral)
        myColors <- myPalette(length(levels) + 1)[-c(1, 1)]
    } else {
        myPalette <- grDevices::colorRampPalette(colors, alpha = TRUE)
        myColors <- myPalette(length(levels))
    }
    
    # Legend
    if (is.null(legend)) {
        legend <- ""
    }
    # prettify legend title
    # TODO: svincolarsi da openair per l'etichetta
    if (requireNamespace("openair", quietly = TRUE)) {
        lgndname <- openair::quickText(legend, auto.text = TRUE)
    } else {
        lgndname <- legend
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

    # If requested, wipe all but main plot 
    if (isTRUE(bare)) {
        baretheme <- theme(axis.line=element_blank(),
                           axis.text.x=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),
                           legend.position="none",
                           panel.background=element_blank(),
                           panel.border=element_blank(),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           plot.background=element_blank())
    } else {
        baretheme <- theme()
    }

    # Contour plot
    v <- ggplot(ttDF) +
        annotation_custom(gimg, -Inf, Inf, -Inf, Inf)  +
        underlayer + 
        stat_hollow_contour(
            data = ttDF,
            aes(x = x, y = y, z = z, fill = factor(..level..)),
            geom = "hollow_polygon",
            size = size,
            breaks = levels,
            alpha = transparency,
            cover = cover,
            na.rm = TRUE) +
        scale_fill_manual(lgndname,
                          guide = guide_legend(reverse = TRUE, label.vjust = 0),
                          breaks = levels,
                          labels = lab_levels,
                          values = myColors) +
        scale_x_continuous(name = "x [m]",
                           limits = c(xminE, xmaxE),
                           breaks = seq(xmin, xmax, length.out = nx),
                           expand = c(0, 0)) +
        scale_y_continuous(name = "y [m]",
                           limits = c(yminE, ymaxE),
                           breaks = seq(ymin, ymax, length.out = ny),
                           expand = c(0, 0)) +
        theme_bw(base_size = 10, base_family = "sans") +
        coord_fixed(ratio = 1, xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
        overlayer +
        baretheme
  
    return(v)
}