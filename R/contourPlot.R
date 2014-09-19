#' Contour plot of pollutant concentration
#' 
#' \code{contourPlot} plots a contour map of pollutants
#' 
#' This is a high level function to plot contour levels of a pollutants matrix.
#' 
#' @param data A dataframe containing data to be plotted in the form of X, Y and Z (levels).
#' @param domain An array with min X, max X, min Y, max Y, number of ticks on X axis, number of ticks on Y axis (optional).
#' @param background String containing the path to the png file to be plotted as a basemap (optional).
#' @param undelayer Array of strings containing layers to be plotted between basemap and contour plot (optional).
#' @param overlayer Array of strings containing layers to be plotted on top of the contour plot (optional).
#' @param legend (string) Legend title (optional).
#' @param levels Array of levels for contour plot. If not set, automatic levels are plotted.
#' 
#' @return A \code{ggplot2} plot
#' 
#' @import ggplot2
#' @import raster
#'  
#' @export
#' 
#' @examples
#' # Importa raster data
#' data <- importRaster(paste(dir, inputfile, sep=""), k = 1000, variable = "CONCAN") 
#' 
#' # Simple contour plot
#' contourPlot(data)
#' 
#' # Specifiy (sub)domain to be plotted; background image; legend title and pollutant levels.
#' contourPlot(data, domain(500000, 510000, 6000000, 6010000, 7, 7), background = "img/background.png", legend = "no2 [ug/m3]", levels = c(10, 20, 30, 40))
#' 
#' # Add underlayer (same for overlayer)
#' library(ggplot2)
#' library(maptools)
#' perimetro <- readShapeLines("path_to/perimetro.shp")
#' perimetro <- fortify(perimetro)
#' strada <- readShapeLines("path_to/strada.shp")
#' strada <- fortify(strada)
#' myUnderlayer <- vector(mode = "list", length = 2)
#' myUnderlayer[[1]] <- geom_polygon(data = perimetro, aes(long, lat, group = group), colour = "black", fill = NA, size = 0.1, alpha = 0.5)
#' myUnderlayer[[2]] <- geom_path(data = strada, aes(long, lat, group = group), colour = "grey", size = 0.1, alpha = 0.5)
#' contourPlot(data = test, background = "path_to/basemap.png", underlayer = myUnderlayer)
#' 

contourPlot <- function(data, domain, background, underlayer, overlayer, legend, levels) {
    
    if (missingArg(domain)) {
        xmin <- min(data[1])    # x coordinates minimum
        xmax <- max(data[1])    # x coordinates max
        ymin <- min(data[2])    # y coordinates min
        ymax <- max(data[2])    # y coordinates max
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
    
    # Extend data domain to be plotted
    tt <- rasterFromXYZ(data)
# TODO: provare a estendere il domino a 10 * res(tt)     
    et <- extent(xmin(tt) - res(tt)[1],
                 xmax(tt) + res(tt)[1], 
                 ymin(tt) - res(tt)[2], 
                 ymax(tt) + res(tt)[2])
    ttE <- extend(tt, et, value = min(values(tt)) * 10^-5)
    ttP <- rasterToPoints(ttE)
    ttDF <- data.frame(ttP)
    colnames(ttDF) <- c("x", "y", "z")
    
    # Automatic scales
    if (missingArg(levels)) {
        nlevels <- 7
        levels <- pretty(range(values(tt), na.rm = T), n = nlevels, min.n = 4)
    } 
    
    # color palette (omit first color)
    myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, name = "Spectral")))
    myColors <- myPalette(length(levels)+1)[-c(1,1)]
    
    # Legend
    if (missingArg(legend)) {
        legend <- ""
    }
    # prettify legend title
    # TODO: svincolarsi da openair per l'etichetta
    if (requireNamespace("openair", quietly = TRUE)) {
        lgndname <- openair::quickText(legend, auto.text=T)
    } else {
        lgndname <- legend
    }

    # Background image
    if (missingArg(background)) {
        img <- matrix(data = NA, nrow = 10, ncol = 10)
        gimg <- grid::rasterGrob(img, interpolate = T)
    } else {
        img <- png::readPNG(background)
        gimg <- grid::rasterGrob(img, interpolate = T)
    }

    # Underlayer
    if (missingArg(underlayer)) {
        img <- matrix(data = NA, nrow = 10, ncol = 10)
        ugrob <- grid::rasterGrob(img, interpolate = T)
        underlayer <- annotation_custom(ugrob, -Inf, Inf, -Inf, Inf)
    }

    # Overlayer
    if (missingArg(overlayer)) {
        img <- matrix(data = NA, nrow = 10, ncol = 10)
        ogrob <- grid::rasterGrob(img, interpolate = T)
        overlayer <- annotation_custom(ogrob, -Inf, Inf, -Inf, Inf)
    }

    # Graphic Options
    opts <- vector("list", length = 5)
    opts[[1]] <- scale_x_continuous(name = "x [m]", 
                                    limits = c(xmin, xmax),
                                    breaks = seq(xmin, xmax, length.out = nx),
                                    expand = c(0, 0))
    opts[[2]] <- scale_y_continuous(name = "y [m]", 
                                    limits = c(ymin, ymax),
                                    breaks = seq(ymin, ymax, length.out = ny),
                                    expand = c(0, 0))
    opts[[3]] <- coord_fixed(ratio = 1)
    opts[[4]] <- theme_bw(base_size = 10, base_family = "Arial")
    

    # Contour plot
    #     v <- qplot(x = xmin:xmax, y = ymin:ymax, geom = "blank") +
    v <- qplot(1:10, 1:10, geom = "blank") + 
            annotation_custom(gimg, -Inf, Inf, -Inf, Inf) +
            underlayer +
            stat_contour(data = ttDF, 
                     geom = "polygon", 
                     aes(x, y, z = z, fill = factor(..level..)), 
                     breaks = levels, 
                     alpha = 0.66) + 
            scale_fill_manual(lgndname, 
# TODO: aggiungere una linea orizzontale a dx della scala di colore verso l'etichetta numerica
                              guide = guide_legend(reverse = T, label.vjust = 0), 
                          breaks = levels, 
                          limits = levels, 
                          values = myColors) +
            overlayer +
            opts
    
    return(v)
}

