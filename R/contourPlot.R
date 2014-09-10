#' Contour plot of pollutants concentration
#' 
#' \code{contourPlot} plots a contour map of pollutants
#' 
#' 
#' 
contourPlot <- function(data, domain = c(0, 1000, 0, 1000, 5, 5),  background, legend, levels) {
    #
    # imageFile (string) path with png file
    # domain    array with min X, max X, min Y, max Y, number of ticks on X axis, number of ticks on Y axis
    # test      data frame of X, Y and Z values to be plotted
    # lgndname  string with legend title
    # lvls      array of levels for contour plot. If not set, automatic levels are plotted.
    #
    
    xmin <- domain[1]       # x coordinates minimum
    xmax <- domain[2]       # x coordinates max
    ymin <- domain[3]       # y coordinates min
    ymax <- domain[4]       # y coordinates max
    nx <- domain[5]         # number of ticks along x axis
    ny <- domain[6]         # number of ticks along y axis
    
    # Extend domain for date to be plotted
#     require(raster)
    tt <- rasterFromXYZ(data)
    et <- extent(xmin(tt) - res(tt)[1], xmax(tt) + res(tt)[1], ymin(tt) - res(tt)[2], ymax(tt) + res(tt)[2])
    ttE <- extend(tt, et, value = min(values(tt)) * 10^-5)
    ttP <- rasterToPoints(ttE)
    ttDF <- data.frame(ttP)
    colnames(ttDF) <- c("x", "y", "z")
    
    # Automatic scales
    #     if (is.na(lvls)) {
    if (missingArg(levels)) {
        lmax = max(values(tt))
        lmax = ceiling(lmax)
        lmin = 0.1
        lvls = seq(lmin, lmax, (lmax - lmin) / 6)
    }
    
    # color palette (omit first color)
    myPalette <- colorRampPalette(rev(brewer.pal(11, name="Spectral")))
    myColors <- myPalette(length(lvls)+1)[-c(1,1)]
    
    
    # Background image
    if (missingArg(background)) {
        img <- raster(nrows=10, ncols=10, xmn=0, xmx=10)
    } else {
        library(png)
        img <- readPNG(background)
    }
    
    # Legend
    if (missingArg(legend)) {
        legend <- ""
    }
    # prettify legend title
#     library(openair)
    lgndname <- quickText(legend, auto.text=T)
    
    # Plot
#     require(ggplot2)
    
    # Graphic Options
    opts <- vector("list", length = 4)
    opts[[1]] <- scale_x_continuous(name = "x [m]", breaks = seq(xmin, xmax, length = nx))
    opts[[2]] <- scale_y_continuous(name = "y [m]", breaks = seq(ymin, ymax, length = ny))
    opts[[3]] <- coord_fixed(xlim=c(minx, maxx), ylim = c(miny, maxy))
    opts[[4]] <- theme_bw(base_size = 10, base_family = "Arial")
    
    # Contour plot
    #     v <- qplot(x = xmin:xmax, y = ymin:ymax, geom = "blank") +
    v <- qplot(1:10, 1:10, geom = "blank") +
        # #         annotation_raster(img, xmin, xmax, ymin, ymax) + 
        #         annotation_raster(img, -Inf, Inf, -Inf, Inf) +
        annotation_raster(img, -Inf, Inf, -Inf, Inf) +
        stat_contour(data=ttDF, geom="polygon", aes(x, y, z=z, fill=factor(..level..)), breaks = lvls, alpha=0.66) + 
        scale_fill_manual(lgndname, guide=guide_legend(reverse=T), breaks = lvls, limits=lvls, values=myColors) +
        #         geom_polygon(data=perimetro_df, aes(long, lat, group=group), size=0.25, color="grey", fill="grey") +
        geom_path(data=autostrada_df, aes(long, lat, group=group), size=0.2, color="darkgrey") +
        geom_path(data=strada_int_df, aes(long, lat, group=group), size=0.2, color="darkgrey") +
        geom_polygon(data=area_df, aes(long, lat, group=group), colour="black", fill=NA, size=0.1) +
        geom_point(data=recettori, aes(X, Y), size=1, color="black") +
        geom_polygon(data = dominio_calcolo_df, aes(long, lat, group = group), colour = "red", fill = NA, size = 0.3) +
        geom_point(data = stazioni, aes(X, Y), size = 1.5, color = "red", ) +   
        geom_text(data = stazioni, aes(X, Y, label = Secondary.ID), size = 3, hjust = 0.5, vjust = -0.5) +
        opts
    
    
    plot(v)
    return(v)
    
}

