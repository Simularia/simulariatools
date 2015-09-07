library(devtools)
dev_mode()

library(grid)
library(dplyr)
library(ggplot2)

test <- importRaster(fname = "~/Simularia/Progetti/Arianet/Italcementi_Palermo/data/spray/avg_SO2A15_inv.nc", k = 1000, destaggering = T, variable = "SO2A15")
test <- test %>% filter(x < max(test$x), y < max(test$y))

levels = c(0.1, 0.2)

ggplot(test, aes(x, y, z = z)) + stat_contour2(aes(fill = factor(..level..)), geom = "polygon2")

contourPlot(test, background = "~/Simularia/Progetti/Arianet/Italcementi_Palermo/data/basemap.png", legend = "SO2", levels = levels)
contourPlot_old(test, background = "~/Simularia/Progetti/Arianet/Italcementi_Palermo/data/basemap.png", legend = "SO2", levels = levels)

levels <- c(0.1, 0.2, 0.3)
contourPlot(test, legend = "SO2", levels = levels)
contourPlot2(test, legend = "SO2", levels = levels)

X = test$x; X <- unique(X); X <- sort(X)
Y = test$y; Y <- unique(Y); Y <- sort(Y)
Z = matrix(test$z, nrow = length(Y), ncol = length(X))
filled.contour(X, Y, Z, levels = levels) # QUESTA FUNZIONA CORRETTAMENTE
filled.contour(X, Y, Z, nlevels = 4) # QUESTA FUNZIONA CORRETTAMENTE


## contour lines
library(grDevices)
cl <- grDevices::contourLines(X,Y,Z, levels = levels)
cl

raster::filledContour(X,Y,Z)

# ###################

library(ggplot2)
p = qplot(1:10, 1:10) + theme_bw()
g = ggplotGrob(qplot(1, 1))
p + annotation_custom(grob = g, xmin = 2, xmax = 4, ymin = 6, ymax = 10)


g= ggplotGrob(filled.contour(X, Y, Z, nlevels = 4))



# lattice -----------------------------------------------------------------
# see http://www.r-bloggers.com/displaying-data-using-level-plots/

library(lattice)
library(latticeExtra)
levels <- c(0.1, 0.2, 0.3)

X = test$x; X <- unique(X); X <- sort(X)
Y = test$y; Y <- unique(Y); Y <- sort(Y)
grid = expand.grid(x= X, y = Y)
grid$z = test$z

elevation.loess = loess(z ~ x*y, data = grid, degree = 2, span = 1/1000)
elevation.fit = expand.grid(list(x = seq(range(X)[1], range(X)[2], 100/5), y = seq(range(Y)[1], range(Y)[2], 100/5)))
z = predict(elevation.loess, newdata = elevation.fit)
elevation.fit$Height = as.numeric(z)

nlevels = c(levels, max(elevation.fit$Height))
myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, name = "Spectral")))
myColors <- myPalette(length(levels)+1)[-1]

levelplot(Height~x*rev(y), elevation.fit, at = nlevels, col.regions = myColors, contour = FALSE, aspect = "iso", alpha.regions = 0.66)




# grid.path ---------------------------------------------------------------

library(grid)
library(grDevices)
library(dplyr)


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
    groups <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")
    
    data.frame(
        level = rep(levels, lengths),
        x = xs,
        y = ys,
        piece = pieces,
        group = groups
    )
}

levels = c(0.1, 0.2, 0.3)
df <- contour_lines(test, breaks = levels)
df <- df %>%
    mutate(x = x / max(x), y = y / max(y)) %>%
    head(10)
    
grid.path(x = df$x/max(df$x), y = df$y/max(df$y), id = factor(df$level), default.units = "native", rule = "evenodd", gp = gpar(fill = "red"))

x <- df$x
y <- df$y
id <- df$piece
grid.path(x, y, id = id, rule = "evenodd", gp = gpar(fill = "red"))

x <- c(.1, .5, 0.9,
       .4, .5, .6,
       .44, .5, .56)
y <- c(.1, .8, .1,
       .3, .4, .3,
       .7, .6, .7)
id <- rep(1:1, each = 3)
cbind(x, y, id)

# grid.path(x, y, id = id, gp = gpar(fill = "grey"))
grid.path(x, y, id = id, rule = "evenodd", gp = gpar(fill = "red", col = "red"))
grid.path(x, y, id = id, rule = "evenodd")
grid.polygon(x, y, id = id)

