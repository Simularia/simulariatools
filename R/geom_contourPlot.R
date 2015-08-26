# See  http://stackoverflow.com/questions/22159087


geom_contourPlot <- function(mapping=NULL, data=NULL, stat=NULL,
                             position = "identity", show.legend = NA,
                             inherit.aes = TRUE, ...) {
    # GeomContourPlot$new(mapping=mapping, data=data)
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomContourPlot,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(...)
    )
    
}


contourplotGrob <- function(x, y, z) {  
    # rasterGrob(
        # car.raster, x=x, y=y, 
        # hjust=1, height=width, width=length
    # )
    # prt = gridPLT()
    filled.contour(X, Y, Z) 
}

GeomContourPlot <- ggproto("GeomContourPlot", Geom, 
    draw = function(self, data, scales, coordinates, ...) {
        # with(
            # coord_transform(coordinates, data, scales), 
            contourplotGrob(x, y, z)
        # )    
    },
    
    # objname <- "contourplot" # name of the geom in lowercase. Must correspond to GeomField.
    # desc <- "A contour plot!"
    
    default_stat = function(.) StatIdentity,
    required_aes = c("x", "y", "z") ,
    guide_geom = function(.) "contourplot",
    default_aes = function(.) aes()
 )