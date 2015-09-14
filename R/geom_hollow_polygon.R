#' geom_hollowpolygon
#' 
#'  new geom to manage hollows in contour plot
#' 
#'  @references See http://rpubs.com/kohske/352
#'  
#'              See http://docs.ggplot2.org/dev/vignettes/extending-ggplot2.html
#'  
#'  
#'  @import ggplot2 grid scales
#'  @export
GeomHollowPolygon <- ggproto("GeomHollowPolygon", Geom,
                             required_aes = c("x", "y"),
                             
                             default_aes = aes(
                                 colour = NA, fill = "grey20", size = 0.5,
                                 linetype = 1, alpha = 1
                             ),
                             
                             draw_key = draw_key_polygon,
                             
                             draw_group = function(data, panel_scales, coord) {
                                 n <- nrow(data)
                                 if (n <= 2) return(grid::nullGrob())
                                 
                                 coords <- coord$transform(data, panel_scales)
                                 coords <- coords[order(coords$piece), ]
                                 
                                 # A polygon can only have a single colour, fill, etc, so take from first row
                                 first_row <- coords[1, , drop = FALSE]
                                 
                                 grid::pathGrob(
                                     coords$x, coords$y, 
                                     default.units = "native",
                                     rule = "evenodd",
                                     id = coords$piece,
                                     gp = grid::gpar(
                                         # col = first_row$colour,
                                         col =  scales::alpha(first_row$fill, first_row$alpha),
                                         fill = scales::alpha(first_row$fill, first_row$alpha),
                                         lwd =  first_row$size * .pt,
                                         lty =  first_row$linetype
                                     )
                                 )
                             }
)

#' @export
geom_hollow_polygon <- function(mapping = NULL, data = NULL, stat = "hollow_contour",
                                position = "identity", show.legend = NA, 
                                inherit.aes = TRUE, ...) {
    layer(
        geom = GeomHollowPolygon, mapping = mapping, data = data, stat = stat, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(...)
    )
}
