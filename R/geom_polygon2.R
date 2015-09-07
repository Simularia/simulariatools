#' new geom
#'  See http://rpubs.com/kohske/3522
#'  See http://docs.ggplot2.org/dev/vignettes/extending-ggplot2.html
#'
#' @import ggplot2
#' @import grid
#' 
#' @export
#' 
GeomPolygon2 <- ggproto("GeomPolygon2", Geom, 
                        required_aes = c("x", "y"),
                        default_aes = aes(order = ..level..),
                        drak_key = draw_key_polygon,
                        
                        draw_panel = function(data, panel_scales, coord, ...) { 
                            n <- nrow(data)
                            if (n == 1) 
                                return(grid::nullGrob())
                            
                            coords <- coord$transform(data, panel_scales)
                            # coords <- coord_munch(coord, data, panel_scales, segment_length = 0.01)
                            coords <- coords[order(coords$piece), ]
                            
                            first_idx <- !duplicated(coords$group)
                            first_rows <- coords[first_idx,]
                            
                            # TODO: Fix alpha
                            mAlpha = 0.1
                            
                            pathGrob(coords$x, 
                                           coords$y, 
                                           default.units = "native",
                                           id = coords$piece,
                                           # rule = "evenodd", 
                                           gp = gpar( 
                                               col = alpha(first_rows$fill, mAlpha),
                                               fill = alpha(first_rows$fill, mAlpha)
                                               # fill = alpha(first_rows$fill, first_rows$alpha)
                                               )
                            )
                        }
)

#' @export
#' 
geom_polygon2 <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE, ...) {
    layer(
        geom = GeomPolygon2, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(...)
    )
}

