#' geom_hollowpolygon
#' 
#'  new geom to manage hollows in contour plot
#' 
#'  @references See http://rpubs.com/kohske/352
#'  
#'              See http://docs.ggplot2.org/dev/vignettes/extending-ggplot2.html
#'  
#'  
#'  @import ggplot2 grid
#'  @export
geom_hollow_polygon2 <- function(mapping = NULL, data = NULL, stat = "hollow_contour",
                               position = "identity", show.legend = NA,
                               inherit.aes = TRUE, ...) {
    layer(
        geom = GeomHollowPolygon2,
        mapping = mapping, 
        data = data, 
        stat = stat,
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list( ... )
    )
}

#' @export
#'
GeomHollowPolygon2 <- ggproto("GeomHollowPolygon2", Geom,
                        required_aes = c("x", "y"),
                        
                        default_aes = aes(
                            order = ..level..,
                            size = 0.5,
                            linetype = 1, alpha = 1
                        ),
                        
                        draw_key = draw_key_polygon,
                        
                           
                        draw_group = function(data, panel_scales, coord, ...) {
                            n <- nrow(data)
                            if (n <= 2)
                                return(grid::nullGrob())
                            
                            coords <- coord$transform(data, panel_scales)
                            # coords <- coord_munch(coord, data, panel_scales, segment_length = 0.01)
                            coords <- coords[order(coords$piece), ]
                            first_idx <- !duplicated(coords$group)
                            # first_rows <- coords[first_idx, , drop = FALSE]
                            first_row <- coords[1, , drop = FALSE]
                            print(first_row)
                            
                            # print(first_rows)
                            
                            grid::pathGrob(coords$x,
                                           coords$y,
                                           default.units = "native",
                                           id = coords$piece,
                                           rule = "evenodd",
                                           gp = grid::gpar(
                                               # col = alpha(first_rows$fill, mAlpha),
                                               # fill = alpha(first_rows$fill, mAlpha)
                                               # col = alpha(first_rows$fill, first_rows$alpha),
                                               # fill = alpha(first_rows$fill, first_rows$alpha)
                                               col = alpha(first_row$fill, first_row$alpha),
                                               fill = alpha(first_row$fill, first_row$alpha),
                                               lwd = first_row$size * .pt,
                                               lty = first_row$linetype
                                           )
                            )
                            
                            
                            
                            
                            
                        }
)



