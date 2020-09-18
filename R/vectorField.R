#' Vector field plot
#'
#' Simple function to plot a **velocities** vector field.
#'
#' This function plots a vector field given a data.frame with coordinates (x, y) 
#' and corresponding velocity components (u, v). Vectors are colored by magnitude (speed).
#' The coordinates are assumed to be on a regular rectangular domain in UTM reference system.
#' 
#' This function is heavily inspired by snippets of code in 
#' *R Graphics Cookbook* by Winston Chang (https://r-graphics.org/index.html).
#'
#' @return A \code{ggplot2} plot.
#' 
#' @importFrom ggplot2 ggplot geom_segment aes
#' @importFrom dplyr filter
#' 
#' @param data A dataframe containing data to be plotted in the form of: *(x, y, u, v)*.
#' @param scalex length factor of vector *x* component
#' @param scaley length factor of vector *y* component
#' @param everyx keep one out of every *everyx* values, along *x* direction.
#' @param everyy keep one out of every *everyy* values, along *y* direction.
#' @param length arrow length.
#' @param size arrow size.
#' 
#' @examples 
#' \dontrun{
#' metU <- importADSOBIN('/path/to/meteofile',
#'                       variable = 'U',
#'                       slice=2,
#'                       k = 1000,
#'                       verbose = TRUE)
#' metU <- as.data.frame(metU)
#' metU <- metU %>% 
#'         mutate(u = z, z = NULL)
#' 
#' metV <- importADSOBIN('/path/to/meteofile',
#'                       variable = 'V', 
#'                       slice=2, 
#'                       k = 1000, 
#'                       verbose = TRUE)
#' metV <- as.data.frame(metV)
#' metV <- metV %>%  
#'         mutate(v = z, z = NULL)
#' 
#' met <- merge(metU, metV, by = c("x", "y"))
#' 
#' vectorField(met, everyx = 2, everyy = 2, scalex = 10, scaley = 10) +
#'     coord_fixed(ratio = 1, xlim = c(0, 1000), ylim = c(0, 1000)) +
#'     scale_color_viridis_c()
#' 
#' }
#' @export
#' 
vectorField <- function(data,
                        scalex = 1.,
                        scaley = 1.,
                        everyx = 1,
                        everyy = 1,
                        length = 0.1,
                        size = 0.25) {
    
    # Fix No visible binding for global variable
    x <- y <- u <- v <- speed <- NULL
    
    every_n <- function(x, by = 2) {
        x <- sort(x)
        x[seq(1, length(x), by = by)]
    }
    
    # Compute vector magnitude
    data <- as.data.frame(data)
    data$speed = sqrt(data$u^2 + data$v^2)
    
    # Skip points
    keepx <- every_n(unique(data$x), by = everyx)
    keepy <- every_n(unique(data$y), by = everyy)
    datasub <- dplyr::filter(data, x %in% keepx  &  y %in% keepy)
    
    # Plot
    pl <- ggplot(datasub, aes(x = x, y = y)) +
        geom_segment(aes(xend = x + scalex * 100 * u, 
                         yend = y + scaley * 100 * v, 
                         colour = speed),
                     arrow = arrow(length = unit(length, "cm")),
                     size = size)
    return(pl)
    
}