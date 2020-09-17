#' Time series of meteorologicas variables
#' 
#' A dataset containing time series of meteorological variables.
#' The variables are as follows:
#'
#' @format A data frame with 8759 rows and 7 variables:
#' \describe{
#'   \item{date}{Date and time of data collection (y-m-d h:m:s).}
#'   \item{ws}{Wind speed (m/s).}
#'   \item{wd}{Wind direction (deg).}
#'   \item{temp}{Air temperature (Celsius).}
#'   \item{radg}{Global radiation (W/m2).}
#'   \item{tcc}{Total cloud cover (0-8).}
#'   \item{pgt}{Pasquill-Gifford-Turner stability class (1-6).}
#' }
"stMeteo"


#' Concentration values on a domain
#'
#' A dataset containing fake concentration values of an unspecified
#' airborne pollutant to test mapping functions. 
#' The domain has size 101 x 101 and the coordinates are expressed in m (as if they were UTM coordinates.)
#'
#' @format A data frame with 10201 rows and 3 variables.
#' \describe{
#'   \item{x}{Easting coordinate (m).}
#'   \item{y}{Northing coordinate (m).}
#'   \item{z}{Concentration values to be plotted.}
#' }
#'
"stTestmap"
