#' Meteorological dataset with hourly values
#'
#' A dataset containing 8760 hourly values of some meteorological variables
#' corresponding to a full solar year.
#'
#' @format A data frame with 8760 rows and 7 variables:
#' \describe{
#'   \item{date}{date time in yyyy-mm-dd HH:MM:SS}
#'   \item{ws}{wind speed in m/s}
#'   \item{wd}{wind direction in deg.}
#'   \item{temp}{air temperature in C}
#'   \item{radg}{Global solar radiation in W/m^2}
#'   \item{tcc}{Total cloud cover in integers ranging from 0 to 8}
#'   \item{pgt}{Pasquill-Gifford-Turner stability class}
#' }
#' @source Self derived dataset.
"stMeteo"

