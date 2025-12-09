#' Stability class.
#'
#' Computes stability class given net radiation, total cloud cover and wind
#' speed.
#'
#' `stabilityClass()` computes stability class according to IAEA method based
#' on net radiation, total cloud cover tcc and wind speed.
#' Net radiation and wind are used by day; tcc and wind are used by night.
#'
#' Three different algorithms are implemented, selected by the `option`
#' argument.
#'
#' \code{iaea} option implements the *radiation-wind method recommended by the
#' International Atomic Energy Agency (IAEA) and it is based on the net
#' radiation during the day and cloud cover by night.
#'
#' \code{pasquill} option is based on the original Pasquill formulation and
#' lacks the "very weak" solar insolation present in the modified \code{iaea}
#' version.
#'
#' Eventually, the \code{custom} options is similar to \code{iaea},
#' with slightly different set of parameters for net radiation, wind speed
#' and cloud cover.
#'
#' Previously used option \code{impact} is the same as \code{iaea} and it is now
#' deprecated.
#'
#' @param rad The net radiation in W/m^2
#' @param tcc The total cloud cover in a range from 1  to 8
#' @param ws wind speed in m/s
#' @param option The method used to determine the stability class. It can be
#' \code{iaea} (default), \code{pasquill} or \code{custom}.
#'
#' @return \code{stabilityClass} returns a numeric vector with Pasquill
#' stability classes coded as: A = 1, B = 2, ... , F = 6 ranging from
#' "very unstable" to "very stable".
#'
#' @seealso [plotStabilityClass()]
#'
#' @export
#'
#' @examples
#'
#' # Compute stability class with custom algorithm
#' stMeteo$cst <- stabilityClass(
#'     rad = stMeteo$rad,
#'     tcc = stMeteo$tcc,
#'     ws = stMeteo$ws,
#'     option = "custom"
#' )
#'
stabilityClass <- function(rad, tcc, ws, option = "iaea") {
    # check if the input vectors have the same length
    if (
        length(rad) != length(tcc) ||
            length(tcc) != length(ws) ||
            length(rad) != length(ws)
    ) {
        stop("The length of the three vectors is different.", call. = FALSE)
    }

    # check for stability method
    if (!option %in% c("iaea", "impact", "pasquill", "custom")) {
        stop("Invalid stability option.", call. = FALSE)
    }

    if (option == "impact") {
        warning(
            "option = 'impact' is now deprecated. Please use the equivalent option 'iaea'"
        )
        option <- "iaea"
    }

    if (option == "iaea") {
        limrad <- 6
        # Original radiation limits as 12.5, 25 and 40 langleys/h
        # Here converted to Watt/m^2
        radlim <- c(limrad, limrad, limrad, 145.4, 290.75, 581.5, Inf)

        # IAEA velocity table
        vel <- c(1, 2, 4, 6, 7, Inf)

        # IAEA cloud cover vector
        nuvo <- c(4, 8, Inf)

        # Impact stability classes
        tabStab <- array(NA, dim = c(6, 7))
        tabStab[1, ] <- c(6, 6, 4, 4, 2, 1, 1)
        tabStab[2, ] <- c(6, 5, 4, 4, 2, 2, 1)
        tabStab[3, ] <- c(6, 5, 4, 4, 3, 2, 1)
        tabStab[4, ] <- c(5, 4, 4, 4, 3, 3, 2)
        tabStab[5, ] <- c(4, 4, 4, 4, 4, 3, 3)
        tabStab[6, ] <- c(4, 4, 4, 4, 4, 4, 3)
    } else if (option == "pasquill") {
        # Pasquill radiaion vector (night, night, night, day)
        limrad <- 1
        radlim <- c(limrad, limrad, 290.75, 581.5, Inf)

        # Pasquill velocity vector
        vel <- c(2, 3, 4, 6, Inf)

        # Pasquill cloud cover vector
        nuvo <- c(4, Inf)

        # Pasquill stability classes
        tabStab <- array(NA, dim = c(5, 5))
        tabStab[1, ] <- c(6, 5, 2, 1, 1)
        tabStab[2, ] <- c(6, 5, 3, 2, 2)
        tabStab[3, ] <- c(5, 4, 3, 3, 2)
        tabStab[4, ] <- c(4, 4, 4, 3, 3)
        tabStab[5, ] <- c(4, 4, 4, 4, 3)
    } else {
        # custom
        limrad <- 1
        radlim <- c(limrad, limrad, limrad, 145.4, 290.75, 581.5, Inf)

        # Velocity vector
        vel <- c(2, 3, 4, 6, Inf)

        # Cloud cover vector
        nuvo <- c(2, 5, Inf)

        # Stability classes
        tabStab <- array(NA, dim = c(5, 7))
        tabStab[1, ] <- c(6, 5, 4, 4, 2, 1, 1)
        tabStab[2, ] <- c(6, 5, 4, 4, 2, 2, 1)
        tabStab[3, ] <- c(4, 4, 4, 4, 3, 3, 2)
        tabStab[4, ] <- c(4, 4, 4, 4, 3, 3, 3)
        tabStab[5, ] <- c(4, 4, 4, 4, 4, 4, 3)
    }

    catStab <- NA

    for (i in seq_along(rad)) {
        # Wind speed
        iv <- 1
        while (ws[i] >= vel[iv]) {
            iv <- iv + 1
        }

        # Daytime, incoming solar radiation
        ir <- 1
        while (rad[i] >= radlim[ir]) {
            ir <- ir + 1
        }

        # Nightime, cloud cover
        if (ir == 1) {
            while (tcc[i] >= nuvo[ir]) {
                ir <- ir + 1
            }
        }

        catStab[i] <- tabStab[iv, ir]
    }

    return(catStab)
}
