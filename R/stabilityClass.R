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
#' @seealso
#' [turnerStabilityClass()] which computes stability class with Turner method.
#' [plotStabilityClass()] to produce graphical outputs with stability class.
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

    catStab
}


#' Turner stability class
#'
#' Computes PGT stability class using Turner method, based on the local
#' wind speed, cloud cover, ceiling
#' height and solar elevation.
#'
#' @details
#'
#' If `datetime` is a vector, an equal length vector for the other input
#' parameters is expected. It is also possible to provide a single value
#' for the other parameters; in that case the value is kept constant along
#' all the deadlines.
#'
#' @param datetime datetime object (class POSIXct). Either a single value or
#' a vector
#' @param longitude,latitude geographical coordinates (in degrees) of the point
#' of interest
#' @param ws wind speed at 10 m (in m/s)
#' @param cloud_cover Total cloud cover in the range 1...8
#' @param ceiling_height Ceiling height in metres
#'
#' @returns
#' A numeric value (or vector) in the range 1 to 6, where 1 = A, 2 = B, ...,
#' 6 = F.
#'
#' @seealso
#' [stabilityClass()] which computes stability class with other methods.
#' [plotStabilityClass()] to produce graphical outputs with stability class.
#'
#' @examples
#'
#' # Single value example:
#' turnerStabilityClass(
#'     datetime = as.POSIXct("2024-12-01 13:00", tz = "ETC/GMT-1"),
#'     longitude = 7.12,
#'     latitude = 45.10,
#'     ws = 3,
#'     cloud_cover = 3,
#'     ceiling_height = 3000
#' )
#'
#' # datetime vector with constant values
#' deadlines <- seq(
#'     from = as.POSIXct("2024-12-01 00:00"),
#'     to = as.POSIXct("2024-12-31 23:00"),
#'     length.out = 24 * 31
#' )
#' turnerStabilityClass(
#'     datetime = deadlines,
#'     longitude = 7.12,
#'     latitude = 45.10,
#'     ws = 3,
#'     cloud_cover = 3,
#'     ceiling_height = 3000
#' )
#'
#'
#' @export
#'
turnerStabilityClass <- function(
    datetime,
    longitude,
    latitude,
    ws,
    cloud_cover,
    ceiling_height
) {
    # Compute PGT stabilty class with Turner method
    # (this is for non-water sites)
    # Output is an integer i = 1 => A, ..., i = 6 => F

    # datetime
    # longitude = point longitude in deg
    # latitude = point latitude in deg
    # ws = wind speed at 10 m
    # dcc = cloud cover (0 to 10)
    # ch = ceiling height (m)

    # Check if datetime is a POSIXct object
    if (!inherits(datetime, "POSIXct")) {
        stop("datetime is not an object of class POSIXct")
    }

    # Length of datetime
    ndata <- length(datetime)
    if (ndata > 1 && length(ws) == 1) {
        ws <- rep(ws, ndata)
    }
    if (ndata > 1 && length(longitude) == 1) {
        longitude <- rep(longitude, ndata)
    }
    if (ndata > 1 && length(latitude) == 1) {
        latitude <- rep(latitude, ndata)
    }
    if (ndata > 1 && length(cloud_cover) == 1) {
        cloud_cover <- rep(cloud_cover, ndata)
    }
    if (ndata > 1 && length(ceiling_height) == 1) {
        ceiling_height <- rep(ceiling_height, ndata)
    }

    # Turner table [insolation class, wind class]
    turner_table <- array(NA, dim = c(7, 12))
    turner_table[1, ] <- c(6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 4, 4)
    turner_table[2, ] <- c(6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 4, 4)
    turner_table[3, ] <- c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
    turner_table[4, ] <- c(3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4)
    turner_table[5, ] <- c(2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4)
    turner_table[6, ] <- c(1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4)
    turner_table[7, ] <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3)

    # Solar elevation thresholds
    alpha_0 <- 0
    alpha_1 <- 15
    alpha_2 <- 30
    alpha_3 <- 60

    # Conversion factor m -> ft
    m2f <- 3.28084

    # Conversion factor m/s -> knots
    m2k <- 1.9438445

    # Convert cloud cover from octets to 1...10
    cloud_cover_dec <- round(cloud_cover / 8 * 10, 0)

    turner <- NA
    for (i in seq_along(datetime)) {
        # alpha = solar elevation (deg)
        sin_alpha <- solar_elevation(datetime[i], longitude[i], latitude[i])
        alpha <- asin(sin_alpha) * 180 / pi

        # Convert ceiling height to hundreds of feet
        ch_f <- ceiling_height[i] * m2f / 100

        # Compute solar elevation index
        alpha_index <- 4
        if (alpha < alpha_3) {
            alpha_index <- 3
        }
        if (alpha < alpha_2) {
            alpha_index <- 2
        }
        if (alpha < alpha_1) {
            alpha_index <- 1
        }
        if (alpha < alpha_0) {
            alpha_index <- 0
        }

        # Compute insolation class
        ins_class <- NULL

        if (alpha_index == 0) {
            ## Night
            ins_class <- -2

            if (cloud_cover_dec[i] > 4) {
                ins_class <- -1
            }
            if (cloud_cover_dec[i] == 10 && ch_f < 70) {
                ins_class <- 0
            }
        } else {
            ## Day
            ins_class <- alpha_index
            if (cloud_cover_dec[i] %in% seq(6, 9)) {
                if (ch_f < 70) {
                    ins_class <- max(1, alpha_index - 2)
                } else if (ch_f < 160) {
                    ins_class <- max(1, alpha_index - 1)
                } else {
                    ins_class <- alpha_index
                }
            }
            if (cloud_cover_dec[i] == 10) {
                if (ch_f < 70) {
                    ins_class <- 0
                } else if (ch_f < 160) {
                    ins_class <- max(1, alpha_index - 2)
                } else {
                    ins_class <- max(1, alpha_index - 1)
                }
            }
        }

        # Convert insolation class to array index
        index <- ins_class + 3

        # Convert wind speed from m/s to knot, bound to 1–12 and round to integer
        wsk <- ws[i] * m2k + 0.5
        wsk <- max(wsk, 1)
        wsk <- min(wsk, 12)
        wsk_index <- round(wsk, 0)

        # Get value from table
        turner[i] <- turner_table[index, wsk_index]
    }

    turner
}


# Compute sin of solar elevation
solar_elevation <- function(
    datetime,
    longitude,
    latitude,
    orographic_height = 0
) {
    # datetime
    # longitude of the point
    # latitude of the point
    # orographic_height

    # Conversion factor degrees to radians
    deg2rad <- pi / 180

    # latitude and longitude in radians
    longitude_rad <- longitude * deg2rad
    latitude_rad <- latitude * deg2rad

    # Get solar declination
    deltas <- solar_declination(datetime)

    # hour of day in UTC
    tutc <- as.integer(format(datetime, "%H", tz = "UTC"))

    # sin of elevation
    sin_elevation <- sin(latitude_rad) *
        sin(deltas) -
        cos(latitude_rad) * cos(deltas) * cos(2 * pi * tutc / 24 + longitude_rad)

    sin_elevation
}


# Compute solar declination:
# angle of between the ecliptic and the plane of the Earth's equator.
# Value in radians
solar_declination <- function(any_day) {
    # Solar declination angle for any day of the year

    # earth tilting angle
    deg2rad <- pi / 180
    earth_tilt_angle <- 23.44 * deg2rad

    # julian day for any day
    any_julian_day <- as.integer(format(any_day, "%j"))

    # julian day for the summer solstice (northern emisphere)
    summer_solstice <- 172

    num_days <- 365
    # Check leap year
    year <- as.integer(format(any_day, "%Y"))
    if ((year %% 4 == 0 && year %% 100 != 0) || year %% 400 == 0) {
        num_days <- 366
    }

    solar_declination <- earth_tilt_angle *
        cos(2 * pi * (any_julian_day - summer_solstice) / num_days)

    solar_declination
}
