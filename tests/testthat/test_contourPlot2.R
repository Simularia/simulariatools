# Build a volcano data frame in long format for the tests below
volcano3d <- function() {
    data("volcano", envir = environment())
    volcano <- as.data.frame(volcano)
    reshape(
        volcano,
        direction = "long",
        varying = list(1:61),
        idvar = "x",
        timevar = "y",
        v.names = "z"
    )
}

# TRUE if the built plot contains a geom_label (contour-label) layer.
has_label_layer <- function(p) {
    any(vapply(p$layers, function(l) inherits(l$geom, "GeomLabel"), logical(1)))
}

# TRUE if the built plot contains a geom_contour_filled layer.
has_filled_layer <- function(p) {
    any(vapply(
        p$layers,
        function(l) inherits(l$geom, "GeomContourFilled"),
        logical(1)
    ))
}

# TRUE if the built plot is ggplot object
test_that("contourPlot2 is a ggplot2 object", {
    v <- contourPlot2(volcano3d())
    expect_s3_class(v, "ggplot")
})

# TRUE if the built plot with contour_labels is ggplot object
test_that("contourPlot2 with label_contours is a ggplot2 object", {
    v <- contourPlot2(volcano3d(), fill = FALSE, contour_labels = TRUE)
    expect_s3_class(v, "ggplot")
    expect_true(has_label_layer(v))
})

# Report missing x, y, z columns
test_that("contourPlot2 errors and lists all missing columns", {
    d <- volcano3d()
    expect_error(contourPlot2(d[c("x", "y")]), "Missing column: z")
    expect_error(contourPlot2(d["x"]), "Missing column: y, z")
})

# Consistency check: tile = TRUE suppresses contour labels regardless of the
# contour_labels argument. Otherwise the contour-label branch would run with
# size forced to 0 and error on the undefined `line_levels`.
test_that("contourPlot2 tile = TRUE suppresses contour labels", {
    v <- contourPlot2(volcano3d(), tile = TRUE, contour_labels = TRUE)
    expect_s3_class(v, "ggplot")
    expect_false(has_label_layer(v))
})

# Consistency check: fill = TRUE draws a filled contour layer and ignores
# contour_labels, which only apply to line contours (fill = FALSE).
test_that("contourPlot2 fill = TRUE draws a filled layer and no contour labels", {
    v <- contourPlot2(volcano3d(), fill = TRUE, contour_labels = TRUE)
    expect_s3_class(v, "ggplot")
    expect_true(has_filled_layer(v))
    expect_false(has_label_layer(v))
})

# Regression: optional arguments default to NULL and are guarded with
# is.null(), not missing().
test_that("contourPlot2 treats explicit NULL optional args as omitted", {
    expect_no_warning(v <- contourPlot2(volcano3d(), basemap = NULL))
    expect_s3_class(v, "ggplot")

    expect_no_warning(v <- contourPlot2(volcano3d(), mask = NULL))
    expect_s3_class(v, "ggplot")

    expect_no_warning(v <- contourPlot2(volcano3d(), domain = NULL))
    expect_s3_class(v, "ggplot")

    expect_no_warning(v <- contourPlot2(volcano3d(), background = NULL))
    expect_s3_class(v, "ggplot")

    expect_no_warning(v <- contourPlot2(volcano3d(), underlayer = NULL, overlayer = NULL))
    expect_s3_class(v, "ggplot")
})

# Deprecated arguments, when actually supplied, must still warn. This guards
# against the is.null() conversion accidentally silencing the deprecation path.
test_that("contourPlot2 still warns when deprecated domain is supplied", {
    expect_warning(
        contourPlot2(volcano3d(), domain = c(1, 61, 1, 61, 5, 5)),
        "deprecated"
    )
})

# Extract the geom_contour_filled layer from a built contourPlot2 plot.
filled_layer <- function(p) {
    b <- ggplot2::ggplot_build(p)
    idx <- which(vapply(b$data, function(d) "fill" %in% names(d), logical(1)))[1]
    b$data[[idx]]
}

fill_limits <- function(p) {
    ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$get_limits()
}

test_that("contourPlot2 shows the full legend when data is outside levels", {
    # geom_contour_filled() generates zero contours.
    v <- suppressWarnings(
        contourPlot2(volcano3d(), levels = c(300, 320, 340, 360))
    )
    # The build emits a *zero contours* warning since nothing is drawn.
    expect_length(suppressWarnings(fill_limits(v)), 4L)
})

# Check axis breaks are alwyas available even when the legend is outside
# data range.
test_that("contourPlot2 keeps axis ticks when data is outside levels", {
    # Reference axis breaks from a normal plot of the same data.
    ref <- ggplot2::ggplot_build(
        contourPlot2(volcano3d(), levels = c(100, 120, 140, 160, 180))
    )$layout$panel_params[[1]]

    # Axis breaks from a plot of the same data with legend outside data range
    out <- suppressWarnings(ggplot2::ggplot_build(
        contourPlot2(volcano3d(), levels = c(300, 320, 340, 360))
    ))$layout$panel_params[[1]]

    expect_equal(out$x$breaks, ref$x$breaks)
    expect_equal(out$y$breaks, ref$y$breaks)
    expect_equal(out$x$get_labels(), ref$x$get_labels())
})

# Check there no NA's values in case of ggplot2:::pretty_isoband_levels modification
test_that("contourPlot2 fill bands all map to a defined colour (no grey)", {
    # Non-integer levels exercise format()-based band labelling: the scale
    # `limits` must match the factor levels produced by geom_contour_filled(),
    # otherwise some bands fall back to the na.value colour (grey).
    v3 <- volcano3d()
    v3$z <- scales::rescale(v3$z, to = c(0, 2))
    v <- contourPlot2(v3, levels = c(-Inf, 0.5, 1.0, 1.5, Inf))
    d <- filled_layer(v)
    present <- unique(as.character(d$level))
    expect_true(all(present %in% fill_limits(v)))
    expect_false(any(d$fill == "grey50" | is.na(d$fill)))
})

# Check the ggplot2:::pretty_isoband_levels and internal computation
# give the same result
test_that("contourPlot2 fill limits match geom_contour_filled factor levels", {
    v <- contourPlot2(volcano3d(), levels = c(-Inf, seq(100, 200, 20), Inf))
    d <- filled_layer(v)
    expect_true(all(levels(d$level) %in% fill_limits(v)))
})

# Check that there are at least 2 levels
test_that("contourPlot2 rejects fewer than two levels", {
    v3 <- volcano3d()
    expect_error(
        contourPlot2(v3, levels = -Inf),
        "at least two values"
    )
    expect_error(
        contourPlot2(v3, levels = 5),
        NA
    )
})

# Check that there are no missing values in the levels
test_that("contourPlot2 rejects NA in levels", {
    v3 <- volcano3d()
    expect_error(
        contourPlot2(v3, levels = c(NA, 100, 200)),
        "without missing values"
    )
})

# Check for unsorted levels
test_that("contourPlot2 rejects unsorted levels", {
    v3 <- volcano3d()
    expect_error(
        contourPlot2(v3, levels = c(100, 50, 200)),
        "strictly increasing"
    )
    expect_error(
        contourPlot2(v3, levels = c(100, 100, 200)),
        "strictly increasing"
    )
})

# Tests for the internal axis bounds computation
test_that("computeAxisBounds defaults to the data range", {
    d <- data.frame(x = c(1, 5, 3), y = c(10, 40, 25))
    b <- computeAxisBounds(d)
    expect_equal(c(b$xmin, b$xmax, b$ymin, b$ymax), c(1, 5, 10, 40))
    expect_equal(c(b$nx, b$ny), c(5, 5))
})
test_that("computeAxisBounds: basemap extent overrides data defaults", {
    d <- data.frame(x = c(1, 5), y = c(10, 40))
    b <- computeAxisBounds(d, basemap_extent = c(0, 100, -5, 200))
    expect_equal(c(b$xmin, b$xmax, b$ymin, b$ymax), c(0, 100, -5, 200))
})
test_that("computeAxisBounds: xlim/ylim override data and basemap extent", {
    d <- data.frame(x = c(1, 5), y = c(10, 40))
    b <- computeAxisBounds(
        d,
        xlim = c(2, 8),
        ylim = c(3, 9),
        basemap_extent = c(0, 100, -5, 200)
    )
    expect_equal(c(b$xmin, b$xmax, b$ymin, b$ymax), c(2, 8, 3, 9))
})
test_that("computeAxisBounds: nticks maps to nx/ny", {
    d <- data.frame(x = c(1, 5), y = c(10, 40))
    expect_equal(computeAxisBounds(d, nticks = 7)[c("nx", "ny")], list(nx = 7, ny = 7))
    expect_equal(computeAxisBounds(d, nticks = c(3, 4))[c("nx", "ny")], list(nx = 3, ny = 4))
})
test_that("computeAxisBounds errors on inverted limits", {
    d <- data.frame(x = c(1, 5), y = c(10, 40))
    expect_error(computeAxisBounds(d, xlim = c(8, 2)), "max\\(x\\) <= min\\(x\\)")
    expect_error(computeAxisBounds(d, ylim = c(9, 3)), "max\\(y\\) <= min\\(y\\)")
})
