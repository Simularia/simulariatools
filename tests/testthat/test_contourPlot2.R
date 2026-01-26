test_that("contourPlot2 is a ggplot2 object", {
    data("volcano")
    volcano <- as.data.frame(volcano)
    volcano3d <- reshape(
        volcano,
        direction = "long",
        varying = list(1:61),
        idvar = "x",
        timevar = "y",
        v.names = "z"
    )
    v <- contourPlot2(volcano3d)
    expect_s3_class(v, "ggplot")
})

test_that("contourPlot2 with label_contours is a ggplot2 object", {
    data("volcano")
    volcano <- as.data.frame(volcano)
    volcano3d <- reshape(
        volcano,
        direction = "long",
        varying = list(1:61),
        idvar = "x",
        timevar = "y",
        v.names = "z"
    )
    v <- contourPlot2(volcano3d, fill = FALSE, label_contours = TRUE)
    expect_s3_class(v, "ggplot")
})
