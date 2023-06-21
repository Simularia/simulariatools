test_that("contourPlot2 is a ggplot2 object", {
    data("volcano")
    volcano <- reshape2::melt(volcano)
    names(volcano) <- c("x", "y", "z")
    v <- contourPlot2(volcano)
    expect_s3_class(v, "ggplot")
})