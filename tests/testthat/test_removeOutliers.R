test_that("removeOutliers value has the same length as the input vector", {
    data("airquality")
    expect_length(removeOutliers(airquality$Wind, k = 1),
                  length(airquality$Wind))
})