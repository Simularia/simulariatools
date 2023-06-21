test_that("rollingMax value has the same length as the input vector", {
    data("airquality")
    expect_length(rollingMax(airquality$Solar.R, length = 24),
                 length(airquality$Solar.R))
})

test_that("rollingMax length is greater than 2", {
    data("airquality")
    expect_error(rollingMax(airquality$Solar.R, length = 2))
})