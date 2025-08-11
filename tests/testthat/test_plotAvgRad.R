test_that("plotAvgRad throws and error if date column is missing", {
    # Create a sample dataframe with missing date column
    mydata <- data.frame(radg = runif(10, min = 0, max = 30))
    expect_error(plotAvgRad(mydata))
})

test_that("plotAvgRad throws and error if date column is numeric", {
    # Create a sample dataframe with date as numeric
    mydata <- data.frame(
        date = runif(10, min = 0, max = 100),
        temp = runif(10, min = 0, max = 30)
    )
    expect_error(plotAvgRad(mydata))
})

test_that("plotAvgRad throws and error if date column is Date", {
    # Create a sample dataframe with date as numeric
    mydata <- data.frame(
        date = seq(
            as.Date("2023-01-01"),
            as.Date("2023-01-31"),
            length.out = 10
        ),
        temp = runif(10, min = 0, max = 30)
    )
    expect_error(plotAvgRad(mydata))
})

test_that("plotAvgRad throws and error if date column is character", {
    # Create a sample dataframe with date as numeric
    mydata <- data.frame(
        date = seq(
            as.Date("2023-01-01"),
            as.Date("2023-01-31"),
            length.out = 10
        ),
        temp = runif(10, min = 0, max = 30)
    )
    mydata$date <- as.character(mydata$date)
    expect_error(plotAvgRad(mydata))
})

test_that("plotAvgRad is a ggplot2 object", {
    mydata <- data.frame(
        date = seq.POSIXt(
            from  = as.POSIXct("2021-01-01 00:00"),
            by = "1 hour",
            length.out = 4000
        ),
        radg = runif(4000, min = 0, max = 900)
    )
    expect_s3_class(plotAvgRad(mydata), "ggplot")
})
