test_that("plotStabilityClass throws and error if date column is missing", {
    # Create a sample dataframe with missing date column
    mydata <- data.frame(radg = runif(10, min = 0, max = 30))
    expect_error(plotStabilityClass(mydata))
})

test_that("plotStabilityClass throws and error if date column is numeric", {
    # Create a sample dataframe with date as numeric
    mydata <- data.frame(
        date = runif(10, min = 0, max = 100),
        temp = runif(10, min = 0, max = 30)
    )
    expect_error(plotStabilityClass(mydata))
})

test_that("plotStabilityClass throws and error if date column is Date", {
    # Create a sample dataframe with date as numeric
    mydata <- data.frame(
        date = seq(
            as.Date("2023-01-01"),
            as.Date("2023-01-31"),
            length.out = 10
        ),
        temp = runif(10, min = 0, max = 30)
    )
    expect_error(plotStabilityClass(mydata))
})

test_that("plotStabilityClass throws and error if date column is character", {
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
    expect_error(plotStabilityClass(mydata))
})

test_that("plotStabilityClass throws and error if type is not season or hour", {
    # Create a sample dataframe with date as numeric
    mydata <- data.frame(
        date = seq(
            as.Date("2023-01-01"),
            as.Date("2023-01-31"),
            length.out = 10
        ),
        temp = runif(10, min = 0, max = 30)
    )
    mydata$date <- as.POSIXct(mydata$date)
    expect_error(plotStabilityClass(mydata, type = "month"))
})

test_that("plotStabilityClass is a ggplot2 object", {
    mydata <- data.frame(
        date = seq.POSIXt(
            from  = as.POSIXct("2021-01-01 00:00"),
            by = "1 hour",
            length.out = 8760
        ),
        sc = runif(8760, min = 1, max = 6)
    )
    mydata$sc <- as.integer(mydata$sc)
    expect_s3_class(plotStabilityClass(mydata), "ggplot")
    expect_s3_class(plotStabilityClass(mydata, type = "hour"), "ggplot")
    expect_s3_class(plotStabilityClass(mydata, type = "season"), "ggplot")
})
