test_that("plotAvgTemp throws and error if date column is missing", {
    # Create a sample dataframe with missing date column
    mydata <- data.frame(temp = runif(10, min = 0, max = 30))
    expect_error(plotAvgTemp(mydata))
})

test_that("plotAvgTemp throws and error if date column is numeric", {
    # Create a sample dataframe with date as numeric
    mydata <- data.frame(
        date = runif(10, min = 0, max = 100),
        temp = runif(10, min = 0, max = 30)
    )
    expect_error(plotAvgTemp(mydata))
})

test_that("plotAvgTemp throws and error if date column is Date", {
    # Create a sample dataframe with date as numeric
    mydata <- data.frame(
        date = seq(
            as.Date("2023-01-01"),
            as.Date("2023-01-31"),
            length.out = 10
        ),
        temp = runif(10, min = 0, max = 30)
    )
    expect_error(plotAvgTemp(mydata))
})

test_that("plotAvgTemp throws and error if date column is character", {
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
    expect_error(plotAvgTemp(mydata))
})
