\name{plotAvgTemp}
\alias{plotAvgTemp}
\title{Plot average temperature}
\usage{
plotAvgTemp(mydata, temp = "temp", avg.time = "1 month",
  ylabel = "Temperatura [°C]")
}
\arguments{
  \item{mydata}{A data frame containing fields date and
  temp}

  \item{temp}{Name of the column representing temperature}

  \item{avg.time}{This defines the time period to average
  to (see openair::timeAverage). Default is "1 month".}

  \item{ylabel}{The label to be plot along y axis}
}
\value{
A ggplot2 plot object.
}
\description{
\code{plotAvgTemp} builds a plot with histograms of monthly
average temperature and two lines with maximim and minimum
monthly temperature.
}
\note{
\code{plotAvgTemp} uses \code{openair::timeAvearge} to
compute average.
}
\examples{
# Plot histogram with monthly averages together with maxima and minima curves
plotAvgTemp(mydata)
}

