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
  to (see openair::timeAverage)}

  \item{ylabel}{The label to be plot along y axis.}
}
\description{
Plot average temperature
}
\note{
\code{plotAvgTemp} uses \code{openair::timeAvearge} to
compute average.
}

