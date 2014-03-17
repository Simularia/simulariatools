\name{plotAvgRad}
\alias{plotAvgRad}
\title{Plot hourly average radiation}
\usage{
plotAvgRad(mydata, rad = "radg")
}
\arguments{
  \item{mydata}{A data frame containing fields date and
  radg}

  \item{radg}{Name of the column representing radiation}
}
\value{
A \code{ggplot2} plot.
}
\description{
Plot a histogram with hourly average of solar radiation,
together with June and December hourly averages (minimum
and maximum).
}
\examples{
plotAvgRad(mydata)
plotAvgRad(mydata, rad="radg")
}

