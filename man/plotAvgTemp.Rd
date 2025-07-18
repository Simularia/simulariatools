% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotAvgTemp.R
\name{plotAvgTemp}
\alias{plotAvgTemp}
\title{Plot average temperature}
\usage{
plotAvgTemp(
  mydata,
  temp = "temp",
  avg.time = "1 month",
  ylabel = NULL,
  title = "",
  locale = NULL
)
}
\arguments{
\item{mydata}{dataframe with data to plot. date and time column must be
named as "date".}

\item{temp}{Name of the column representing temperature (default = "temp")}

\item{avg.time}{Defines the time period to average to.
Currently the only supported period is "1 month" (default).}

\item{ylabel}{The label along the y axis.
If missing a default label is plotted.}

\item{title}{Optional plot title}

\item{locale}{Locale to use for day and month names. Default is current
locale. Supported locales are listed in stringi::stri_locale_list().
All other labels are in English by default or in Italian if its locale is
specified.}
}
\value{
A plot with average, min and max temperature in a given
range of time.
}
\description{
\code{plotAvgTemp} builds a bar plot of time average temperature and two
line plots with maximum and minimum temperature.
}
\examples{
# Plot histogram with monthly averages together with maxima and minima
# curves
data("stMeteo")
plotAvgTemp(stMeteo)
plotAvgTemp(stMeteo, temp = "temperature",
            avg.time = "1 month", ylabel = "Temperatura [C]")

# Override default locale
plotAvgTemp(stMeteo, avg.time = "1 month", locale = "it_IT")

# Add title
plotAvgTemp(stMeteo, title = "Monthly temperature")

}
\seealso{
\code{\link[=plotStabilityClass]{plotStabilityClass()}}, \code{\link[=plotAvgRad]{plotAvgRad()}}
}
