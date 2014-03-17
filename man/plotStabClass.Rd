\name{plotStabClass}
\alias{plotStabClass}
\title{Plot stability class}
\usage{
plotStabClass(mydata, type = "season")
}
\arguments{
  \item{mydata}{A data frame containing cs field}

  \item{type}{type determines how the data are split and
  then plotted. The default is "season". Any other string
  will give an hourly plot.}
}
\description{
Plot histogram of stability class on season or hour base.
}
\details{
Numerical values of stability classes are mapped as: 1 = A,
2 = B, ..., 6 = F
}
\examples{
plotStabClass(t, type="season")
plotStabClass(t, type="hour")
}

