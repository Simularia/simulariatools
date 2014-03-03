\name{removeOutliers}
\alias{removeOutliers}
\title{Remove data outliers}
\usage{
removeOutliers(x, k)
}
\arguments{
  \item{x}{vector of data to be cleaned}

  \item{k}{factor to applied to the interquartile range}
}
\description{
Outliers are removed from a data set. The interquartile
range is computed. Data larger or smaller than k * IQR are
removed.
}
\examples{
# Example
mydata$ws <- removeOutliers(mydata$ws, 5)
}

