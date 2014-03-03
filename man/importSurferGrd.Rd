\name{importSurferGrd}
\alias{importSurferGrd}
\title{Import Grid file}
\usage{
importSurferGrd(fname, k = 1000)
}
\arguments{
  \item{fname}{Surfer grd file to be imported}

  \item{k}{Factor to apply to x and y coordinates}
}
\description{
It imports Surfer like grid file
}
\details{
Surfer grd file is imported and an array of x, y, z columns
is returned X and y coordinats can be converted from km to
m (default k=1000) and viceversa.
}

