\name{stabilityClass}
\alias{stabilityClass}
\title{Stability class.}
\usage{
stabilityClass(radg, tcc, ws, option = "impact")
}
\arguments{
  \item{radg}{The net radiation in W/m^2}

  \item{tcc}{The total cloud cover in a range from 1 to 8}

  \item{ws}{wind speed in m/s}
}
\description{
\code{stabilityClass} computes stability class.
}
\details{
It computes stability class according to IAES method based
on net radiation and wind. Net radiation and wind are used
by day; tcc and wind are used by night.
}

