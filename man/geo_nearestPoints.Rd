% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_nearestPoints.R
\name{geo_nearestPoints}
\alias{geo_nearestPoints}
\title{Get the nearby points given a radius}
\usage{
geo_nearestPoints(data, sepDistance)
}
\arguments{
\item{data}{data frame with lng and lat columns}

\item{sepDistance}{interval of geopositions in meters}
}
\value{
data frame
}
\description{
Get the nearby points given a radius
}
\examples{
data<-geospatial::georadius(-96.1312,18.1232,radius = 100,1000)
geospatial::geo_nearestPoints(data,sepDistance = 120)
}
