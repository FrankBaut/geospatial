% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trunc.R
\name{trunc}
\alias{trunc}
\title{remove decimals from a value (significant numbers)}
\usage{
trunc(x, ..., prec = 0)
}
\arguments{
\item{x}{float value}

\item{prec}{number of removed decimals in the x value}
}
\value{

}
\description{
remove decimals from a value (significant numbers)
}
\examples{
x<-129.317313
trunc(x,prec=4)
}