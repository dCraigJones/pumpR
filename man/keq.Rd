% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pumpR.R
\name{keq}
\alias{keq}
\title{Calculate the equivalent hydraulic gradient (k) for parallel pipes}
\usage{
keq(k1, k2)
}
\arguments{
\item{k1}{hydraulic gradient for a pipe segment}

\item{k2}{hydraulic gradient for a pipe segment}
}
\value{
the equivalent hydraulic gradient (k) for parallel pipes
}
\description{
Calculate the equivalent hydraulic gradient (k) for parallel pipes
}
\examples{
# Find the equiv k for 1,000 LF of twin 12-inch pipes at C=130
2000*keq(ke(12,130),ke(12,130))
}
