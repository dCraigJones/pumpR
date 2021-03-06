% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GOF.R
\name{NSE}
\alias{NSE}
\title{Nash-Sutcliffe Coefficient}
\usage{
NSE(mod, obs)
}
\arguments{
\item{mod, }{vector of model data}

\item{obs, }{vector of observed data}
}
\value{
Nash-Sutcliffe Coefficient
}
\description{
\tabular{ccl}{
NSE Range \tab Calibration Rating \tab Model Application \cr
1.00 to 0.50 \tab Excellent \tab Planning, Prelim Design, Final Design \cr
0.49 to 0.40 \tab Very Good \tab Planning, Prelim Design, Final Design \cr
0.39 to 0.30 \tab Good \tab Planning, Prelim Design \cr
0.29 to 0.20 \tab Fair \tab Planning \cr
0.21 or less \tab Poor \tab Screening \cr
}
}
\examples{
NSE(1:10,1:10)
}
