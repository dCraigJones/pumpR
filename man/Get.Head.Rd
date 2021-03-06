% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pumpR.R
\name{Get.Head}
\alias{Get.Head}
\title{get Head at Flow}
\usage{
Get.Head(PumpCurve, Q)
}
\arguments{
\item{PumpCurve}{pump object from New.Pump()}

\item{Q}{Flow, GPM}
}
\value{
Head, in FT
}
\description{
get Head at Flow
}
\examples{
pump <- New.Pump(c(0, 100,50,80, 100,60,150,40,200,20))
Get.Head(pump,76)
}
