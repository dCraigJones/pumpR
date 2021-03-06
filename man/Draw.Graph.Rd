% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pumpR.R
\name{Draw.Graph}
\alias{Draw.Graph}
\title{Draws the framework for the pumpR style H-Q plot}
\usage{
Draw.Graph(Max.Q = 15000, Major.Q = 1500, Minor.Q = 250,
  Max.H = 250, Major.H = 50, Minor.H = 10, AWRMP = FALSE,
  MGD = FALSE, TDH = TRUE)
}
\arguments{
\item{Max.Q}{Maximum Flow (GPM)}

\item{Major.Q}{Major Flow Axis (GPM)}

\item{Minor.Q}{Minor Flow Axis (GPM)}

\item{Max.H}{Maximum Head (FT)}

\item{Major.H}{Major Head Axis (FT)}

\item{Minor.H}{Minor Head Axis (FT)}

\item{AWRMP}{Boolean, Add Planning Caveat to plot}

\item{MGD}{Boolean, Add addition flow axis for MGD}

\item{TDH}{Boolean, set y-axis label to FT TDH (TRUE) or FT (FALSE)}
}
\value{
None
}
\description{
Draws the framework for the pumpR style H-Q plot
}
\examples{
Draw.Graph(1000,200,50,250,50,10)
}
\seealso{
Draw.Pump
}
