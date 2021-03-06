% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pumpR.R
\name{Text.Highlight}
\alias{Text.Highlight}
\title{Draws Text to Screen}
\usage{
Text.Highlight(Q, H, text, highlight_color = "white",
  text_color = "red", TextSize = 0.75)
}
\arguments{
\item{Q}{Flow, GPM}

\item{H}{Head, FT}

\item{text}{Text to display}

\item{highlight_color}{optional}

\item{text_color}{optional}

\item{TextSize}{optional}
}
\value{
Plot
}
\description{
Draws Text to Screen
}
\examples{
Draw.Graph()
Text.Highlight(3000,50, "Test Point")
}
