\name{prettyplot}
\alias{prettyplot}
\title{Pretty plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Build a plot with all the usual bits and pieces.
}
\usage{prettyplot(plot, plotgrob, scales=plot$scales, cs=plot$coordinates)}
\arguments{
\item{plot}{plot}
\item{plotgrob}{plot grob}
\item{scales}{}
\item{cs}{}
}

\details{As well as the plotting area, a plot need:
\item main title
\item x and y axis labels
\item space for legends (currently on the right hand side)

These are stored as options in the plot object.

This function sets up the appropriate viewports and packs the
various components in.  The viewport is set up so that each component
will only take up the amount of space that it requires.}

\examples{}
\keyword{hplot}
