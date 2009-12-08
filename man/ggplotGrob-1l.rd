\name{ggplotGrob}
\alias{ggplotGrob}
\title{Pretty plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Build a plot with all the usual bits and pieces.
}
\usage{ggplotGrob(plot, drop = plot$options$drop, keep = plot$options$keep, ...)}
\arguments{
\item{plot}{plot}
\item{drop}{plot grob}
\item{keep}{}
\item{...}{}
}

\details{As well as the plotting area, a plot needs:

\itemize{
\item main title
\item x and y axis labels
\item space for legends (currently on the right hand side)
}

These are stored as options in the plot object.

This function sets up the appropriate viewports and packs the
various components in.  The viewport is set up so that each component
will only take up the amount of space that it requires.}


\keyword{hplot}
