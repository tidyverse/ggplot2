\name{viewport_default}
\alias{viewport_default}
\title{Default viewports}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Set up named viewports that the other components use.
}
\usage{viewport_default(plot, guides, scales, coordinates)}
\arguments{
\item{plot}{plot object}
\item{guides}{guides grobs}
\item{scales}{scales grobs}
\item{coordinates}{}
}

\details{This function sets up a \\code{\\link[grid]{vpTree}} in which all of
the components of a plot will be placed.   This allows for a clean
separation between the generation of plot objects, and their placement, and
means none of the components have to know anything about the others.

This function is responsible for the overall layout of the plot, ie
where the panels, labels and axes go.  In future, I will add more
viewport layout functions so that you can have the same layout as, e.g.,
the trellis default.}

\examples{}
\keyword{hplot}
\keyword{internal}
