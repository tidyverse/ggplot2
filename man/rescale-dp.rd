\name{rescale}
\alias{rescale}
\title{Rescale numeric vector}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Rescale numeric vector to have specified minimum and maximum.
}
\usage{rescale(x, to=c(0,1), from=range(x, na.rm=TRUE), clip = TRUE)}
\arguments{
\item{x}{data to rescale}
\item{to}{range to scale to}
\item{from}{range to scale from, defaults to range of data}
\item{clip}{should values be clipped to specified range?}
}




\keyword{manip}
