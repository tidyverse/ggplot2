\name{chop}
\alias{chop}
\title{Chop}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Chop a continuous variable into a categorical variable.
}
\usage{chop(x, n = 10, width = NULL, method="quantiles", midpoint=0, digits=2, ...)}
\arguments{
\item{x}{continuous variable to chop into pieces}
\item{n}{number of bins to chop into}
\item{width}{method to use: quantiles (approximately equal numbers), cut (equal lengths) or pretty}
\item{method}{mid point for diverging factors}
\item{midpoint}{other arguments passed to format}
\item{digits}{}
\item{...}{}
}

\details{Chop provides a convenient interface for discretising a continuous variable.
It will break up a continuous variable into chunks with equal numbers of
points (\code{method = "quantiles"}) or equal ranges (
\code{method = "cut"}).  You can specify the number of bins, with \code{n},
or the "width" of each bin, with \code{width}}
\seealso{\code{\link[reshape]{round_any}}, \code{\link{chop.breaks}} to get breaks used}
\examples{}
\keyword{manip}
