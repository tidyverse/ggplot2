\name{dhist}
\alias{dhist}
\title{dhist.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
An another algorithm for computing histogram breaks.  Produces irregular bins.
}
\usage{dhist(x, a=5*diff(quantile(x, c(0.25,0.75))), nbins=10, rx = range(x))}
\arguments{
\item{x}{}
\item{a}{}
\item{nbins}{}
\item{rx}{}
}

\details{}

\examples{}
\keyword{internal}
