\name{resolution}
\alias{resolution}
\title{Resolution}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Compute the "resolution" of a data vector, ie. what is the smallest non-zero
}
\usage{resolution(x, zero = TRUE)}
\arguments{
\item{x}{numeric vector}
\item{zero}{should a zero value be automatically included in the computation of resolution}
}

\details{If there is only one unique value, then the resolution is defined to be one.}

\examples{resolution(1:10)
resolution((1:10) - 0.5)
resolution((1:10) - 0.5, FALSE)
resolution(c(1,2, 10, 20, 50))}
\keyword{hplot}
\keyword{internal}
