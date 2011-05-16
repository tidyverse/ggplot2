\name{limits}
\alias{limits}
\title{Generate correct scale type for specified limits...}
\usage{limits(lims, var)}

\description{
  Generate correct scale type for specified limits
}
\keyword{internal}
\arguments{
  \item{limts}{vector of limits}
  \item{var}{name of variable}
}
\examples{limits(c(1, 5), "x")
limits(c(5, 1), "x")
limits(c("A", "b", "c"), "x")
limits(c("A", "b", "c"), "fill")
limits(as.Date(c("2008-01-01", "2009-01-01")), "x")}
