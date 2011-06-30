\name{cut_number}
\alias{cut_number}
\title{Cut numeric vector into intervals containing equal number of points.}
\usage{cut_number(x, n = NULL, ...)}

\description{
  Cut numeric vector into intervals containing equal number
  of points.
}
\seealso{\code{\link{cut_interval}}}
\arguments{
  \item{x}{numeric vector}
  \item{n}{number of intervals to create}
  \item{...}{other arguments passed on to \code{\link{cut}}}
}
\examples{table(cut_number(runif(1000), n = 10))}

