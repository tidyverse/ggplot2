\name{cut_interval}
\alias{cut_interval}
\title{Discretise continuous variable, equal interval length.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Cut numeric vector into intervals of equal length.
}
\usage{cut_interval(x, n = NULL, length = NULL, ...)}
\arguments{
\item{x}{numeric vector}
\item{n}{number of intervals to create, OR}
\item{length}{length of each interval}
\item{...}{other arguments passed on to \code{\link{cut}}}
}

\details{@arguments numeric vector
@arguments number of intervals to create, OR
@arguments length of each interval
@arguments other arguments passed on to \code{\link{cut}}
@keyword manip
@seealso \code{\link{cut_number}}}
\seealso{\code{\link{cut_number}}}
\examples{table(cut_interval(1:100, n = 10))
table(cut_interval(1:100, n = 11))
table(cut_interval(1:100, length = 10))}
\keyword{manip}
