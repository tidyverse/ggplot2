\name{cut_number}
\alias{cut_number}
\title{Discretise continuous variable, equal number of points.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Cut numeric vector into intervals containing equal number of points.
}
\usage{cut_number(x, n = NULL, ...)}
\arguments{
\item{x}{numeric vector}
\item{n}{number of intervals to create, OR}
\item{...}{length of each interval}
\item{}{other arguments passed on to \code{\link{cut}}}
}

\details{}
\seealso{\code{\link{cut_interval}}}
\examples{table(cut_number(runif(1000), n = 10))}

