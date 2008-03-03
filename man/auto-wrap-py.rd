\name{auto_wrap}
\alias{auto_wrap}
\title{Wrap summary function}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Creates a new function which will operate correctly with \code{\link{stat_summary}}.
}
\usage{auto_wrap(f)}
\arguments{
\item{f}{function to wrap}
}

\details{\code{\link{stat_summary}} assumes that summary functions take data.frames
as input, and return data frames as output.  This function will convert
a function that takes a vector of values to the correct format.}

\examples{sum_mean <- auto_wrap(mean)
sum_mean(data.frame(y = 1:10))}
\keyword{internal}
