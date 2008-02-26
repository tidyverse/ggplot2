\name{remove_missing}
\alias{remove_missing}
\title{Convenience function to remove missing values from a data.frame}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Remove all non-complete rows, with a warning if \code{na.rm = FALSE}.
}
\usage{remove_missing(df, na.rm=FALSE, vars = names(df), name="")}
\arguments{
\item{df}{data.frame}
\item{na.rm}{suppress warning that rows are being removed?}
\item{vars}{optional function name to make warning message more informative}
\item{name}{}
}

\details{ggplot is somewhat more accomodating of missing values than R generally.
For those stats which require complete data, missing values will be
automatically removed with a warning.  If \code{na.rm = TRUE} is supplied
to the statistic, the warning will be suppressed.}

\examples{a <- remove_missing(movies)
a <- remove_missing(movies, na.rm = TRUE)
qplot(mpaa, budget, data=movies, geom="boxplot")}
\keyword{internal}
