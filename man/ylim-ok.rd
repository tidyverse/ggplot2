\name{ylim}
\alias{ylim}
\title{Set y limits}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Convenience function to set the limits of the y axis.
}
\usage{ylim(min=NA, max=NA)}
\arguments{
\item{min}{lower limit}
\item{max}{upper limit}
}

\details{Works by creating a new continuous scale, so will only work for
continuous variables.}

\examples{qplot(mpg, wt, data=mtcars) + ylim(15, 20)}
\keyword{hplot}
