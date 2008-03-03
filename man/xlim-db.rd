\name{xlim}
\alias{xlim}
\title{Set x limits}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Convenience function to set the limits of the x axis.
}
\usage{xlim(min=NA, max=NA)}
\arguments{
\item{min}{lower limit}
\item{max}{upper limit}
}

\details{Works by creating a new continuous scale, so will only work for
continuous variables.}

\examples{qplot(mpg, wt, data=mtcars) + xlim(15, 20)}
\keyword{hplot}
