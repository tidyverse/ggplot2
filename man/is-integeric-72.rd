\name{is.integeric}
\alias{is.integeric}
\title{Are integers?}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Determine if a vector contains only integers
}
\usage{is.integeric(x)}
\arguments{
\item{x}{vector to test}
}

\details{}

\examples{is.integeric(runif(100))
is.integeric(rpois(100, 10))
is.integeric(1:10)}
\keyword{internal}
