\name{should_stop}
\alias{should_stop}
\title{Used in examples to illustrate when errors should occur.}
\usage{should_stop(expr)}

\description{
  Used in examples to illustrate when errors should occur.
}
\keyword{internal}
\arguments{
  \item{expr}{code to evaluate.}
}
\examples{should_stop(stop("Hi!"))
should_stop(should_stop("Hi!"))}

