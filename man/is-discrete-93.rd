\name{is.discrete}
\alias{is.discrete}
\title{Determine if a vector is discrete}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
A discrete vector is a factor or a character vector
}
\usage{is.discrete(x)}
\arguments{
\item{x}{vector to test}
}

\details{}

\examples{is.discrete(1:10)
is.discrete(c("a", "b", "c"))
is.discrete(factor(c("a", "b", "c")))}
\keyword{internal}
