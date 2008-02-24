\name{is.integeric}
\alias{is.integeric}
\title{Determine if a vector contains only integers}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{

}
\usage{is.integeric(x)}
\arguments{
\item{x}{vector to test}
\item{}{vector to test}
\item{}{data.frame}
}
\value{data.frame with group variable}
\details{@arguments vector to test
@keyword internal
X is.integeric(runif(100))
X is.integeric(rpois(100, 10))
X is.integeric(1:10)
Determine if a vector is discrete
A discrete vector is a factor or a character vector

@arguments vector to test
@keywords internal
X is.discrete(1:10)
X is.discrete(c("a", "b", "c"))
X is.discrete(factor(c("a", "b", "c")))
Add group
Ensure that the data frame contains a grouping variable.

If the \code{group} variable is not present, then a new group
variable is generated from the interaction of all discrete (factor or
character) vectors excluding label.}

\examples{is.integeric(runif(100))
is.integeric(rpois(100, 10))
is.integeric(1:10)
is.discrete(1:10)
is.discrete(c("a", "b", "c"))
is.discrete(factor(c("a", "b", "c")))}
\keyword{internal}
\keyword{internal}
