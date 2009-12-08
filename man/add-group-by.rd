\name{add_group}
\alias{add_group}
\title{Add group}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Ensure that the data frame contains a grouping variable.
}
\usage{add_group(data)}
\arguments{
\item{data}{data.frame}
}
\value{data.frame with group variable}
\details{If the \code{group} variable is not present, then a new group
variable is generated from the interaction of all discrete (factor or
character) vectors excluding label.}


\keyword{internal}
