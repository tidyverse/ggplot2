\name{try_default}
\alias{try_default}
\alias{tryNULL}
\title{Try, with default in case of error}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
\code{try_default} wraps try so that it returns a default value in the case of error.
}
\usage{try_default(expr, default = NA)}
\arguments{
\item{expr}{expression to try}
\item{default}{default value in case of error}
}

\details{\code{tryNULL} provides a useful special case when dealing with lists.}
\seealso{\code{\link{tryapply}}}
\examples{}
\keyword{internal}
