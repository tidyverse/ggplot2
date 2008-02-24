\name{tryNULL}
\alias{tryNULL}
\alias{tryNULL}
\title{Try, with default in case of error}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
\code{try_default} wraps try so that it returns a default value in the case of error
}
\usage{tryNULL(expr)}
\arguments{
\item{expr}{expression to try}
\item{}{default value in case of error}
}

\details{\code{tryNULL} provides a useful special case when dealing with lists.

@arguments expression to try
@arguments default value in case of error
@keyword internal
@seealso \code{\link{tryapply}}
@alias tryNULL
Apply with built in try
Uses compact, lapply and tryNULL}
\seealso{\code{\link{tryapply}}}
\examples{}
\keyword{internal}
\keyword{internal}
