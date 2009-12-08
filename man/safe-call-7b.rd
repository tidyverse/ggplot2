\name{safe.call}
\alias{safe.call}
\title{A "safe" version of do.call}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
\code{safe.call} works like \code{\link{do.call}} but it will only supply arguments that exist in the function specification.
}
\usage{safe.call(f, params, f.params = names(formals(f)), ignore.dots = TRUE)}
\arguments{
\item{f}{function to call}
\item{params}{parameter names of function}
\item{f.params}{}
\item{ignore.dots}{}
}

\details{If ... is present in the param list, all parameters will be passed through
unless \code{ignore.dots = TRUE}.  Positional arguments are not currently
supported.}


\keyword{internal}
