\name{clist}
\alias{clist}
\title{Concatenate a named list for output}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Print a \code{list(a=1, b=2)} as \code{(a=1, b=2)}
}
\usage{clist(l)}
\arguments{
\item{l}{list to concatenate}
}



\examples{clist(list(a=1, b=2))
clist(par()[1:5])}
\keyword{internal}
