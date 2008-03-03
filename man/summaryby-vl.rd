\name{summaryby}
\alias{summaryby}
\title{Summarise a data.frame by parts}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Summarise a data frame by unique value of x
}
\usage{summaryby(data, split, summary, ...)}
\arguments{
\item{data}{}
\item{split}{}
\item{summary}{}
\item{...}{}
}

\details{This function is used by \code{\link{stat_summary}} to break a
data.frame into pieces, summarise each piece, and join the pieces
back together, retaining original columns unaffected by the summary.}

\examples{}
\keyword{internal}
