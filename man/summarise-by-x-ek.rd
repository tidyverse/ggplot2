\name{summarise_by_x}
\alias{summarise_by_x}
\title{Summarise a data.frame by parts}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Summarise a data frame by unique value of x
}
\usage{summarise_by_x(data, summary, ...)}
\arguments{
\item{data}{\code{\link{data.frame}} to summarise}
\item{summary}{vector to summarise by}
\item{...}{summary function (must take and return a data.frame)}
\item{}{other arguments passed on to summary function}
}

\details{This function is used by \code{\link{stat_summary}} to break a
data.frame into pieces, summarise each piece, and join the pieces
back together, retaining original columns unaffected by the summary.}


\keyword{internal}
