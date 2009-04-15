\name{limits}
\alias{limits}
\alias{limits.numeric}
\alias{limits.character}
\alias{limits.factor}
\alias{limits.Date}
\alias{limits.POSIXct}
\alias{limits.POSIXlt}
\title{Scale limits}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Generate correct scale type for specified limits
}
\usage{limits(lims, var)}
\arguments{
\item{lims}{vector of limits}
\item{var}{variable}
}

\details{}

\examples{limits(c(1, 5), "x")
limits(c(5, 1), "x")
limits(c("A", "b", "c"), "x")
limits(as.Date(c("2008-01-01", "2009-01-01")), "x")}

