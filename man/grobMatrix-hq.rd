\name{grobMatrix}
\alias{grobMatrix}
\title{Build grob matrix}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Build a matrix of grobs given a vector of grobs and the desired dimensions of the matrix
}
\usage{grobMatrix(vec, nrow, ncol, as.table = FALSE)}
\arguments{
\item{vec}{vector of grobs}
\item{nrow}{number of rows}
\item{ncol}{number of columns}
\item{as.table}{should the matrix be arranged like a table or a plot}
}

\details{Any missing cells at the end will be filled in with zeroGrobs.}


\keyword{internal}
