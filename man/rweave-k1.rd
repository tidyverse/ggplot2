\name{rweave}
\alias{rweave}
\title{Row weave}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Weave together two (or more) matrices by row
}
\usage{rweave(...)}
\arguments{
\item{...}{matrices to weave together}
}

\details{Matrices must have smae dimensions}

\examples{a <- matrix(1:10 * 2, ncol = 2)
b <- matrix(1:10 * 3, ncol = 2)
c <- matrix(1:10 * 5, ncol = 2)}

