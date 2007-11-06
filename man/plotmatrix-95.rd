\name{plotmatrix}
\alias{plotmatrix}
\title{Code to create a scatterplot matrix (experimental)}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Crude experimental scatterplot matrix
}
\usage{plotmatrix(data, mapping=aes())}
\arguments{
\item{data}{data frame}
\item{mapping}{any additional aesthetic mappings (do not use x and y)}
}

\details{}

\examples{plotmatrix(mtcars)
plotmatrix(mtcars, aes(colour=factor(cyl)))
plotmatrix(mtcars) + geom_smooth(method="lm")
plotmatrix(mtcars, aes(colour=factor(cyl))) }
\keyword{hplot}
