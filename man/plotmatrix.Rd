\name{plotmatrix}
\alias{plotmatrix}
\title{Code to create a scatterplot matrix (experimental)...}
\usage{plotmatrix(data, mapping = aes(), colour = "black")}

\description{
  Code to create a scatterplot matrix (experimental)
}
\keyword{hplot}
\arguments{
  \item{data}{data frame}
  \item{mapping}{any additional aesthetic mappings (do not use x and y)}
  \item{colour}{default point colour}
}
\examples{plotmatrix(mtcars[, 1:3])
plotmatrix(mtcars[, 1:3]) + geom_smooth(method="lm")}

