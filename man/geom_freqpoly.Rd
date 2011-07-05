\name{geom_freqpoly}
\alias{geom_freqpoly}
\title{Frequency polygon.}
\usage{
  geom_freqpoly(mapping = NULL, data = NULL, stat = "bin",
  position = "identity", ...)
}
\description{
  Frequency polygon.
}
\examples{
qplot(carat, data = diamonds, geom = "freqpoly")
qplot(carat, data = diamonds, geom = "freqpoly", binwidth = 0.1)
qplot(carat, data = diamonds, geom = "freqpoly", binwidth = 0.01)

qplot(price, data = diamonds, geom = "freqpoly", binwidth = 1000)
qplot(price, data = diamonds, geom = "freqpoly", binwidth = 1000,
colour = color)
qplot(price, ..density.., data = diamonds, geom = "freqpoly",
binwidth = 1000, colour = color)
}
\seealso{
  \code{\link{geom_histogram}}: histograms
}

