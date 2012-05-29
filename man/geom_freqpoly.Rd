\name{geom_freqpoly}
\alias{geom_freqpoly}
\title{Frequency polygon.}
\usage{
  geom_freqpoly(mapping = NULL, data = NULL, stat = "bin",
    position = "identity", ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  Frequency polygon.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "freqpoly")}
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

