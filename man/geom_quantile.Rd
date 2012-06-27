\name{geom_quantile}
\alias{geom_quantile}
\title{Add quantile lines from a quantile regression.}
\usage{
  geom_quantile(mapping = NULL, data = NULL,
    stat = "quantile", position = "identity",
    lineend = "butt", linejoin = "round", linemitre = 1,
    na.rm = FALSE, ...)
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

  \item{na.rm}{If \code{FALSE} (the default), removes
  missing values with a warning.  If \code{TRUE} silently
  removes missing values.}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}

  \item{lineend}{Line end style (round, butt, square)}

  \item{linejoin}{Line join style (round, mitre, bevel)}

  \item{linemitre}{Line mitre limit (number greater than
  1)}
}
\description{
  This can be used as a continuous analogue of a
  geom_boxplot.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "quantile")}
}
\examples{
# See stat_quantile for examples
}
\seealso{
  See \code{\link{stat_quantile}} for examples.
}

