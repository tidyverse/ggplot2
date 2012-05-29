\name{geom_density2d}
\alias{geom_density2d}
\title{Contours from a 2d density estimate.}
\usage{
  geom_density2d(mapping = NULL, data = NULL,
    stat = "density2d", position = "identity",
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
  Perform a 2D kernel density estimatation using kde2d and
  display the results with contours.
}
\details{
  This can be useful for dealing with overplotting.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "density2d")}
}
\examples{
# See stat_density2d for examples
}
\seealso{
  \code{\link{geom_contour}} for contour drawing geom,
  \code{\link{stat_sum}} for another way of dealing with
  overplotting
}

