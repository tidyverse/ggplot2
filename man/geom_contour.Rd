\name{geom_contour}
\alias{geom_contour}
\title{Display contours of a 3d surface in 2d.}
\usage{
  geom_contour(mapping = NULL, data = NULL,
    stat = "contour", position = "identity",
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
}
\description{
  Display contours of a 3d surface in 2d.
}
\examples{
# See stat_contour for examples
}
\seealso{
  \code{\link{geom_density2d}}: 2d density contours
}

