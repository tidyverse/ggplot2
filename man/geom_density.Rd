\name{geom_density}
\alias{geom_density}
\title{Display a smooth density estimate.}
\usage{
  geom_density(mapping = NULL, data = NULL,
    stat = "density", position = "identity", na.rm = FALSE,
    ...)
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
  A smooth density estimate calculated by
  \code{\link{stat_density}}.
}
\examples{
# See stat_density for examples
}
\seealso{
  \code{\link{geom_histogram}} for the histogram
}

