\name{geom_blank}
\alias{geom_blank}
\title{Blank, draws nothing.}
\usage{
  geom_blank(mapping = NULL, data = NULL,
    stat = "identity", position = "identity", ...)
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
  The blank geom draws nothing, but can be a useful way of
  ensuring common scales between different plots.
}
\examples{
qplot(length, rating, data=movies, geom="blank")
# Nothing to see here!
}

