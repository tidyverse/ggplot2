\name{geom_area}
\alias{geom_area}
\title{Area plot.}
\usage{
  geom_area(mapping = NULL, data = NULL, stat = "identity",
    position = "stack", na.rm = FALSE, ...)
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
  An area plot is the continuous analog of a stacked bar
  chart (see \code{\link{geom_bar}}), and can be used to
  show how composition of the whole varies over the range
  of x.  Choosing the order in which different components
  is stacked is very important, as it becomes increasing
  hard to see the individual pattern as you move up the
  stack.
}
\details{
  An area plot is a special case of
  \code{\link{geom_ribbon}}, where the minimum of the range
  is fixed to 0, and the position adjustment defaults to
  position_stacked.
}
\examples{
# see geom_ribbon
}

