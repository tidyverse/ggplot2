\name{geom_rug}
\alias{geom_rug}
\title{Marginal rug plots.}
\usage{
  geom_rug(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", sides = "bl", ...)
}
\arguments{
  \item{sides}{A string that controls which sides of the
  plot the rugs appear on.  It can be set to a string
  containing any of \code{"trbl"}, for top, right, bottom,
  and left.}

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
  Marginal rug plots.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "rug")}
}
\examples{
p <- ggplot(mtcars, aes(x=wt, y=mpg))
p + geom_point()
p + geom_point() + geom_rug()
p + geom_point() + geom_rug(sides="b")    # Rug on bottom only
p + geom_point() + geom_rug(sides="trbl") # All four sides
p + geom_point() + geom_rug(position='jitter')
}

