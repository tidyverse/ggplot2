\name{stat_spoke}
\alias{stat_spoke}
\title{Convert angle and radius to xend and yend.}
\usage{
  stat_spoke(mapping = NULL, data = NULL, geom = "segment",
    position = "identity", ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{geom}{The geometric object to use display the data}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\value{
  a data.frame with additional columns \item{xend}{x
  position of end of line segment} \item{yend}{x position
  of end of line segment}
}
\description{
  Convert angle and radius to xend and yend.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat",
  "spoke")}
}
\examples{
df <- expand.grid(x = 1:10, y=1:10)
df$angle <- runif(100, 0, 2*pi)
df$speed <- runif(100, 0, 0.5)

qplot(x, y, data=df) + stat_spoke(aes(angle=angle), radius = 0.5)
last_plot() + scale_y_reverse()

qplot(x, y, data=df) + stat_spoke(aes(angle=angle, radius=speed))
}

