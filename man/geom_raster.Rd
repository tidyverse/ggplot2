\name{geom_raster}
\alias{geom_raster}
\title{High-performance rectangular tiling.}
\usage{
  geom_raster(mapping = NULL, data = NULL,
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
  This is a special case of \code{\link{geom_tile}} where
  all tiles are the same size.  It is implemented highly
  efficiently using the internal \code{rasterGrob}
  function.
}
\examples{
# Generate data
pp <- function (n,r=4) {
 x <- seq(-r*pi, r*pi, len=n)
 df <- expand.grid(x=x, y=x)
 df$r <- sqrt(df$x^2 + df$y^2)
 df$z <- cos(df$r^2)*exp(-df$r/6)
 df
}
qplot(x, y, data = pp(20), fill = z, geom = "raster")

# For the special cases where it is applicable, geom_raster is much
# faster than geom_tile:
pp200 <- pp(200)
base <- ggplot(pp200, aes(x, y, fill = z))
benchplot(base + geom_raster())
benchplot(base + geom_tile())
}

