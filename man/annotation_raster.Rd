\name{annotation_raster}
\alias{annotation_raster}
\title{Annotation: High-performance rectangular tiling.}
\usage{
  annotation_raster(raster, xmin, xmax, ymin, ymax)
}
\arguments{
  \item{raster}{raster object to display}

  \item{xmin,xmax}{x location (in data coordinates) giving
  horizontal location of raster}

  \item{ymin,ymax}{y location (in data coordinates) giving
  vertical location of raster}
}
\description{
  This is a special version of \code{\link{geom_raster}}
  optimised for static anotations that are the same in
  every panel. These anotations will not affect scales
  (i.e. the x and y axes will not grow to cover the range
  of the raster, and the raster must already have it's own
  colours).
}
\details{
  Most useful for adding bitmap images.
}
\examples{
# Generate data
rainbow <- matrix(hcl(seq(0, 360, length = 50 * 50), 80, 70), nrow = 50)
qplot(mpg, wt, data = mtcars) +
  annotation_raster(rainbow, 15, 20, 3, 4)
# To fill up whole plot
qplot(mpg, wt, data = mtcars) +
  annotation_raster(rainbow, -Inf, Inf, -Inf, Inf) +
  geom_point()
}

