\name{geom_density2d}
\alias{geom_density2d}
\title{Contours from a 2d density estimate.}
\usage{
  geom_density2d(mapping = NULL, data = NULL, stat =
  "density2d", position = "identity", lineend = "butt",
  linejoin = "round", linemitre = 1, na.rm = FALSE, ...)
}
\description{
  Perform a 2D kernel density estimatation using kde2d and
  display the results with contours.
}
\details{
  This can be useful for dealing with overplotting.
}
\examples{
# See stat_density2d for examples
}
\seealso{
  \code{\link{geom_contour}} for contour drawing geom,
  \code{\link{stat_sum}} for another way of dealing with
  overplotting
}

