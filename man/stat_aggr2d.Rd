\name{stat_aggr2d}
\alias{stat_aggr2d}
\title{Apply function for rectangular bins.}
\usage{
  stat_aggr2d(mapping = NULL, data = NULL, geom = "rect",
    position = "identity", bins = 30, drop = TRUE,
    fun = mean, ...)
}
\description{
  Apply function for rectangular bins.
}
\examples{
d <- ggplot(diamonds, aes(carat, depth, z = price))
d + stat_aggr2d()

# Specifying function
d + stat_aggr2d(fun = function(x) sum(x^2))
d + stat_aggr2d(fun = var)
}
\seealso{
  \code{\link{stat_aggrhex}} for hexagonal aggregation.
  \code{\link{stat_bin2d}} for the binning options.
}

